{ config, lib, ... }: let
  thoughtfull = config.thoughtfull.nginx;
in {
  options.thoughtfull.nginx = {
    allowedIPs = lib.mkOption {
      default = [];
      description = lib.mdDoc ''
        List of IP addresses that should be allowed a higher request rate. They can also be CIDR
        notation or special values, see https://nginx.org/en/docs/http/ngx_http_geo_module.html.
      '';
      type = lib.types.listOf lib.types.str;
    };
    blockedIPs = lib.mkOption {
      default = [];
      description = lib.mdDoc ''
        List of IP addresses that should be blocked from any request. They can also be CIDR notation
        or special values, see https://nginx.org/en/docs/http/ngx_http_access_module.html.
      '';
      type = lib.types.listOf lib.types.str;
    };
    proxies = lib.mkOption {
      default = {};
      description = lib.mdDoc ''
        Proxies for nginx.
      '';
      type = lib.types.attrsOf (lib.types.submodule ({ name, ... } : {
        options = {
          backend = lib.mkOption {
            default = null;
            description = lib.mdDoc ''
                URL of the backend service for this proxy.
              '';
            type = lib.types.str;
          };
          forceSSL = lib.mkOption {
            default = true;
            description = lib.mdDoc ''
                Whether to force SSL connections to this proxy.
              '';
            type = lib.types.bool;
          };
          name = lib.mkOption {
            default = name;
            description = lib.mdDoc ''
                Name of the virtual host for the proxy.
              '';
            type = lib.types.str;
          };
        };
      }));
    };
  };
  config = {
    networking = {
      firewall.allowedTCPPorts = lib.mkIf config.services.nginx.enable [
        80
        443
      ];
    };
    security = lib.mkMerge (lib.mapAttrsToList
      (name : {name, backend, forceSSL}:
        {
          acme.certs = {
            ${name} = {};
          };
        })
      thoughtfull.proxies);
    services = {
      nginx = let
        blockStr = lib.concatMapStringsSep "\n" (ip: "deny ${ip};") thoughtfull.blockedIPs;
        allowStr = lib.concatMapStringsSep "\n" (ip: "${ip} 0;") thoughtfull.allowedIPs;
      in {
        appendHttpConfig = ''
          # blocked IPs:
          ${blockStr}
          allow all;

          # rate limiting: https://www.nginx.com/blog/rate-limiting-nginx/
          # allow list IPs subject to relaxed rate limit
          # map allow list IPs to 0, others to 1
          geo $limit {
            default 1;
            # allow list IPs:
            ${allowStr}
          }

          # $limit_key is "" for allow list IPs, remote address for others
          map $limit $limit_key {
            0 "";
            1 $binary_remote_addr;
          }

          # allow list will match only the relaxed rate; others will match both,
          # and the more restrictive limit will apply
          limit_req_zone $limit_key          zone=limit_strict:1m  rate=20r/s;
          limit_req_zone $binary_remote_addr zone=limit_relaxed:1m rate=1000r/s;

          # Most websites have no more than 12 resources per load
          limit_req zone=limit_strict  burst=40   delay=20;
          limit_req zone=limit_relaxed burst=2000 nodelay;
          limit_req_status 429;
        '';
        enable = lib.mkDefault ((builtins.length (builtins.attrNames thoughtfull.proxies)) > 0);
        recommendedProxySettings = true;
        virtualHosts = lib.mkMerge (lib.mapAttrsToList (name : {name, backend, forceSSL}:
          {
            ${name} = {
              enableACME = lib.mkDefault true;
              forceSSL = lib.mkDefault forceSSL;
              locations."/".proxyPass = lib.mkDefault backend;
            };
          })
          thoughtfull.proxies);
      };
    };
  };
}
