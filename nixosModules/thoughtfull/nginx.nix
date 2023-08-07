{ config, lib, ... }: let
  cfg = config.services.nginx;
in {
  config = lib.mkIf cfg.enable {
    networking = {
      firewall.allowedTCPPorts = [
        80
        443
      ];
    };
    services = {
      nginx = {
        appendHttpConfig = ''
          # blocked IPs:
          # deny 73.177.192.154;
          allow all;

          # rate limiting: https://www.nginx.com/blog/rate-limiting-nginx/
          # allow list IPs subject to relaxed rate limit
          # map allow list IPs to 0, others to 1
          geo $limit {
            default 1;
            # allow list IPs:
            # 73.177.192.154/32 0;
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
        recommendedProxySettings = true;
      };
    };
  };
}
