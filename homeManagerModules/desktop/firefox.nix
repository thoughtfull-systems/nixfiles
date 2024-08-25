{ config, lib, pkgs, ... }: let
  desktop = config.thoughtfull.desktop.enable;
in {
  config = lib.mkIf desktop {
    home.packages = with pkgs; [
      b612
      source-code-pro
    ];
    fonts.fontconfig.enable = lib.mkForce true;
    programs = {
      firefox = {
        enable = lib.mkDefault true;
        profiles.default = {
          id = 0;
          settings = {
            # Search for text when you start typing.
            #
            # I like this, but it interacts badly with Google Calendar keyboard
            # shortcuts.  I can still search quickly with '/'.
            "accessibility.typeaheadfind" = lib.mkDefault false;
            # Supress warning when opening about:config
            "browser.aboutConfig.showWarning" = lib.mkDefault false;
            # Supress Firefox Privacy Notice on first run
            "toolkit.telemetry.reportingpolicy.firstRun" = lib.mkDefault false;

            ### General
            ## Startup
            # Open previous windows and tabs
            "browser.startup.page" = lib.mkDefault 3;
            ## Tabs
            # Confirm before closing multiple tabs
            "browser.tabs.warnOnClose" = lib.mkDefault false;
            # Confirm before quitting with Ctrl+Q
            "browser.warnOnQuitShortcut" = lib.mkDefault false;

            ### Language and Appearance
            ## Website appearance
            # System theme for dark/light mode
            "layout.css.prefers-color-scheme.content-override" = lib.mkDefault 2;
            ## Fonts
            # Proportional Serif
            "font.name.serif.x-western" = lib.mkDefault "B612";
            "font.minimum-size.x-western" = lib.mkDefault 16;
            "font.size.variable.x-western" = lib.mkDefault 16;
            # Proportianal Sans-serif
            "font.name.sans-serif.x-western" = lib.mkDefault "B612";
            # Monospace
            "font.name.monospace.x-western" = lib.mkDefault "Source Code Pro";
            "font.size.monospace.x-western" = lib.mkDefault 16;

            ### Browsing
            # Recommend extensions as you browse
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" =
              lib.mkDefault false;
            # Recommend features as you browse
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" =
              lib.mkDefault false;

            ### Home
            ## New Windows and Tabs
            # Homepage and new windows
            "browser.startup.homepage" = lib.mkDefault "about:blank";
            # New tabs
            "browser.newtabpage.enabled" = lib.mkDefault false;
            ## Firefox Home Content
            # Web Search
            "browser.newtabpage.activity-stream.showSearch" = lib.mkDefault false;
            # Shortucts
            "browser.newtabpage.activity-stream.feeds.topsites" = lib.mkDefault false;
            # Sponsored Shortcuts
            "browser.newtabpage.activity-stream.showSponsoredTopSites" = lib.mkDefault false;
            # Recent activity
            "browser.newtabpage.activity-stream.feeds.section.highlights" = lib.mkDefault false;
            # Visited Pages
            "browser.newtabpage.activity-stream.section.highlights.includeVisited" =
              lib.mkDefault false;
            # Bookmarks
            "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" =
              lib.mkDefault false;
            # Most Recent Download
            "browser.newtabpage.activity-stream.section.highlights.includeDownloads" =
              lib.mkDefault false;
            # Pages Saved to Pocket
            "browser.newtabpage.activity-stream.section.highlights.includePocket" =
              lib.mkDefault false;

            ### Search
            ## Search Suggestions
            # Provide search suggestions
            "browser.search.suggest.enabled" = lib.mkDefault false;
            # Show search suggestions in address bar results
            "browser.urlbar.suggest.searches" = lib.mkDefault false;
            # Show search suggestions ahead of browsing history in address bar results
            "browser.urlbar.showSearchSuggestionsFirst" = lib.mkDefault false;
            # Show search suggestions in Private Windows
            "browser.search.suggest.enabled.private" = lib.mkDefault false;

            ### Browser Privacy
            ## Enhanced Tracking Protection
            "browser.contentblocking.category" = lib.mkDefault "strict";
            "network.cookie.cookieBehavior" = lib.mkDefault 5;
            "network.http.referer.disallowCrossSiteRelaxingDefault.top_navigation" =
              lib.mkDefault true;
            "privacy.annotate_channels.strict_list.enabled" = lib.mkDefault true;
            "privacy.partition.network_state.ocsp_cache" = lib.mkDefault true;
            "privacy.query_stripping.enabled" = lib.mkDefault true;
            "privacy.trackingprotection.enabled" = lib.mkDefault true;
            "privacy.trackingprotection.socialtracking.enabled" = lib.mkDefault true;
            # Send websites a "Do Not Track" signal that you don't want to be tracked
            "privacy.donottrackheader.enabled" = lib.mkDefault true;
            ## Cookies and Site Data
            # Delete cookies and site data when Firefox is closed
            "network.cookie.lifetimePolicy" = lib.mkDefault 2;
            ## Downloads
            # Store downloads in tmp dir (wiped on boot)
            "browser.download.dir" = lib.mkDefault "/tmp";
            ## Logins and Passwords
            # Ask to save logins and passwords for websites
            "signon.rememberSignons" = lib.mkDefault false;
            # Autofill logins and passwords
            "signon.autofillForms" = lib.mkDefault false;
            # Suggest and generate strong passwords
            "signon.generation.enabled" = lib.mkDefault false;
            # Show alerts about passwords for breached websites
            "signon.management.page.breach-alerts.enabled" = lib.mkDefault false;
            ## History
            # Firefox will use custom settings for history
            "privacy.history.custom" = lib.mkDefault true;
            # Must preserve history to restore tabs on startup
            "privacy.sanitize.pending" =
              lib.mkDefault "[{\"id\":\"newtab-container\",\"itemsToClear\":[],\"options\":{}}]";
            "privacy.sanitize.sanitizeOnShutdown" = lib.mkDefault false;

            ### Permissions
            # Location
            "permissions.default.geo" = lib.mkDefault 2;
            # Camera (ask)
            "permissions.default.camera" = lib.mkDefault 0;
            # Microphone (ask)
            "permissions.default.microphone" = lib.mkDefault 0;
            # Notifications
            "permissions.default.desktop-notification" = lib.mkDefault 2;
            # Autoplay
            "media.autoplay.default" = lib.mkDefault 5;
            # Virtual Reality
            "permissions.default.xr" = lib.mkDefault 2;
            # Disallow disabling pasting into fields
            "dom.event.clipboardevents.enable" = lib.mkDefault false;

            ### Firefox Data Collection and Use
            # Allow Firefox to send technical and interaction data to Mozilla
            "datareporting.healthreport.uploadEnabled" = lib.mkDefault false;
            # Allow Firefox to make personalized extension recommendations
            "browser.discovery.enabled" = lib.mkDefault false;
            # Allow Firefox to install and run studies
            "app.shield.optoutstudies.enabled" = lib.mkDefault false;
            # Allow Firefox to send backlogged crash reports on your behalf
            "browser.crashReports.unsubmittedCheck.autoSubmit2" = lib.mkDefault false;

            ### Security
            ## Deceptive Content and Dangerous Software Protection
            # Block dangerous and deceptive content
            "browser.safebrowsing.malware.enabled" = lib.mkDefault false;
            "browser.safebrowsing.phishing.enabled" = lib.mkDefault false;
            # Block dangerous downloads
            "browser.safebrowsing.downloads.enabled" = lib.mkDefault false;
            # Warn you about unwanted and uncommon software
            "browser.safebrowsing.downloads.remote.block_potentially_unwanted" =
              lib.mkDefault false;
            "browser.safebrowsing.downloads.remote.block_uncommon" = lib.mkDefault false;
            "urlclassifier.malwareTable" =
              lib.mkDefault "goog-malware-proto,moztest-harmful-simple,moztest-malware-simple";
            ## HTTPS-Only mode
            # Enable HTTPS-Only Mode in all windows
            "dom.security.https_only_mode" = lib.mkDefault true;
            "dom.security.https_only_mode_ever_enabled" = lib.mkDefault true;
            ## don't show data privacy warning
            "datareporting.policy.dataSubmissionPolicyAcceptedVersion" = lib.mkDefault 2;
            "datareporting.policy.dataSubmissionPolicyNotifiedTime" = lib.mkDefault "1656951310242";
          };
        };
      };
    };
  };
}
