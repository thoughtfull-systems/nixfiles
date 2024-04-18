{ config, lib, pkgs, ... }: let
  cfg = config.thoughtfull.desktop;
in {
  config = lib.mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        b612
        source-code-pro
      ];
    };
    fonts.fontconfig.enable = lib.mkForce true;
    programs = lib.mkDefault {
      firefox = {
        enable = true;
        profiles.default = {
          id = 0;
          settings = {
            # Search for text when you start typing.
            #
            # I like this, but it interacts badly with Google Calendar keyboard
            # shortcuts.  I can still search quickly with '/'.
            "accessibility.typeaheadfind" = false;
            # Supress warning when opening about:config
            "browser.aboutConfig.showWarning" = false;
            # Supress Firefox Privacy Notice on first run
            "toolkit.telemetry.reportingpolicy.firstRun" = false;

            ### General
            ## Startup
            # Open previous windows and tabs
            "browser.startup.page" = 3;
            ## Tabs
            # Confirm before closing multiple tabs
            "browser.tabs.warnOnClose" = false;
            # Confirm before quitting with Ctrl+Q
            "browser.warnOnQuitShortcut" = false;

            ### Language and Appearance
            ## Website appearance
            # System theme for dark/light mode
            "layout.css.prefers-color-scheme.content-override" = 2;
            ## Fonts
            # Proportional Serif
            "font.name.serif.x-western" = "B612";
            "font.minimum-size.x-western" = 16;
            "font.size.variable.x-western" = 16;
            # Proportianal Sans-serif
            "font.name.sans-serif.x-western" = "B612";
            # Monospace
            "font.name.monospace.x-western" = "Source Code Pro";
            "font.size.monospace.x-western" = 16;

            ### Browsing
            # Recommend extensions as you browse
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
            # Recommend features as you browse
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;

            ### Home
            ## New Windows and Tabs
            # Homepage and new windows
            "browser.startup.homepage" = "about:blank";
            # New tabs
            "browser.newtabpage.enabled" = false;
            ## Firefox Home Content
            # Web Search
            "browser.newtabpage.activity-stream.showSearch" = false;
            # Shortucts
            "browser.newtabpage.activity-stream.feeds.topsites" = false;
            # Sponsored Shortcuts
            "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
            # Recent activity
            "browser.newtabpage.activity-stream.feeds.section.highlights" = false;
            # Visited Pages
            "browser.newtabpage.activity-stream.section.highlights.includeVisited" = false;
            # Bookmarks
            "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = false;
            # Most Recent Download
            "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = false;
            # Pages Saved to Pocket
            "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;

            ### Search
            ## Search Suggestions
            # Provide search suggestions
            "browser.search.suggest.enabled" = false;
            # Show search suggestions in address bar results
            "browser.urlbar.suggest.searches" = false;
            # Show search suggestions ahead of browsing history in address bar results
            "browser.urlbar.showSearchSuggestionsFirst" = false;
            # Show search suggestions in Private Windows
            "browser.search.suggest.enabled.private" = false;

            ### Browser Privacy
            ## Enhanced Tracking Protection
            "browser.contentblocking.category" = "strict";
            "network.cookie.cookieBehavior" = 5;
            "network.http.referer.disallowCrossSiteRelaxingDefault.top_navigation" = true;
            "privacy.annotate_channels.strict_list.enabled" = true;
            "privacy.partition.network_state.ocsp_cache" = true;
            "privacy.query_stripping.enabled" = true;
            "privacy.trackingprotection.enabled" = true;
            "privacy.trackingprotection.socialtracking.enabled" = true;
            # Send websites a "Do Not Track" signal that you don't want to be tracked
            "privacy.donottrackheader.enabled" = true;
            ## Cookies and Site Data
            # Delete cookies and site data when Firefox is closed
            "network.cookie.lifetimePolicy" = 2;
            ## Logins and Passwords
            # Ask to save logins and passwords for websites
            "signon.rememberSignons" = false;
            # Autofill logins and passwords
            "signon.autofillForms" = false;
            # Suggest and generate strong passwords
            "signon.generation.enabled" = false;
            # Show alerts about passwords for breached websites
            "signon.management.page.breach-alerts.enabled" = false;
            ## History
            # Firefox will use custom settings for history
            "privacy.history.custom" = true;
            # Must preserve history to restore tabs on startup
            "privacy.sanitize.pending" = "[{\"id\":\"newtab-container\",\"itemsToClear\":[],\"options\":{}}]";
            "privacy.sanitize.sanitizeOnShutdown" = false;

            ### Permissions
            # Location
            "permissions.default.geo" = 2;
            # Camera
            "permissions.default.camera" = 2;
            # Microphone
            "permissions.default.microphone" = 2;
            # Notifications
            "permissions.default.desktop-notification" = 2;
            # Autoplay
            "media.autoplay.default" = 5;
            # Virtual Reality
            "permissions.default.xr" = 2;

            ### Firefox Data Collection and Use
            # Allow Firefox to send technical and interaction data to Mozilla
            "datareporting.healthreport.uploadEnabled" = false;
            # Allow Firefox to make personalized extension recommendations
            "browser.discovery.enabled" = false;
            # Allow Firefox to install and run studies
            "app.shield.optoutstudies.enabled" = false;
            # Allow Firefox to send backlogged crash reports on your behalf
            "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;

            ### Security
            ## Deceptive Content and Dangerous Software Protection
            # Block dangerous and deceptive content
            "browser.safebrowsing.malware.enabled" = false;
            "browser.safebrowsing.phishing.enabled" = false;
            # Block dangerous downloads
            "browser.safebrowsing.downloads.enabled" = false;
            # Warn you about unwanted and uncommon software
            "browser.safebrowsing.downloads.remote.block_potentially_unwanted" = false;
            "browser.safebrowsing.downloads.remote.block_uncommon" = false;
            "urlclassifier.malwareTable" = "goog-malware-proto,moztest-harmful-simple,moztest-malware-simple";
            ## HTTPS-Only mode
            # Enable HTTPS-Only Mode in all windows
            "dom.security.https_only_mode" = true;
            "dom.security.https_only_mode_ever_enabled" = true;
            ## don't show data privacy warning
            "datareporting.policy.dataSubmissionPolicyAcceptedVersion" = 2;
            "datareporting.policy.dataSubmissionPolicyNotifiedTime" = "1656951310242";
          };
        };
      };
    };
  };
}
