{ pkgs, config, ... }:

let
  service = pkgs.python3Packages.callPackage ./package.nix {};

  webhookUrl = "https://gitlab.haskell.org/ghc/ghc/alerts/notify/grafana/c723e1f68627a214.json";
  authToken  = "7615690ee936a99f5cd66c99e73838c0";

  alert-forward-server = {
    systemd.services.alert-forward-server = {
      description = "Alert forwarding server";
      script = ''
        ${service}/bin/alert-forward-server \
          ${webhookUrl} \
          ${authToken} \
          8050
      '';
      wantedBy = [ "multi-user.target" ];
    };
  };

in {
  imports = [ alert-forward-server ];
}

