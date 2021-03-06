{ lib, config, pkgs, ... }:

let
  cfg = config.services.mystore;
  mystore = pkgs.callPackage ./default.nix {};
in

with lib;

{
  options = {
    services.mystore = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable the mystore service.";
      };
      port = mkOption {
        type = types.ints.u16;
        default = 8081;
        description = "Port to serve requests on.";
      };
      folder = mkOption {
        type = types.path;
        description = "Folder to serve.";
      };
      user = mkOption {
        type = types.string;
        description = ''
          User to run service with. Avoid root; prefer user
          with isolated rights on folder to serve.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.services.mystore = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      description = "mystore service";
      serviceConfig = {
        ExecStart = ''
          ${mystore}/bin/mystore \
            --port ${(toString cfg.port)} \
            --folder ${cfg.folder}
        '';
        User = "${cfg.user}";
      };
    };
  };
}
