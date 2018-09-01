{ lib, config, pkgs, ... }:

let
  cfg = config.services.mystore;
  mystore = pkgs.callPackage ./shell.nix {};
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
        description = "Port to listen requests on.";
      };
      folder = mkOption {
        type = types.path;
        description = "Folder to serve.";
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.services.mystore = {
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      description = "Start the mystore service.";
      serviceConfig = {
        ExecStart = ''
          ${mystore}/bin/mystore ${(toString cfg.port)} ${cfg.folder}
        '';
      };
    };
  };
}
