{config, pkgs, lib, ...}:
let cfg = config.programs.zoom;
in with lib; {
  options.programs.zoom = {
    enable = mkEnableOption "Zoom";
    homedir = mkOption {};
  };
  config = mkIf cfg.enable {
    home.packages = [ pkgs.zoom-us ];

    home.activation."defaultZoom" = lib.hm.dag.entryAfter ["writeBoundary"] ''
    run --silence ${pkgs.swiftdefaultapps}/bin/swda setHandler --URL zoomus --app "${pkgs.zoom-us}/Applications/zoom.us.app"
    run --silence ${pkgs.swiftdefaultapps}/bin/swda setHandler --URL zoommtg --app "${pkgs.zoom-us}/Applications/zoom.us.app"
    run --silence ${pkgs.swiftdefaultapps}/bin/swda setHandler --URL zoomphonecall --app "${pkgs.zoom-us}/Applications/zoom.us.app"
    run --silence ${pkgs.swiftdefaultapps}/bin/swda setHandler --URL callto --app "${pkgs.zoom-us}/Applications/zoom.us.app"
    run --silence ${pkgs.swiftdefaultapps}/bin/swda setHandler --URL sip --app "${pkgs.zoom-us}/Applications/zoom.us.app"
  '';
  };
}
