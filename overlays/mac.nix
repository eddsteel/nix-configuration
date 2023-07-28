self: pkgs:
    pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin rec {
      nixpkgs-local  = pkgs.callPackages ../../../src/nixpkgs {};
      mac-apps       = pkgs.callPackages ../pkgs/mac {};
      bitwarden      = self.mac-apps.bitwarden;
      signal-desktop = self.mac-apps.signal;
      firefox        = self.mac-apps.firefox;
      intellij-idea  = self.mac-apps.intellij-idea-ce;
      exfalso        = self.mac-apps.exfalso;
      terraform-docs = self.mac-apps.terraform-docs;
      aws-vpn        = self.mac-apps.aws-vpn;
    }
