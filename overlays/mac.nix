self: pkgs:
    pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin rec {
      mac-apps       = pkgs.callPackages ../pkgs/mac {};
      firefox        = self.mac-apps.firefox;
      intellij-idea  = self.mac-apps.intellij-idea-ce;
      exfalso        = self.mac-apps.exfalso;
      terraform-docs = self.mac-apps.terraform-docs;
      aws-vpn        = self.mac-apps.aws-vpn;
    }
