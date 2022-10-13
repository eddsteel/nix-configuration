{pkgs, config, ...} :
let
  workPkgs = pkgs.callPackages ../../../src/nix-work {};
  secrets = import ../home/secrets { inherit workPkgs; };
in
{
  name = "work";
  email = secrets.email;
  gpg = "8433C6F9F807CE8E8DFA99EFB10455BC05772724";
  packages = with pkgs; [jdk17 kotlin gradle terraform terraform-docs] ++ workPkgs.all;
  gnome = false;
  linux = false;
  go = true;
  macos = true;
  src.repos = [
    {"name" = "git-web-link";}
    {"name" = "scripts";}
    {"name" = "nixpkgs"; "remote" = "git@github.com:NixOS/nixpkgs"; "fork" = true; }
  ] ++ secrets.repos;
  bashAliases = {
    "s3" = "AWS_PROFILE=s3-dl-personal ${pkgs.scripts}/bin/s3";
    "gradle" = "envchain gradle gradle";
  } // secrets.aliases;
}
