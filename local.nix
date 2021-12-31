{pkgs, ...}:
{
  scripts = pkgs.callPackage ../../src/scripts {};
  brainzo = pkgs.callPackage ../../src/brainzo {};
}
