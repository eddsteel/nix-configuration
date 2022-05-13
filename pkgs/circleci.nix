{ buildGoModule, fetchFromGitHub, pkgs ? import <nixpkgs> {} }:
buildGoModule rec {
  pname = "circleci";
  version = "0.1.17183";
  checkPhase = "true";
  src = fetchFromGitHub {
    owner = "circleCI-Public";
    repo = "circleci-cli";
    rev = "v${version}";
    sha256 = "sha256-GEjf5A5NeMiCBZIcqrtd6/7lwVV5N6XarRJTuu2XZ90=";
  };
  vendorSha256 = "sha256-7u2y1yBVpXf+D19tslD4s3B1KmABl4OWNzzLaBNL/2U=";
  meta = with pkgs.lib; {
    description = "Circle CI CLI";
    homepage = "https://circleci.com";
    maintainers = [];
    platforms = platforms.linux ++ platforms.darwin;
  };
}
