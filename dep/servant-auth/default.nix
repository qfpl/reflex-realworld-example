{ nixpkgs ? import <nixpkgs> {} }: self: super:
let 
  gitinfo = nixpkgs.pkgs.lib.importJSON ./git.json;
  servant-auth-src = nixpkgs.pkgs.fetchgit {
    inherit (gitinfo) url rev sha256;
  }; 
in self.callCabal2nix "servant-auth-snap" "${servant-auth-src}/servant-auth-snap" {}
