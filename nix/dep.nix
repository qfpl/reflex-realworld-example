{ name, gitpath, nixpkgs ? import <nixpkgs> {} }: super: self:
let gitinfo = nixpkgs.pkgs.lib.importJSON gitpath;
in self.callCabal2nix name (nixpkgs.pkgs.fetchgit {
  inherit (gitinfo) url rev sha256;
}) {}
