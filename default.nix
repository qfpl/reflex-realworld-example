{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  withHoogle = true;
  overrides = (super: self: {
    servant-reflex = self.callCabal2nix "servant-reflex" (pkgs.fetchFromGitHub {
      owner = "imalsogreg";
      repo  = "servant-reflex";
      rev = "ba8d4f8a269d785ed7e62f11eddb392d9f582e19";
      sha256 = "0zppzl1ii01bzjrfj5x71vff5ivpcngrs0njvjawx6hf985x2zbk";
    }) {};
    reflex-dom-storage = self.callCabal2nix "reflex-dom-storage" (pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo  = "reflex-dom-storage";
      rev = "5b39a385456587d051b4907f98a8f408c6720f18";
      sha256 = "1lzr1ibhibxkrnn9vn76vs31nmsxyw5bwjj4yrki68zvgr2s9fzw";
    }) {};
  });
})
