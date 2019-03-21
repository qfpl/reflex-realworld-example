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
  overrides = (self: super: {
    reflex-dom-storage = (import ./nix/reflex-dom-storage) self super;
    servant-reflex = (import ./nix/servant-reflex) self super;
    mmark = pkgs.haskell.lib.overrideCabal 
     ((import ./nix/mmark) self super)
     (drv: {
       doHaddock = false;
       doCheck   = false;
     });
    megaparsec = pkgs.haskell.lib.dontCheck ((import ./nix/megaparsec) self super);
    modern-uri = pkgs.haskell.lib.dontCheck ((import ./nix/modern-uri) self super);
    neat-interpolation = pkgs.haskell.lib.dontCheck ((import ./nix/neat-interpolation) self super);
    email-validate = pkgs.haskell.lib.dontCheck super.email-validate;
    servant = pkgs.haskell.lib.dontCheck super.servant;
  });
})
