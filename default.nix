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
    reflex-dom-storage = (import ./nix/reflex-dom-storage) super self;
    servant-reflex = (import ./nix/servant-reflex) super self;
    entropy = (import ./nix/entropy) super self;
    mmark = pkgs.haskell.lib.dontCheck self.mmark;
    email-validate = pkgs.haskell.lib.dontCheck self.email-validate;
    #silently = pkgs.haskell.lib.dontCheck self.silently;
    #mockery = pkgs.haskell.lib.dontCheck self.mockery;
    #unliftio = pkgs.haskell.lib.dontCheck self.unliftio;
    #servant = pkgs.haskell.lib.dontCheck self.servant;
    #conduit = pkgs.haskell.lib.dontCheck self.conduit;
  });
})
