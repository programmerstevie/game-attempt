{
  description = "A Haskell Flake?";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.11";
  };

  outputs = { self, nixpkgs, ... } @ inputs :
  let
    system = "x86_64-darwin";
    pkgs = nixpkgs.legacyPackages.${system};
    # pkgsUnstable = inputs.nixpkgsUnstable.legacyPackages.${system};
    # overlayHs = final: prev: {
    #   my-app = prev.callCabal2nix "miniGame" ./. { };
    # };
    # myHaskellPackages = pkgs.haskell.packages.ghc96.extend overlayHs;
  in {
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = with pkgs; [
        SDL2
        SDL2_image
        pkg-config
        pkgs.gcc
      ];
    };
    # = myHaskellPackages.shellFor {
    #   packages = p: [
    #     p.my-app
    #   ];
    #   nativeBuildInputs = with myHaskellPackages; [
    #     ghcid
    #     cabal-install
    #     haskell-language-server
    #     pkgs.SDL2
    #     pkgs.SDL2_ttf
    #     sdl2
    #     # pkgs.gnumake
    #     # pkgs.cmake
    #   ];
    # };
  };
}
