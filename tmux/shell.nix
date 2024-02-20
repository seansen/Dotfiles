{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.neovim
    # Add any other dependencies you might need for your Neovim setup
  ];
}
