{ pkgs ? import <nixpkgs> {}, ... }:

pkgs.mkShell {
    buildInputs = with pkgs; [
        git
        elmPackages.elm
        elm2nix
    ];

    shellHook = ''
        export TERM=xterm-256color
    '';
}