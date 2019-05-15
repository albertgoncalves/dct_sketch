{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
    name = "Haskell";
    buildInputs = [
        (haskell.packages.ghc865.ghcWithPackages (pkgs: [
            pkgs.regex-compat
            pkgs.hlint
            pkgs.hoogle
            pkgs.tf-random
            pkgs.cairo
        ]))
        libiconv
    ];
    shellHook = ''
        if [ $(uname -s) = "Darwin" ]; then
            alias ls='ls --color=auto'
            alias ll='ls -al'
        fi
        alias hlint="hlint -c=never"
    '';
}
