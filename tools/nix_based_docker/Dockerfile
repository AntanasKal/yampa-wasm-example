FROM nixos/nix

RUN nix build \
      --extra-experimental-features nix-command \
      --extra-experimental-features flakes \
      "https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz#all_9_6"