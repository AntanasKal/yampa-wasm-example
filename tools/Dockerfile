FROM ubuntu:latest

# Based on https://github.com/ComposableFi/composable/blob/main/Dockerfile
ARG NIX_VERSION=2.17.0
ARG CHANNEL_URL=https://github.com/NixOS/nixpkgs/archive/1db42b7fe3878f3f5f7a4f2dc210772fd080e205.tar.gz

ARG NIX_INSTALLER=https://nixos.org/nix/install

SHELL [ "/bin/bash", "-o", "pipefail", "-o", "errexit", "-c" ]

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install --yes --no-install-recommends \
    ca-certificates \
    curl \
    sudo \
    xz-utils

RUN mkdir --parents /etc/nix/ && \
    echo "sandbox = relaxed" >> /etc/nix/nix.conf && \
    echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf && \
    echo "cores = 32" >> /etc/nix/nix.conf && \
    echo "allow-import-from-derivation = true" >> /etc/nix/nix.conf && \
    echo "narinfo-cache-negative-ttl = 30" >> /etc/nix/nix.conf  && \
    echo "trusted-users = root vscode actions-runner" >> /etc/nix/nix.conf  && \
    echo "substitute = true" >> /etc/nix/nix.conf  && \
    echo "substituters = https://nix-community.cachix.org/ https://cache.nixos.org/ https://composable.cachix.org/ https://devenv.cachix.org/ https://cosmos.cachix.org https://nixpkgs-update.cachix.org" >> /etc/nix/nix.conf  && \
    echo "require-sigs = false" >> /etc/nix/nix.conf  && \
    echo "trusted-public-keys = nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= cosmos.cachix.org-1:T5U9yg6u2kM48qAOXHO/ayhO8IWFnv0LOhNcq0yKuR8= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= composable.cachix.org-1:J2TVJKH4U8xqYdN/0SpauoAxLuDYeheJtv22Vn3Hav8= nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8= devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=" >> /etc/nix/nix.conf  && \
    echo "trusted-substituters = https://nix-community.cachix.org/ https://cache.nixos.org/ https://composable.cachix.org/ https://devenv.cachix.org/ https://cosmos.cachix.org https://nixpkgs-update.cachix.org/" >> /etc/nix/nix.conf  && \
    passwd --delete root

RUN curl --location ${NIX_INSTALLER} > ~/install.sh && \
    chmod +x ~/install.sh  && \
    ~/install.sh --daemon

ENV HOME=/root
ENV NIX_LINK "/root/.nix-profile"
ENV PATH "$NIX_LINK/bin:$PATH"

RUN source ~/.nix-profile/etc/profile.d/nix.sh && \
      nix shell --extra-experimental-features flakes --extra-experimental-features nix-command "https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz#all_9_6" \
       -c \
       wasm32-wasi-cabal update