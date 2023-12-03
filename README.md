# Yampa Wasm Example

A (WIP) example of using Yampa and GHC Wasm backend to create a simple web game.

!["Circle rotating around the mouse."](assets/rotating_circle.gif)

## Instructions

### Building

1. Clone and enter the repo
    ```
    git clone https://github.com/AntanasKal/yampa-wasm-example
    cd yampa-wasm-example
    ```

2. Enter nix flake of GHC Wasm backend with `all_9_6` flavour: 
    ```
    nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz#all_9_6
    ```
    To leave the flake you may call:
    ```
    exit
    ```
    If you want to setup GHC with Wasm backend without nix, follow [ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta#getting-started-without-nix) guide (make sure to set the flavour to be `9.6`).

3. Inside the flake (or while having GHC with Wasm backend set up by different means), run:
    ```
    make serve
    ```
    This command will build Haskell code, bundle JS code and serve the application.
    
4. The web application should be available at http://localhost:8080/ via your browser.

### Using Docker to build and deploy

1. Clone the repo:
    ```
    git clone https://github.com/AntanasKal/yampa-wasm-example
    ```
2. Run this command:
    ```
    docker run -p 8080:8080 -v $PWD/yampa-wasm-example/:/root/yampa-wasm-example/ -it nixos/nix bash -c "cd /root/yampa-wasm-example/ && nix shell --extra-experimental-features flakes --extra-experimental-features nix-command "https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz#all_9_6" -c make serve"
    ```
    Instead of `nixos/nix` image, one can use a different image based on Dockerfile provided in this repo [here](./tools/Dockerfile). This image is built on top of base Ubuntu.
3. The web application should be available at http://localhost:8080/ via your browser.