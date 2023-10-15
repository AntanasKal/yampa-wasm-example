## Yampa Wasm Example

A (WIP) example of using Yampa and GHC Wasm backend to create a simple web game.

### Building

1. Enter nix flake of GHC Wasm of `all_9_6` flavour: 
    ```
    nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz#all_9_6
    ```
    To leave the flake you make call:
    ```
    exit
    ```
    If you want to setup GHC with Wasm backend without nix, follow [ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta#getting-started-without-nix) guide (make sure to set the flavour to be `9.6`).

2. Inside the flake, build Haskell code by:
    ```
    wasm32-wasi-cabal build
    ```
3. Bundle JS code:
    ```
    npm run bundle
    ```
4. Start local server:
    ```
    npm run serve
    ```
5. The web application should be available at http://localhost:8080/ via your browser.
