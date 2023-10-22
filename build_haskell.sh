wasm32-wasi-cabal build
# copy the build binary to a different path
# (as currently wasm paths depend on particular GHC intermediate version)
cp `wasm32-wasi-cabal list-bin exe:game-core` dist/game-core.wasm