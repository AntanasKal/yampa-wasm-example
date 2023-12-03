build-haskell:
	wasm32-wasi-cabal update
	wasm32-wasi-cabal build
	# copy the build binary to a different path
	# (as currently wasm paths depend on particular GHC intermediate version)
	mkdir -p dist/
	cp `wasm32-wasi-cabal list-bin exe:game-core` dist/game-core.wasm

build: build-haskell
	npm install
	npm run bundle

serve: build
	npm run serve
