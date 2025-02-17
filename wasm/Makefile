.PHONY: run-test

run-test: test/test.js
	node test/test.js

test/test.js : test/test.ts pkg/package.json
	tsc --lib es2019,dom --resolveJsonModule --esModuleInterop test/test.ts

pkg/package.json: Cargo.toml ../lib/Cargo.toml $(shell find src/ ../lib/src/ -name '*.rs')
	wasm-pack build --release --target nodejs
