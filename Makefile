.PHONY: docker docker-dev push-image push-image-dev release release-abo update-ts

docker:
	-docker image rm relation-graph-engine
	DOCKER_BUILDKIT=1 docker build --ssh default --target engine-release -t relation-graph-engine .

docker-dev:
	-docker image rm relation-graph-engine-dev
	DOCKER_BUILDKIT=1 docker build --ssh default --target engine-dev -t relation-graph-engine-dev .

push-image: docker
	docker tag relation-graph-engine:latest gitea.contc/continuousc/relation-graph-engine
	docker push gitea.contc/continuousc/relation-graph-engine:latest

push-image-dev: docker-dev
	docker tag relation-graph-engine-dev:latest gitea.contc/continuousc/relation-graph-engine
	docker push gitea.contc/continuousc/relation-graph-engine:latest

clean-images:
	docker image ls | sed -rne 's@^(gitea.contc/continuousc/)?relation-graph-engine(-dev)?\s+<none>\s+([0-9a-f]+)  .*$@\3@p' | xargs -rn1 docker image rm

update-wasm:
#	cargo build --release
	CARGO_PROFILE_RELEASE_OPT_LEVEL=z wasm-pack build wasm --target web --release --reference-types --weak-refs
	@mkdir -p ../Frontend/node_modules/@continuousc/relation-graph
	cp -a wasm/pkg/* ../Frontend/node_modules/@continuousc/relation-graph

openapi.json: $(shell find engine/src lib/src -name '*.rs')
	cargo run --bin relation-graph-engine -- --spec --app-version 0.0.0 > $@

engine/ts/relation-graph-api.d.ts: openapi.json
	npx openapi-typescript $< --output $@

update-ts: engine/ts/relation-graph-api.d.ts
	cp -a $< ../Frontend/src/types/
