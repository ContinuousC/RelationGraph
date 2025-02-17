# syntax=docker/dockerfile:1.6

FROM gitea.contc/controlplane/rust-builder:0.2.0 AS source
ARG GITVERSION=
WORKDIR /root/source/relation-graph
COPY --link . /root/source/relation-graph

FROM source AS test
RUN --mount=type=ssh,required=true \
    --mount=type=cache,target=/root/.cargo/git \
    --mount=type=cache,target=/root/.cargo/registry \
    --mount=type=cache,target=/root/source/relation-graph/target \
    RUST_BACKTRACE=full /root/.cargo/bin/cargo test

FROM source AS audit
RUN --mount=type=ssh,required=true \
    --mount=type=cache,target=/root/.cargo/git \
    --mount=type=cache,target=/root/.cargo/registry \
    --mount=type=cache,target=/root/source/relation-graph/target \
    /root/.cargo/bin/cargo audit --color=always

FROM source AS build-engine-dev
RUN --mount=type=ssh,required=true \
    --mount=type=cache,target=/root/.cargo/git \
    --mount=type=cache,target=/root/.cargo/registry \
    --mount=type=cache,target=/root/source/relation-graph/target \
    /root/.cargo/bin/cargo build --bin relation-graph-engine \
    && cp target/debug/relation-graph-engine .

FROM source AS build-cmd-dev
RUN --mount=type=ssh,required=true \
    --mount=type=cache,target=/root/.cargo/git \
    --mount=type=cache,target=/root/.cargo/registry \
    --mount=type=cache,target=/root/source/relation-graph/target \
    /root/.cargo/bin/cargo build --bin relation-graph-cmd \
    && cp target/debug/relation-graph-cmd .

FROM source AS build-engine-release
RUN --mount=type=ssh,required=true \
    --mount=type=cache,target=/root/.cargo/git \
    --mount=type=cache,target=/root/.cargo/registry \
    --mount=type=cache,target=/root/source/relation-graph/target \
    /root/.cargo/bin/cargo build --release --bin relation-graph-engine \
    && cp target/release/relation-graph-engine .

FROM source AS build-cmd-release
RUN --mount=type=ssh,required=true \
    --mount=type=cache,target=/root/.cargo/git \
    --mount=type=cache,target=/root/.cargo/registry \
    --mount=type=cache,target=/root/source/relation-graph/target \
    /root/.cargo/bin/cargo build --release --bin relation-graph-cmd \
    && cp target/release/relation-graph-cmd .

FROM source AS build-wasm-dev
RUN --mount=type=ssh,required=true \
    --mount=type=cache,target=/root/.cargo/git \
    --mount=type=cache,target=/root/.cargo/registry \
    --mount=type=cache,target=/root/source/relation-graph/target \
    PATH=/root/.cargo/bin:$PATH \
#    CARGO_PROFILE_RELEASE_OPT_LEVEL=z \
    wasm-pack build wasm -d pkg-dev --target web --dev \
      --reference-types --weak-refs

FROM source AS build-wasm-release
RUN --mount=type=ssh,required=true \
    --mount=type=cache,target=/root/.cargo/git \
    --mount=type=cache,target=/root/.cargo/registry \
    --mount=type=cache,target=/root/source/relation-graph/target \
    PATH=/root/.cargo/bin:$PATH \
#    CARGO_PROFILE_RELEASE_OPT_LEVEL=z \
    wasm-pack build wasm -d pkg --target web --release \
      --reference-types --weak-refs

FROM ubuntu:24.04 AS engine-base
RUN --mount=type=cache,target=/var/cache/apt/archives \
    apt update && apt install -y ca-certificates

FROM engine-base AS engine-dev
COPY --from=build-engine-dev \
     /root/source/relation-graph/relation-graph-engine \
     /usr/bin/
EXPOSE 9999
CMD /usr/bin/relation-graph-engine

FROM engine-base AS engine-release
COPY --from=build-engine-release \
     /root/source/relation-graph/relation-graph-engine \
     /usr/bin/
EXPOSE 9999
CMD /usr/bin/relation-graph-engine

FROM node:lts-slim AS publish-wasm
ARG WASM_TAG
WORKDIR /root/wasm
RUN --mount=type=cache,id=node-apt-archives,target=/var/cache/apt/archives \
    apt update && apt install -y jq
COPY --from=build-wasm-release /root/source/relation-graph/wasm/pkg .
RUN [ "$(jq -r '.version' package.json)" = "$WASM_TAG" ] || ( echo "\e[31mwasm version tag does not correspond to the version found in wasm/Cargo.toml\e[0m" && exit 1 )
RUN jq '. + {"name": "@continuousc/relation-graph", "publishConfig": {"registry": "https://gitea.contc/api/packages/continuousc/npm/"}}' package.json > package2.json && mv package2.json package.json
RUN --mount=type=secret,id=npmrc,target=/root/.npmrc,required=true \
    npm publish

FROM gitea.contc/controlplane/techdocs-builder:0.0.7 AS build-techdocs
WORKDIR /app
COPY --link mkdocs.yml /app/
COPY --link docs /app/docs
RUN npx @techdocs/cli generate --no-docker

FROM build-techdocs AS publish-techdocs
ENV NODE_TLS_REJECT_UNAUTHORIZED=0
ENV AWS_REGION=us-west-2
RUN --mount=type=secret,id=minio_credentials,target=/root/.minio.env,required=true \
    export $(cat /root/.minio.env); \
    npx @techdocs/cli publish \
        --publisher-type awsS3 \
	--storage-name techdocs \
        --entity default/component/relation-graph \
        --awsEndpoint https://minio.cortex \
        --awsS3ForcePathStyle

FROM source AS build-rustdocs
RUN --mount=type=ssh,required=true \
    --mount=type=cache,target=/root/.cargo/registry \
    --mount=type=cache,target=/root/source/relation-graph/target \
    rm -rf target/doc && \
    /root/.cargo/bin/cargo doc --release --no-deps && \
    cp -a target/doc /docs

FROM bitnami/minio-client AS publish-rustdocs
COPY --from=build-rustdocs /docs /docs
RUN --mount=type=secret,id=minio-config,target=/.mc/config.json,uid=1001,required=true \
    --mount=type=secret,id=contc-ca,target=/.mc/certs/CAs/contc.ca,uid=1001,required=true \
    mc mirror --overwrite --remove /docs minio/rustdocs/relation-graph
