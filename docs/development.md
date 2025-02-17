# Development #

The command-line tool can be built and run locally through `cargo
run`. The relation graph engine can be built with auto-reload and
deployed in kubernetes through devspace. Provisioned files from the
main ContinuousC repo are also synchronized. Auto-rebuild of the wasm
library together with the frontend is also possible through devspace.

## Dependencies ##

Local development requires a recent stable version of cargo and the
rust compiler. See https://rustup.rs/ for instructions on how to
download and install this. You may also want to install the
rust-analyzer using `rustup component add rust-analyzer`.

To install or update devspace, run: `npm install -g devspace`.

## Instructions ##

Most setups require the RelationGraph repo to be checked out as a
submodule of the ContinuousC repo. To clone the repo and initialize
its submodules, run:

```
git clone git@[gitea.contc:22222]:ContinuousC/ContinuousC.git
cd ContinuousC
git git submodule update --init
```

For initial deployment to kubernetes, run `devspace deploy` from the
ContinuousC repo.

To start devspace for the relation graph engine only, run `devspace
dev` from the root of the RelationGraph repo. This will also
synchronize provisioned files from `../Chart/provisioned` and the
PrometheusSchema libraries from `../PrometheusSchema`.

If you  want to work  on the  WebAssembly library and/or  the frontend
together with  the backend, run  `devspace dev`  from the root  of the
ContinuousC repo.

When you are done, don't forget to run `devspace reset pods` to revert
to standard deployment and clean up resources.
