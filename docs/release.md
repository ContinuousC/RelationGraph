# Release #

Version management is done through an internal tool called the Version Coordinator.

## Dependencies ##

Install cargo and the rust compiler following the instructions on
https://rustup.rs/. Then, install or update the version coordinator
running `cargo install --registry si version-coordinator`.

## Instructions ##

These instructions must be run from inside the ContinuousC repository,
with the submodules checked out. Make sure everything is committed
before you start.

- To build and tag a test version, run `vc --all --test --commit --tag`.
- To build and tag an acceptance version, run `vc --all --accept --commit --tag`.
- To build and tag a release version, run `vc --all --release --commit --tag`.
