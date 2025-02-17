# Overview #

[![Build Status (demo)](https://drone.contc/api/badges/ContinuousC/relation-graph/status.svg?ref=refs/heads/development)](https://drone.contc/ContinuousC/relation-graph)

The Relation Graph describes a set of interrelated items, versioned over time.

The repository contains a number of sub-projects:

- The [library](lib.md)
- An [http backend](engine.md) that provides an API for managing and querying the data
- A [WebAssembly library](wasm.md) for use in the frontend
- A [command-line tool](cmd.md) for testing purposes

Topics:

- [Data model](data-model.md)
- [In-memory representation](in-memory-repr.md)
- [Transactions](transactions.md)
- [Status and Alert handling](status-and-alerts.md)
- [Role-based authorization](Role-based%20authorization.pptx)

CI/CD

- [Development](development.md)
- [Release](release.md)
- [Deployment](deploy.md)
