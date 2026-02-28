# Gappy — Clojure bindings for Google APIs

## Project Overview

Gappy dynamically generates Clojure bindings for Google APIs using the Google
Discovery API. It follows a data-driven, map-based design inspired by
Cognitect's aws-api.

## Architecture

- `gappy.api` — Main API surface: `client`, `resource`, `invoke`, `ops`, `doc`, `resources`
- `gappy.discovery` — Fetches/caches Google Discovery documents
- `gappy.oauth2` — OAuth2 with PKCE (token obtain, refresh, revoke)
- `gappy.util` — Browser OAuth flow, query string building, multipart parsing
- `gappy.config` — cprop/mount config from `resources/config.edn`
- `gappy.core` — Main entry point (minimal)

## Key Design Decisions

- **Data over objects**: API schemas are plain Clojure maps, navigated with generic functions
- **Runtime discovery**: No code generation; schemas fetched/cached at runtime
- **Multimethod dispatch**: `invoke` dispatches on `:http-method` keyword
- **REPL introspection**: `ops` and `doc` for discoverability

## Development

```sh
# Run tests
clj -M:test

# Build JAR
clj -T:build jar

# REPL
clj
```

## Testing

- Tests live in `test/gappy/` using `clojure.test`
- Fixtures (cached discovery docs, mock responses) in `test/fixtures/`
- Live integration tests gated behind `GAPPY_LIVE_TESTS=true` env var
- Use `with-redefs` on `clj-http.client/*` for mocking HTTP in tests

## Conventions

- Clojure style: kebab-case for functions, UPPER_CASE for constants
- Private helpers prefixed with `-` (e.g., `-method-data`)
- Return structured maps from API functions; attach HTTP metadata via `with-meta`
- Google API errors surfaced as `ex-info` with `:gappy/error` data key
- No `println` debugging in production code

## Dependencies

JVM-only for now (cross-platform ClojureScript/Babashka is a future goal):
- `clj-http` for HTTP
- `cheshire` for JSON
- `uritemplate-clj` for RFC 6570 URI templates
- `ring` + `ring-jetty-adapter` for OAuth browser flow
- `cprop` + `mount` for configuration
