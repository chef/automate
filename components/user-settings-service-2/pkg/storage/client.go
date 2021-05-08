package storage

// Client is the interface for storage operations used by `server.Server`.
// In production, it's a `postgres.Postgres` object. We use an interface here
// for two reasons:
// 1. Facilitate the use of mocks for unit testing
// 2. Expose only a subset of the functionality of the Postgres object's
// methods: we wish to hide low-level and testing-only methods from the
// application code in the Server.
type Client interface{}
