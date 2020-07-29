package db

import (
	"context"
	"database/sql"
	"math/rand"
	"time"

	_ "github.com/lib/pq"
)

type dbOpts struct {
	maxConnectionLifetime       time.Duration
	maxConnectionLifetimeJitter time.Duration
	pingInterval                time.Duration
	pingJitter                  time.Duration
}

type PGOpenOption func(*dbOpts)

// WithMaxConnectionLifetime configures the maximum connection
// lifetime. The current default value is set based on the known
// pg-gateway and a2-backend timeouts. Consider carefully before
// changing. A zero duration disables the connection lifetime.
func WithMaxConnectionLifetime(d time.Duration) PGOpenOption {
	return func(opts *dbOpts) {
		opts.maxConnectionLifetime = d
	}
}

// WithMaxConnectionLifetimeJitter adds a random duration in the range
// of [0, d) to the configured MaxConnectionLifetime if it is
// non-zero.
func WithMaxConnectionLifetimeJitter(d time.Duration) PGOpenOption {
	return func(opts *dbOpts) {
		opts.maxConnectionLifetimeJitter = d
	}
}

// WithPingInterval configures a background goroutine that
// will ping the database every interval. A zero duration disables the
// pinger. Default: 20s
func WithPingInterval(d time.Duration) PGOpenOption {
	return func(opts *dbOpts) {
		opts.pingInterval = d
	}
}

// WithoutPinger disables background pinging.
func WithoutPinger() PGOpenOption { return WithPingInterval(0) }

// WithPingJitter adds a random duration in the range of [o,
// d) to the PeriodicPingInterval. Default: 20s
func WithPingJitter(d time.Duration) PGOpenOption {
	return func(opts *dbOpts) {
		opts.pingJitter = d
	}
}

func PGOpen(dataSourceName string, opts ...PGOpenOption) (*sql.DB, error) {
	return PGOpenContext(context.Background(), dataSourceName, opts...)
}

func PGOpenContext(ctx context.Context, dataSourceName string, userOpts ...PGOpenOption) (*sql.DB, error) {
	opts := &dbOpts{
		maxConnectionLifetime:       15 * time.Minute,
		maxConnectionLifetimeJitter: 15 * time.Minute,
		pingInterval:                20 * time.Second,
		pingJitter:                  20 * time.Second,
	}

	for _, o := range userOpts {
		o(opts)
	}
	db, err := sql.Open("postgres", dataSourceName)
	if err != nil {
		return nil, err
	}

	if opts.maxConnectionLifetime > 0 {
		db.SetConnMaxLifetime(opts.maxConnectionLifetime + jitter(opts.maxConnectionLifetimeJitter))
	}

	if opts.pingInterval > 0 {
		p := newPinger(db, opts.pingInterval+jitter(opts.pingJitter))
		// NOTE(ssd) 2020-02-26: We currently depend on the
		// context for the started goroutine to exit.
		p.Start(ctx)
	}

	return db, nil
}

func jitter(maxJitter time.Duration) time.Duration {
	return time.Duration(float64(maxJitter) * rand.Float64())
}
