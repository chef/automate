package prng

import (
	"math/rand"
	"os"
	"strconv"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

// Seed allows for repeating a certain (failing) test case by exposing the value
// used for seeding its PRNG. Dually, it's accepting a seed value from the
// TEST_SEED env var.
// Note: this doesn't affect the map iteration order; so we're not perfectly
// reproducible. However, given a test failure, there's less moving parts
// hindering the debug process.
func Seed(t *testing.T) *Prng {
	rand.Seed(GenSeed(t))
	return &Prng{}
}

func GenSeed(t *testing.T) int64 {
	seed := time.Now().Unix()
	if s := os.Getenv("TEST_SEED"); s != "" {
		i, err := strconv.Atoi(s)
		require.NoErrorf(t, err, "read random seed from environment TEST_SEED=%q", s)
		seed = int64(i)
	}
	t.Logf("random seed: %d", seed)
	return seed
}

// This re-exports the global random state, exposed by rand (the package)'s
// exported methods, in a way that can be passed to faker.NewWithSeed().
type Prng struct{}

func (*Prng) Int63() int64 { return rand.Int63() }
func (*Prng) Seed(i int64) { rand.Seed(i) }
