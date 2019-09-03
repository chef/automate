package cereal

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestJitterDownIntervalProvider(t *testing.T) {
	base := 10 * time.Second
	jitter := 1 * time.Second
	provider := newJitterDownIntervalProvider(base, jitter)

	t.Run("it doesn't return a value less than interval - jitter", func(t *testing.T) {
		for i := 0; i < 1000; i++ {
			n := provider.Next()
			assert.True(t, n > 9*time.Second, "returned interval is above min")
		}
	})
	// This test is kinda silly
	t.Run("it rarely returns a value equal to the base", func(t *testing.T) {
		sameCount := 0
		for i := 0; i < 1000; i++ {
			n := provider.Next()
			if n == base {
				sameCount++
			}
		}
		assert.True(t, sameCount < 3, "returned interval was the same as the base an unusual number of times")
	})
}

func TestWaywardWorkflowList(t *testing.T) {
	waywardWorkflowTimeout = 10 * time.Millisecond
	t.Run("filter returns the entire list if there are no wayward workflows", func(t *testing.T) {
		w := make(waywardWorkflowList)
		in := []string{"a", "b", "c"}
		out := w.Filter(in)
		assert.Equal(t, in, out)
	})
	t.Run("filter doesn't return wayward workflows", func(t *testing.T) {
		w := make(waywardWorkflowList)
		in := []string{"a", "b", "c"}
		w.Add("b")
		out := w.Filter(in)
		assert.Equal(t, []string{"a", "c"}, out)
	})
	t.Run("filter returns a previously wayward workflow after expiration", func(t *testing.T) {
		w := make(waywardWorkflowList)
		in := []string{"a", "b", "c"}
		w.Add("b")
		time.Sleep(2 * waywardWorkflowTimeout)
		out := w.Filter(in)
		assert.Equal(t, in, out)
	})
}
