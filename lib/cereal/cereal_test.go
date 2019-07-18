package cereal

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

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
