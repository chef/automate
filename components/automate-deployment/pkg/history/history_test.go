package history

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSimple(t *testing.T) {
	ctx := LogEvent(context.Background(), "Foo", "Bar")
	assert.True(t, accessInternalCtx(ctx, func(h *historyContext) {
		assert.NotEqual(t, noEventID, h.GetID())
	}))
}
