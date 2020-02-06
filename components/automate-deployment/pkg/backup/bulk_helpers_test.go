package backup

import (
	"io"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestBulkDeleteBatcher(t *testing.T) {
	t.Run("len(0)", func(t *testing.T) {
		stream := EmptyStream()
		batcher := newBulkDeleteBatcher(3, "", stream)
		_, err := batcher.Next()
		assert.Equal(t, io.EOF, err)
	})

	t.Run("exact read", func(t *testing.T) {
		stream := NewArrayStream([]string{"a", "b", "c"})
		batcher := newBulkDeleteBatcher(3, "", stream)
		batch, err := batcher.Next()
		require.NoError(t, err)
		assert.Equal(t, []string{"a", "b", "c"}, batch)

		_, err = batcher.Next()
		require.Equal(t, io.EOF, err)

		_, err = batcher.Next()
		require.Equal(t, io.EOF, err)
	})

	t.Run("batch size bigger than len(items)", func(t *testing.T) {
		stream := NewArrayStream([]string{"a", "b", "c"})
		batcher := newBulkDeleteBatcher(5, "", stream)
		batch, err := batcher.Next()
		require.NoError(t, err)
		assert.Equal(t, []string{"a", "b", "c"}, batch)

		_, err = batcher.Next()
		require.Equal(t, io.EOF, err)

		_, err = batcher.Next()
		require.Equal(t, io.EOF, err)
	})

	t.Run("batch size smaller than len(items) 1", func(t *testing.T) {
		stream := NewArrayStream([]string{"a", "b", "c"})
		batcher := newBulkDeleteBatcher(1, "", stream)

		batch, err := batcher.Next()
		require.NoError(t, err)
		assert.Equal(t, []string{"a"}, batch)

		batch, err = batcher.Next()
		require.NoError(t, err)
		assert.Equal(t, []string{"b"}, batch)

		batch, err = batcher.Next()
		require.NoError(t, err)
		assert.Equal(t, []string{"c"}, batch)

		_, err = batcher.Next()
		require.Equal(t, io.EOF, err)

		_, err = batcher.Next()
		require.Equal(t, io.EOF, err)
	})

	t.Run("batch size smaller than len(items) 2", func(t *testing.T) {
		stream := NewArrayStream([]string{"a", "b", "c"})
		batcher := newBulkDeleteBatcher(2, "", stream)

		batch, err := batcher.Next()
		require.NoError(t, err)
		assert.Equal(t, []string{"a", "b"}, batch)

		batch, err = batcher.Next()
		require.NoError(t, err)
		assert.Equal(t, []string{"c"}, batch)

		_, err = batcher.Next()
		require.Equal(t, io.EOF, err)

		_, err = batcher.Next()
		require.Equal(t, io.EOF, err)
	})
}
