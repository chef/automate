package chunks_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/io/chunks"
)

func TestChunkWriter(t *testing.T) {
	testBuffer := []byte("this is a test")

	for i := 1; i < 2*len(testBuffer); i++ {
		bytes := []byte{}
		w := chunks.NewWriter(i, func(p []byte) error {
			bytes = append(bytes, p...)
			return nil
		})

		_, err := w.Write(testBuffer)
		require.NoError(t, err)
		require.Equal(t, string(testBuffer), string(bytes))
	}
}

func TestChunkWriterLargerThanMax(t *testing.T) {
	maxSize := 10
	w := chunks.NewWriter(maxSize, func(p []byte) error {
		assert.True(t, len(p) <= maxSize, "The max size should only be 10 or less")
		return nil
	})

	// 14 bytes
	testBuffer := []byte("this is a test")
	_, err := w.Write(testBuffer)
	assert.NoError(t, err)
}
