package chunks

import (
	"io"
)

type chunkWriter struct {
	chunkFunc    func([]byte) error
	maxChunkSize int
}

type ChunkWriterFunc func(p []byte) error

// NewWriter returns a writer that sends bytes to the provided function.
// It will never ask that function to write more than maxChunkSize bytes
// per call.
func NewWriter(maxChunkSize int, f ChunkWriterFunc) io.Writer {
	return &chunkWriter{
		chunkFunc:    f,
		maxChunkSize: maxChunkSize,
	}
}

func (c *chunkWriter) Write(p []byte) (int, error) {
	if len(p) < c.maxChunkSize {
		return len(p), c.chunkFunc(p)
	}
	writeBuf := p
	for {
		if len(writeBuf) == 0 {
			break
		}
		end := min(len(writeBuf), c.maxChunkSize)
		if err := c.chunkFunc(writeBuf[:end]); err != nil {
			return 0, err
		}
		writeBuf = writeBuf[end:]
	}
	return len(p), nil
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
