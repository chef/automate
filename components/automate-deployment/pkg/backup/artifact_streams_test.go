package backup

import (
	"bytes"
	"io"
	"io/ioutil"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type readCloserTester struct {
	reader io.ReadCloser
	called int
}

func (r *readCloserTester) Read(p []byte) (n int, err error) {
	return r.reader.Read(p)
}

func (r *readCloserTester) Close() error {
	r.called++
	return r.reader.Close()
}

type streamCloserTester struct {
	stream ArtifactStream
	called int
}

func (s *streamCloserTester) Next() (string, error) {
	return s.stream.Next()
}

func (s *streamCloserTester) Close() error {
	s.called++
	return s.stream.Close()
}

func TestPeekableStream(t *testing.T) {
	t.Run("returns EOF for empty stream", func(t *testing.T) {
		a := NewPeekableStream(NewArrayStream([]string{}))
		_, err := a.Peek()
		assert.Equal(t, io.EOF, err)
		_, err = a.Peek()
		assert.Equal(t, io.EOF, err)

		_, err = a.Next()
		assert.Equal(t, io.EOF, err)
		_, err = a.Next()
		assert.Equal(t, io.EOF, err)
	})

	t.Run("one element", func(t *testing.T) {
		a := NewPeekableStream(NewArrayStream([]string{"a"}))
		v, err := a.Peek()
		require.NoError(t, err)
		assert.Equal(t, "a", v)

		v, err = a.Peek()
		require.NoError(t, err)
		assert.Equal(t, "a", v)

		v, err = a.Next()
		require.NoError(t, err)
		assert.Equal(t, "a", v)

		v, err = a.Next()
		require.Equal(t, io.EOF, err)

		v, err = a.Peek()
		require.Equal(t, io.EOF, err)
	})

	t.Run("multiple element", func(t *testing.T) {
		a := NewPeekableStream(NewArrayStream([]string{"a", "b"}))
		v, err := a.Peek()
		require.NoError(t, err)
		assert.Equal(t, "a", v)

		v, err = a.Next()
		require.NoError(t, err)
		assert.Equal(t, "a", v)

		v, err = a.Peek()
		require.NoError(t, err)
		assert.Equal(t, "b", v)

		v, err = a.Peek()
		require.NoError(t, err)
		assert.Equal(t, "b", v)

		_, err = a.Next()
		require.NoError(t, err)
		assert.Equal(t, "b", v)

		_, err = a.Peek()
		require.Equal(t, io.EOF, err)

		_, err = a.Next()
		require.Equal(t, io.EOF, err)
	})

	t.Run("closes underlying stream", func(t *testing.T) {
		tester := &streamCloserTester{
			stream: EmptyStream(),
		}
		reader := NewPeekableStream(tester)
		require.NoError(t, reader.Close())
		require.NoError(t, reader.Close())
		assert.Equal(t, 1, tester.called)
	})
}

func TestLineReaderStream(t *testing.T) {
	t.Run("empty", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBuffer(nil))
		reader := NewLineReaderStream(buffer)
		assert.Equal(t, []string{}, consume(t, reader))
	})

	t.Run("compact", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBufferString("a\nb\nc"))
		reader := NewLineReaderStream(buffer)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
	})

	t.Run("compact with trailing newline", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBufferString("a\nb\nc\n"))
		reader := NewLineReaderStream(buffer)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
	})

	t.Run("multiple trailing newline", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBufferString("a\nb\nc\n\n\n"))
		reader := NewLineReaderStream(buffer)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
	})

	t.Run("starting newline", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBufferString("\na\nb\nc\n"))
		reader := NewLineReaderStream(buffer)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
	})

	t.Run("newlines in middle", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBufferString("\na\n\n\nb\n\nc\n"))
		reader := NewLineReaderStream(buffer)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
	})

	t.Run("chomps whitespace", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBufferString(" \na \n \n\n b\n  \nc \n "))
		reader := NewLineReaderStream(buffer)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
	})

	t.Run("closes underlying stream", func(t *testing.T) {
		readCloserTester := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBufferString("a\nb\nc")),
		}
		reader := NewLineReaderStream(readCloserTester)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
		require.NoError(t, reader.Close())
		require.NoError(t, reader.Close())
		assert.Equal(t, 1, readCloserTester.called)
	})
}

func TestXor(t *testing.T) {
	t.Run("a is empty", func(t *testing.T) {
		a := NewArrayStream([]string{})
		b := NewArrayStream([]string{"a", "c", "d"})
		c := Xor(a, b)
		assert.Equal(t, consume(t, c), []string{"a", "c", "d"})
	})

	t.Run("b is empty", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "c", "d"})
		b := NewArrayStream([]string{})
		c := Xor(a, b)
		assert.Equal(t, consume(t, c), []string{"a", "c", "d"})
	})

	t.Run("both empty", func(t *testing.T) {
		a := NewArrayStream([]string{})
		b := NewArrayStream([]string{})
		c := Xor(a, b)
		assert.Equal(t, consume(t, c), []string{})
	})

	t.Run("no difference", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "c", "d"})
		b := NewArrayStream([]string{"a", "c", "d"})
		c := Xor(a, b)
		assert.Equal(t, consume(t, c), []string{})
	})

	t.Run("a is longer", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "c", "d"})
		b := NewArrayStream([]string{"a", "c"})
		c := Xor(a, b)
		assert.Equal(t, consume(t, c), []string{"d"})
	})

	t.Run("b is longer", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "c"})
		b := NewArrayStream([]string{"a", "c", "d"})
		c := Xor(a, b)
		assert.Equal(t, consume(t, c), []string{"d"})
	})

	t.Run("ends are different 1", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "c", "d", "f"})
		b := NewArrayStream([]string{"b", "c", "d", "e"})
		c := Xor(a, b)
		assert.Equal(t, consume(t, c), []string{"a", "b", "e", "f"})
	})

	t.Run("ends are different 2", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "c", "d", "f"})
		b := NewArrayStream([]string{"b", "c", "d", "e", "g"})
		c := Xor(a, b)
		assert.Equal(t, consume(t, c), []string{"a", "b", "e", "f", "g"})
	})

	t.Run("ends are different 2", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "c", "d", "f", "g"})
		b := NewArrayStream([]string{"b", "c", "d", "e"})
		c := Xor(a, b)
		assert.Equal(t, consume(t, c), []string{"a", "b", "e", "f", "g"})
	})

	t.Run("middle are different 1", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "c", "d", "f"})
		b := NewArrayStream([]string{"a", "b", "e", "f"})
		c := Xor(a, b)
		assert.Equal(t, consume(t, c), []string{"b", "c", "d", "e"})
	})

	t.Run("closes underlying streams", func(t *testing.T) {
		a := &streamCloserTester{
			stream: EmptyStream(),
		}
		b := &streamCloserTester{
			stream: EmptyStream(),
		}
		c := Xor(a, b)
		require.NoError(t, c.Close())
		require.NoError(t, c.Close())
		assert.Equal(t, 1, a.called)
		assert.Equal(t, 1, b.called)
	})
}

func TestSub(t *testing.T) {
	t.Run("no overlap 1", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c"})
		b := NewArrayStream([]string{"d", "e"})
		c := Sub(a, b)
		assert.Equal(t, consume(t, c), []string{"a", "b", "c"})
	})

	t.Run("no overlap 1", func(t *testing.T) {
		a := NewArrayStream([]string{"b", "c", "d"})
		b := NewArrayStream([]string{"a", "e"})
		c := Sub(a, b)
		assert.Equal(t, consume(t, c), []string{"b", "c", "d"})
	})

	t.Run("no overlap 2", func(t *testing.T) {
		a := NewArrayStream([]string{"x", "y", "z"})
		b := NewArrayStream([]string{"a", "e"})
		c := Sub(a, b)
		assert.Equal(t, consume(t, c), []string{"x", "y", "z"})
	})

	t.Run("edge overlap", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c"})
		b := NewArrayStream([]string{"a", "c"})
		c := Sub(a, b)
		assert.Equal(t, consume(t, c), []string{"b"})
	})

	t.Run("middle overlap 1", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c", "d"})
		b := NewArrayStream([]string{"b", "e"})
		c := Sub(a, b)
		assert.Equal(t, consume(t, c), []string{"a", "c", "d"})
	})

	t.Run("middle overlap 2", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c", "d"})
		b := NewArrayStream([]string{"b", "c"})
		c := Sub(a, b)
		assert.Equal(t, consume(t, c), []string{"a", "d"})
	})

	t.Run("long a 1", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c", "d"})
		b := NewArrayStream([]string{"a"})
		c := Sub(a, b)
		assert.Equal(t, consume(t, c), []string{"b", "c", "d"})
	})

	t.Run("long a 2", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c", "d"})
		b := NewArrayStream([]string{"d"})
		c := Sub(a, b)
		assert.Equal(t, consume(t, c), []string{"a", "b", "c"})
	})

	t.Run("long a 3", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c", "d"})
		b := NewArrayStream([]string{"z"})
		c := Sub(a, b)
		assert.Equal(t, consume(t, c), []string{"a", "b", "c", "d"})
	})

	t.Run("long b 1", func(t *testing.T) {
		a := NewArrayStream([]string{"a"})
		b := NewArrayStream([]string{"a", "b", "c", "d"})
		c := Sub(a, b)
		assert.Equal(t, consume(t, c), []string{})
	})

	t.Run("long b 2", func(t *testing.T) {
		a := NewArrayStream([]string{"a"})
		b := NewArrayStream([]string{"b", "c", "d"})
		c := Sub(a, b)
		assert.Equal(t, consume(t, c), []string{"a"})
	})

	t.Run("closes underlying streams", func(t *testing.T) {
		a := &streamCloserTester{
			stream: EmptyStream(),
		}
		b := &streamCloserTester{
			stream: EmptyStream(),
		}
		c := Sub(a, b)
		require.NoError(t, c.Close())
		require.NoError(t, c.Close())
		assert.Equal(t, 1, a.called)
		assert.Equal(t, 1, b.called)
	})
}

func TestMerge(t *testing.T) {
	t.Run("no streams", func(t *testing.T) {
		assert.Equal(t, []string{}, consume(t, Merge()))
	})

	t.Run("one stream", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c"})
		b := Merge(a)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, b))
	})

	t.Run("empty a", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c"})
		b := NewArrayStream([]string{})
		c := Merge(a, b)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, c))
	})

	t.Run("empty b", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c"})
		b := NewArrayStream([]string{})
		c := Merge(a, b)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, c))
	})

	t.Run("no overlap 1", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "c"})
		b := NewArrayStream([]string{"b", "d", "e"})
		c := Merge(a, b)
		assert.Equal(t, []string{"a", "b", "c", "d", "e"}, consume(t, c))
	})

	t.Run("no overlap 2", func(t *testing.T) {
		a := NewArrayStream([]string{"x", "y"})
		b := NewArrayStream([]string{"a", "b", "c"})
		c := Merge(a, b)
		assert.Equal(t, []string{"a", "b", "c", "x", "y"}, consume(t, c))
	})

	t.Run("full overlap", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c"})
		b := NewArrayStream([]string{"a", "b", "c"})
		c := Merge(a, b)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, c))
	})

	t.Run("overlap 1", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c", "d"})
		b := NewArrayStream([]string{"a", "b", "c"})
		c := Merge(a, b)
		assert.Equal(t, []string{"a", "b", "c", "d"}, consume(t, c))
	})

	t.Run("overlap 2", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c", "d", "z"})
		b := NewArrayStream([]string{"a", "b", "c", "x", "y"})
		c := Merge(a, b)
		assert.Equal(t, []string{"a", "b", "c", "d", "x", "y", "z"}, consume(t, c))
	})

	t.Run("overlap 3", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c", "d", "y"})
		b := NewArrayStream([]string{"a", "b", "c", "x", "y", "z"})
		c := Merge(a, b)
		assert.Equal(t, []string{"a", "b", "c", "d", "x", "y", "z"}, consume(t, c))
	})
}

func TestMerge3Streams(t *testing.T) {
	t.Run("overlap 1", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c", "d"})
		b := NewArrayStream([]string{"a", "b", "c"})
		c := NewArrayStream([]string{"a", "x", "y", "z"})
		d := Merge(a, b, c)
		assert.Equal(t, []string{"a", "b", "c", "d", "x", "y", "z"}, consume(t, d))
	})

	t.Run("overlap 2", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c", "d"})
		b := NewArrayStream([]string{"a", "b", "c"})
		c := NewArrayStream([]string{"a", "b", "c", "d", "x", "y", "z"})
		d := Merge(a, b, c)
		assert.Equal(t, []string{"a", "b", "c", "d", "x", "y", "z"}, consume(t, d))
	})

	t.Run("no overlap 1", func(t *testing.T) {
		a := NewArrayStream([]string{"a", "b", "c", "f"})
		b := NewArrayStream([]string{"d", "e", "j", "k"})
		c := NewArrayStream([]string{"g", "h", "i", "l"})
		d := Merge(a, b, c)
		assert.Equal(t, []string{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"}, consume(t, d))
	})

	t.Run("closes underlying streams 2", func(t *testing.T) {
		a := &streamCloserTester{
			stream: EmptyStream(),
		}
		b := &streamCloserTester{
			stream: EmptyStream(),
		}
		c := Merge(a, b)
		require.NoError(t, c.Close())
		require.NoError(t, c.Close())
		assert.Equal(t, 1, a.called)
		assert.Equal(t, 1, b.called)
	})

	t.Run("closes underlying streams 3", func(t *testing.T) {
		a := &streamCloserTester{
			stream: EmptyStream(),
		}
		b := &streamCloserTester{
			stream: EmptyStream(),
		}
		c := &streamCloserTester{
			stream: EmptyStream(),
		}

		d := Merge(a, b, c)
		require.NoError(t, d.Close())
		require.NoError(t, d.Close())
		assert.Equal(t, 1, a.called)
		assert.Equal(t, 1, b.called)
		assert.Equal(t, 1, c.called)
	})
}

// TestReplayableStream tests that the replayable stream is replayable and that
// it cleans up the underlying streams and files it uses.
func TestReplayableStream(t *testing.T) {
	t.Run("replayable if reset", func(t *testing.T) {
		contents := []string{"a", "b", "c"}
		r, err := NewReplayableStream(NewArrayStream(contents))
		require.NoError(t, err)
		// Consume, verify, reset, repeat
		require.Equal(t, contents, consume(t, r))
		require.NoError(t, r.Reset())
		require.Equal(t, contents, consume(t, r))
		require.NoError(t, r.Reset())
		require.Equal(t, contents, consume(t, r))
		// Make sure it throws an error when we try to consume it without a reset
		_, err = r.Next()
		require.Error(t, err)
		// Can close twice without error
		require.NoError(t, r.Close())
		require.NoError(t, r.Close())
	})

	t.Run("closes underlying stream", func(t *testing.T) {
		a := &streamCloserTester{
			stream: EmptyStream(),
		}
		r, err := NewReplayableStream(a)
		require.NoError(t, err)
		require.NoError(t, r.Close())
		require.Equal(t, 1, a.called)
	})

	t.Run("closes reader stream and cleans up replay file", func(t *testing.T) {
		contents := []string{"a", "b", "c"}
		a := &streamCloserTester{
			stream: NewArrayStream(contents),
		}
		// Manually compose the replay stream so that we can verify it closes things
		replayFile, err := ioutil.TempFile("", "replay-test")
		require.NoError(t, err)
		r := &replayableStream{
			replayFile:  replayFile,
			writeStream: NewLoggingStream(a, replayFile),
		}
		consume(t, r)
		require.NoError(t, r.Close())
		require.Equal(t, 1, a.called)
		_, err = os.Stat(replayFile.Name())
		require.True(t, os.IsNotExist(err))
	})

	t.Run("always consumes source stream entirely before replay", func(t *testing.T) {
		contents := []string{"a", "b", "c"}
		r, err := NewReplayableStream(NewArrayStream(contents))
		require.NoError(t, err)
		// Consume some, reset, consume all and expect replay to have all
		res, err := r.Next()
		require.NoError(t, err)
		require.Equal(t, "a", res)
		res, err = r.Next()
		require.NoError(t, err)
		require.Equal(t, "b", res)

		require.NoError(t, r.Reset())
		require.Equal(t, contents, consume(t, r))
	})
}
