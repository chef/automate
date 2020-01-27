package backup

import (
	"bytes"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"io"
	"io/ioutil"
	"testing"
)

func consume(t *testing.T, stream ArtifactStream) []string {
	ret := []string{}
	for {
		v, err := stream.Next()
		if err != nil {
			if err == io.EOF {
				break
			}
			require.NoError(t, err)
		}
		ret = append(ret, v)
	}
	return ret
}

type readCloserTester struct {
	reader io.ReadCloser
	called bool
}

func (r *readCloserTester) Read(p []byte) (n int, err error) {
	return r.reader.Read(p)
}

func (r *readCloserTester) Close() error {
	r.called = true
	return r.reader.Close()
}

func TestPeekableStream(t *testing.T) {
	t.Run("returns EOF for empty stream", func(t *testing.T) {
		a := NewPeekableArtifactStream(NewArrayStream([]string{}))
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
		a := NewPeekableArtifactStream(NewArrayStream([]string{"a"}))
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
		a := NewPeekableArtifactStream(NewArrayStream([]string{"a", "b"}))
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
		readCloserTester := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBufferString("a\nb\nc")),
		}
		reader := NewPeekableArtifactStream(LineReaderStream(readCloserTester))
		require.NoError(t, reader.Close())
		assert.True(t, readCloserTester.called)
	})
}

func TestLineReaderStream(t *testing.T) {
	t.Run("empty", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBuffer(nil))
		reader := LineReaderStream(buffer)
		assert.Equal(t, []string{}, consume(t, reader))
	})

	t.Run("compact", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBufferString("a\nb\nc"))
		reader := LineReaderStream(buffer)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
	})

	t.Run("compact with trailing newline", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBufferString("a\nb\nc\n"))
		reader := LineReaderStream(buffer)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
	})

	t.Run("multiple trailing newline", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBufferString("a\nb\nc\n\n\n"))
		reader := LineReaderStream(buffer)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
	})

	t.Run("starting newline", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBufferString("\na\nb\nc\n"))
		reader := LineReaderStream(buffer)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
	})

	t.Run("newlines in middle", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBufferString("\na\n\n\nb\n\nc\n"))
		reader := LineReaderStream(buffer)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
	})

	t.Run("chomps whitespace", func(t *testing.T) {
		buffer := ioutil.NopCloser(bytes.NewBufferString(" \na \n \n\n b\n  \nc \n "))
		reader := LineReaderStream(buffer)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
	})

	t.Run("closes underlying stream", func(t *testing.T) {
		readCloserTester := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBufferString("a\nb\nc")),
		}
		reader := LineReaderStream(readCloserTester)
		assert.Equal(t, []string{"a", "b", "c"}, consume(t, reader))
		reader.Close()
		assert.True(t, readCloserTester.called)
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
		readCloserTesterA := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBuffer(nil)),
		}
		readCloserTesterB := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBuffer(nil)),
		}
		a := LineReaderStream(readCloserTesterA)
		b := LineReaderStream(readCloserTesterB)
		c := Xor(a, b)
		require.NoError(t, c.Close())
		assert.True(t, readCloserTesterA.called)
		assert.True(t, readCloserTesterB.called)
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
		readCloserTesterA := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBuffer(nil)),
		}
		readCloserTesterB := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBuffer(nil)),
		}
		a := LineReaderStream(readCloserTesterA)
		b := LineReaderStream(readCloserTesterB)
		c := Sub(a, b)
		require.NoError(t, c.Close())
		assert.True(t, readCloserTesterA.called)
		assert.True(t, readCloserTesterB.called)
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

	t.Run("closes underlying streams 1", func(t *testing.T) {
		readCloserTesterA := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBuffer(nil)),
		}

		a := LineReaderStream(readCloserTesterA)
		c := Merge(a)
		require.NoError(t, c.Close())
		assert.True(t, readCloserTesterA.called)
	})

	t.Run("closes underlying streams 2", func(t *testing.T) {
		readCloserTesterA := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBuffer(nil)),
		}
		readCloserTesterB := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBuffer(nil)),
		}
		a := LineReaderStream(readCloserTesterA)
		b := LineReaderStream(readCloserTesterB)
		c := Merge(a, b)
		require.NoError(t, c.Close())
		assert.True(t, readCloserTesterA.called)
		assert.True(t, readCloserTesterB.called)
	})

	t.Run("closes underlying streams 3", func(t *testing.T) {
		readCloserTesterA := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBuffer(nil)),
		}
		readCloserTesterB := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBuffer(nil)),
		}
		readCloserTesterC := &readCloserTester{
			reader: ioutil.NopCloser(bytes.NewBuffer(nil)),
		}
		a := LineReaderStream(readCloserTesterA)
		b := LineReaderStream(readCloserTesterB)
		c := LineReaderStream(readCloserTesterC)

		d := Merge(a, b, c)
		require.NoError(t, d.Close())
		assert.True(t, readCloserTesterA.called)
		assert.True(t, readCloserTesterB.called)
		assert.True(t, readCloserTesterC.called)
	})
}
