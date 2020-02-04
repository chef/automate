package backup

import "io"

// ArtifactStream streams a list of artifacts keys
// by name. A stream may not produce the same value
// twice.
// Returns io.EOF when no more elements will be produced
type ArtifactStream interface {
	Next() (string, error)
}

// ErrStream is an ArtifactStream that always returns
// the given error
func ErrStream(err error) ArtifactStream {
	return &errStream{
		err: err,
	}
}

type errStream struct {
	err error
}

func (e errStream) Next() (string, error) {
	return "", e.err
}

// EmptyStream is an ArtifactStream that always returns
// io.EOF
func EmptyStream() ArtifactStream {
	return ErrStream(io.EOF)
}

// PeekableArtifactStream is an artifact stream that
// allows peeking at the next element without consuming
// it
type PeekableArtifactStream interface {
	ArtifactStream
	// Peek returns the next element that will be returned
	// when Next is called. It can be called multiple times
	// and will keep generating the same value until Next
	// is called
	Peek() (string, error)
}

type peekableArtifactStream struct {
	stream ArtifactStream
	next   *string
	err    error
}

func (p *peekableArtifactStream) Next() (string, error) {
	if p.err != nil {
		return "", p.err
	}
	if p.next != nil {
		ptr := p.next
		p.next = nil
		return *(ptr), nil
	}
	return p.stream.Next()
}

func (p *peekableArtifactStream) Peek() (string, error) {
	if p.err != nil {
		return "", p.err
	}
	if p.next != nil {
		return *(p.next), nil
	}
	v, err := p.stream.Next()
	if err != nil {
		p.err = err
		return "", err
	}
	p.next = &v
	return v, nil
}

// NewPeekableArtifactStream wraps the given ArtifactStream so that it can be peeked
func NewPeekableArtifactStream(s ArtifactStream) PeekableArtifactStream {
	return &peekableArtifactStream{
		stream: s,
	}
}

type xorStream struct {
	a PeekableArtifactStream
	b PeekableArtifactStream
}

func (d *xorStream) Next() (string, error) {
	for {
		aNext, err := d.a.Peek()
		if err != nil {
			if err == io.EOF {
				return d.b.Next()
			}
			return "", err
		}

		bNext, err := d.b.Peek()
		if err != nil {
			if err == io.EOF {
				return d.a.Next()
			}
			return "", err
		}

		if aNext < bNext {
			return d.a.Next()
		} else if aNext > bNext {
			return d.b.Next()
		} else {
			if _, err := d.a.Next(); err != nil {
				return "", err
			}
			if _, err := d.b.Next(); err != nil {
				return "", err
			}
		}
	}
}

// Xor returns a stream with elements in a or b but not both
// a xor b
func Xor(a ArtifactStream, b ArtifactStream) ArtifactStream {
	peekableA := NewPeekableArtifactStream(a)
	peekableB := NewPeekableArtifactStream(b)

	if _, err := peekableA.Peek(); err != nil {
		if err == io.EOF {
			return b
		}
		return ErrStream(err)
	}

	if _, err := peekableB.Peek(); err != nil {
		if err == io.EOF {
			return peekableA
		}
		return ErrStream(err)
	}

	return &xorStream{
		a: peekableA,
		b: peekableB,
	}
}

type subStream struct {
	a PeekableArtifactStream
	b PeekableArtifactStream
}

func (d *subStream) Next() (string, error) {
	for {
		aNext, err := d.a.Peek()
		if err != nil {
			if err == io.EOF {
				return "", io.EOF
			}
			return "", err
		}

		bNext, err := d.b.Peek()
		if err != nil {
			if err == io.EOF {
				return d.a.Next()
			}
			return "", err
		}

		if aNext < bNext {
			return d.a.Next()
		} else if aNext > bNext {
			d.b.Next() // nolint: errcheck
		} else {
			d.a.Next() // nolint: errcheck
			d.b.Next() // nolint: errcheck
		}
	}
}

// Sub returns a stream with elements in a but not in b
// a - b
func Sub(a ArtifactStream, b ArtifactStream) ArtifactStream {
	peekableA := NewPeekableArtifactStream(a)
	peekableB := NewPeekableArtifactStream(b)

	return &subStream{
		a: peekableA,
		b: peekableB,
	}
}

type mergeStream struct {
	a PeekableArtifactStream
	b PeekableArtifactStream
}

func (d *mergeStream) Next() (string, error) {
	for {
		aNext, err := d.a.Peek()
		if err != nil {
			if err == io.EOF {
				return d.b.Next()
			}
			return "", err
		}

		bNext, err := d.b.Peek()
		if err != nil {
			if err == io.EOF {
				return d.a.Next()
			}
			return "", err
		}

		if aNext < bNext {
			return d.a.Next()
		} else if aNext > bNext {
			return d.b.Next()
		} else {
			d.b.Next() // nolint: errcheck
			return d.a.Next()
		}
	}
}

// Merge returns a stream which is the union of all the given
// streams
func Merge(streams ...ArtifactStream) ArtifactStream {
	if len(streams) == 0 {
		return EmptyStream()
	} else if len(streams) == 1 {
		return streams[0]
	}

	root := &mergeStream{
		a: NewPeekableArtifactStream(streams[0]),
	}
	last := root
	for i := 1; i < len(streams); i++ {
		p := NewPeekableArtifactStream(streams[i])

		if i == len(streams)-1 {
			last.b = p
		} else {
			next := &mergeStream{
				a: p,
			}
			last.b = NewPeekableArtifactStream(next)
			last = next
		}
	}

	return root
}
