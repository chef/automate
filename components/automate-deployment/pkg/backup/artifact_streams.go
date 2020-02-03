package backup

import (
	"bufio"
	"fmt"
	"io"
	"strings"

	"go.uber.org/multierr"
)

// ArtifactStream streams a list of artifacts keys
// by name. A stream may not produce the same value
// twice.
// Returns io.EOF when no more elements will be produced
type ArtifactStream interface {
	Next() (string, error)
	Close() error
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

func (e errStream) Close() error {
	return nil
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

	closed bool
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

func (p *peekableArtifactStream) Close() error {
	if p.closed {
		return nil
	}
	p.closed = true
	return p.stream.Close()
}

// NewPeekableStream wraps the given ArtifactStream so that it can be peeked
func NewPeekableStream(s ArtifactStream) PeekableArtifactStream {
	return &peekableArtifactStream{
		stream: s,
	}
}

type lineReaderStream struct {
	r        io.ReadCloser
	scanner  *bufio.Scanner
	finished bool
	closed   bool
}

func (s *lineReaderStream) Next() (string, error) {
	for {
		if s.finished {
			return "", io.EOF
		}

		s.finished = !s.scanner.Scan()

		if err := s.scanner.Err(); err != nil {
			return "", err
		}
		txt := strings.TrimSpace(s.scanner.Text())
		if txt == "" {
			continue
		}
		return txt, nil
	}

}

func (s *lineReaderStream) Close() error {
	if s.closed {
		return nil
	}
	s.closed = true
	return s.r.Close()
}

// NewLineReaderStream returns an artifact stream that reads line by line
// from the provided reader, and produces each line as an item
func NewLineReaderStream(reader io.ReadCloser) ArtifactStream {
	scanner := bufio.NewScanner(reader)

	return &lineReaderStream{
		r:        reader,
		scanner:  scanner,
		finished: false,
	}
}

type arrayStream struct {
	idx   int
	items []string
}

func (a *arrayStream) Next() (string, error) {
	if a.idx >= len(a.items) {
		return "", io.EOF
	}
	idx := a.idx
	a.idx++
	return a.items[idx], nil
}

func (a *arrayStream) Close() error {
	return nil
}

func NewArrayStream(items []string) ArtifactStream {
	return &arrayStream{
		items: items,
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

func (d *xorStream) Close() error {
	err1 := d.a.Close()
	err2 := d.b.Close()
	return multierr.Combine(err1, err2)
}

// Xor returns a stream with elements in a or b but not both
// a xor b
func Xor(a ArtifactStream, b ArtifactStream) ArtifactStream {
	peekableA := NewPeekableStream(a)
	peekableB := NewPeekableStream(b)

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

func (d *subStream) Close() error {
	err1 := d.a.Close()
	err2 := d.b.Close()
	return multierr.Combine(err1, err2)
}

// Sub returns a stream with elements in a but not in b
// a - b
func Sub(a ArtifactStream, b ArtifactStream) ArtifactStream {
	peekableA := NewPeekableStream(a)
	peekableB := NewPeekableStream(b)

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

func (d *mergeStream) Close() error {
	err1 := d.a.Close()
	err2 := d.b.Close()
	return multierr.Combine(err1, err2)
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
		a: NewPeekableStream(streams[0]),
	}
	last := root
	for i := 1; i < len(streams); i++ {
		p := NewPeekableStream(streams[i])

		if i == len(streams)-1 {
			last.b = p
		} else {
			next := &mergeStream{
				a: p,
			}
			last.b = NewPeekableStream(next)
			last = next
		}
	}

	return root
}

type loggingStream struct {
	stream ArtifactStream
	out    io.Writer
	closed bool
}

func (s *loggingStream) Next() (string, error) {
	next, err := s.stream.Next()
	if err != nil {
		return "", err
	}

	if _, err := fmt.Fprintln(s.out, next); err != nil {
		return "", err
	}

	return next, nil
}

func (s *loggingStream) Close() error {
	if s.closed {
		return nil
	}
	s.closed = true
	return s.stream.Close()
}

// NewLoggingStream returns a stream which writes the produced items to the provided
// writer as the stream is read.
func NewLoggingStream(stream ArtifactStream, out io.Writer) ArtifactStream {
	return &loggingStream{
		stream: stream,
		out:    out,
	}
}

// CountingStream is a stream that keeps track of the number of items procuced
type CountingStream interface {
	ArtifactStream
	Count() int64
}

type countingStream struct {
	stream ArtifactStream
	count  int64
	closed bool
}

func (s *countingStream) Next() (string, error) {
	next, err := s.stream.Next()
	if err != nil {
		return "", err
	}

	s.count++

	return next, nil
}

func (s *countingStream) Close() error {
	if s.closed {
		return nil
	}
	s.closed = true
	return s.stream.Close()
}

func (s *countingStream) Count() int64 {
	return s.count
}

// NewCountingStream returns a stream which keeps track of the number of
// items produced
func NewCountingStream(stream ArtifactStream) CountingStream {
	return &countingStream{
		stream: stream,
	}
}

// ConsumeStream reads all elements in the stream until EOF is reached.
func ConsumeStream(stream ArtifactStream) error {
	for {
		_, err := stream.Next()
		if err != nil {
			if err == io.EOF {
				return nil
			}
			return err
		}
	}
}
