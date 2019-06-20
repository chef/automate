package fileutils

import (
	"fmt"
	"io"
	"os"
	"path"
	"path/filepath"

	"go.uber.org/multierr"
)

const defaultCreateMode os.FileMode = 0644

type atomicWriteOpts struct {
	mode   os.FileMode
	noSync bool
	chown  bool
	uid    int
	gid    int
}

// AtomicWriteOpt allows setting options for writing a file
type AtomicWriteOpt func(*atomicWriteOpts)

// WithAtomicWriteFileMode specifies the file mode the file must have
func WithAtomicWriteFileMode(mode os.FileMode) AtomicWriteOpt {
	return func(opts *atomicWriteOpts) {
		opts.mode = mode
	}
}

func WithAtomicWriteChown(uid int, gid int) AtomicWriteOpt {
	return func(opts *atomicWriteOpts) {
		opts.uid = uid
		opts.gid = gid
		opts.chown = true
	}
}

// WithAtomicWriterNoSync specifies if sync should be skipped. Skipping
// sync is not safe.
func WithAtomicWriteNoSync(noSync bool) AtomicWriteOpt {
	return func(opts *atomicWriteOpts) {
		opts.noSync = noSync
	}
}

// AtomicWrite reads the content of r into the file at path p atomically.
// This is done by first writing into a temporary file, and then renaming that into
// place.
func AtomicWrite(p string, r io.Reader, opts ...AtomicWriteOpt) error {
	return AtomicWriter(p, func(w io.Writer) error {
		_, err := io.Copy(w, r)
		return err
	}, opts...)
}

type atomicWriter struct {
	finalPath string
	tmpPath   string
	f         *os.File
	failed    bool
	noSync    bool
	closed    bool
}

type WriteCloserFailer interface {
	io.WriteCloser
	Fail(error) error
}

func NewAtomicWriter(p string, opts ...AtomicWriteOpt) (WriteCloserFailer, error) {
	dirname := filepath.Dir(p)
	tmpPath := path.Join(dirname, fmt.Sprintf(".tmp.%s", filepath.Base(p)))
	writeOpts := atomicWriteOpts{
		mode: defaultCreateMode,
	}

	for _, o := range opts {
		o(&writeOpts)
	}

	f, err := os.OpenFile(tmpPath, os.O_RDWR|os.O_CREATE|os.O_TRUNC, writeOpts.mode)
	if err != nil {
		return nil, err
	}

	if writeOpts.chown {
		err := os.Chown(tmpPath, writeOpts.uid, writeOpts.gid)
		if err != nil {
			f.Close() // nolint: err-check
			return nil, err
		}
	}

	return &atomicWriter{
		finalPath: p,
		tmpPath:   tmpPath,
		f:         f,
		noSync:    writeOpts.noSync,
	}, nil
}

func (w *atomicWriter) Write(p []byte) (n int, err error) {
	return w.f.Write(p)
}

func (w *atomicWriter) Fail(err error) error {
	w.failed = true

	return multierr.Combine(
		err,
		w.Close(),
	)
}

func (w *atomicWriter) Close() error {
	if w.closed {
		return nil
	}

	w.closed = true

	if !w.noSync {
		if err := w.f.Sync(); err != nil {
			return err
		}
	}

	if err := w.f.Close(); err != nil {
		return err
	}

	if !w.failed {
		if err := os.Rename(w.tmpPath, w.finalPath); err != nil {
			return err
		}

		if !w.noSync {
			dirname := filepath.Dir(w.finalPath)
			if err := syncDir(dirname); err != nil {
				return err
			}
		}
	}

	return nil
}

// AtomicWriter writes a file atomically into p. It provides a writer to given callback.
// If the callback returns an error, the file is not written.
func AtomicWriter(p string, w func(io.Writer) error, opts ...AtomicWriteOpt) error {
	writer, err := NewAtomicWriter(p, opts...)
	if err != nil {
		return err
	}
	err = w(writer)
	if err != nil {
		return writer.Fail(err)
	} else {
		return writer.Close()
	}
}

func syncDir(dirname string) error {
	dir, err := os.Open(dirname)
	if err != nil {
		return err
	}

	if err := dir.Sync(); err != nil {
		return err
	}

	return dir.Close()
}
