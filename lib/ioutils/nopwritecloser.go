package ioutils

import "io"

type NopWriteCloser struct {
	io.Writer
}

func NewNopWriteCloser(w io.Writer) *NopWriteCloser {
	return &NopWriteCloser{w}
}

func (wc *NopWriteCloser) Write(p []byte) (int, error) { return wc.Writer.Write(p) }
func (wc *NopWriteCloser) Close() error                { return nil }
