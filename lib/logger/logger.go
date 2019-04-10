// Package logger is intended to abstract our concrete logging backend, to not
// litter all the code base with "sirupsen/logrus" imports; also makes it easier
// to eventually switch out the logging library.
package logger

import (
	"fmt"
	"io"
	"io/ioutil"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

// Logger is the interface provided by our logger abstraction
type Logger interface {
	// grpclog uses this to determine log levels
	V(i int) bool

	// NewEntry returns a new logrus.Entry to be used with grpc_logrus middleware
	NewEntry() *logrus.Entry

	logrus.FieldLogger
}

// KV is the type for field literals. These are key/value pairs, hence
// "KV". We declare it as an alias to the logrus.Fields type which is
// a type defined as map[string]interface{}.
type KV = logrus.Fields

// NewLogger initializes a new logger with the passed format and level
func NewLogger(format, level string) (Logger, error) {
	l := logrus.New()

	// TODO: log format (timestamps?)
	// TODO: redirect stdlog
	err := configureLogrusLogger(l, format, level)
	if err != nil {
		return nil, err
	}
	return &wrap{Logger: l}, nil
}

func NewLoggerWithOut(format, level string, out io.Writer) (Logger, error) {
	l := logrus.New()
	err := configureLogrusLogger(l, format, level)
	if err != nil {
		return nil, err
	}
	if out != nil {
		l.Out = out
	}
	return &wrap{Logger: l}, nil
}

func (l *wrap) NewEntry() *logrus.Entry {
	return logrus.NewEntry(l.Logger)
}

func configureLogrusLogger(l *logrus.Logger, format, level string) error {
	lvl, err := logrus.ParseLevel(level)
	if err != nil {
		return errors.Wrap(err, "parse log level")
	}
	l.SetLevel(lvl)

	switch format {
	case "text":
		// it's the default, do nothing
	case "json":
		l.Formatter = &logrus.JSONFormatter{}
	default:
		return fmt.Errorf("unknown log format %v", format)
	}

	return nil
}

// NewTestLogger returns a logger that discards all output -- to keep test
// output undisturbed.
func NewTestLogger() Logger {
	l := logrus.New()
	l.Out = ioutil.Discard
	return &wrap{Logger: l}
}

type wrap struct {
	*logrus.Logger
}

// V returns whether the verbosity-level passed is something we want to see.
// GRPC knows multiple verbosity levels, and for example the transport
// package always sends logs at level 2.
// This allows passing a logger.Logger into grpclog.SetLoggerV2()
func (w *wrap) V(i int) bool {
	// accept anything if we're on debug, only <= 1 otherwise
	return w.Level == logrus.DebugLevel || i <= 1
}
