package manifest

import "github.com/pkg/errors"

func NewInvalidSchemaError(err error) error {
	return &InvalidSchemaError{err}
}

type InvalidSchemaError struct {
	err error
}

func (e *InvalidSchemaError) Error() string {
	return errors.Wrap(e.err, "unknown schema").Error()
}

type CannotParseError struct {
	err error
}

func NewCannotParseError(err error) error {
	return &CannotParseError{err}
}

func (e *CannotParseError) Error() string {
	return errors.Wrap(e.err, "failed to parse manifest").Error()
}

type NoSuchManifestError struct {
	err error
}

func NewNoSuchManifestError(err error) error {
	return &NoSuchManifestError{err}
}

func (e *NoSuchManifestError) Error() string {
	return errors.Wrap(e.err, "failed to locate manifest").Error()
}
