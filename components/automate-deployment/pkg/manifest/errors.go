package manifest

import "github.com/pkg/errors"

func NewErrInvalidSchema(err error) error {
	return &ErrInvalidSchema{err}
}

type ErrInvalidSchema struct {
	err error
}

func (e *ErrInvalidSchema) Error() string {
	return errors.Wrap(e.err, "unknown schema").Error()
}

type ErrCannotParse struct {
	err error
}

func NewErrCannotParse(err error) error {
	return &ErrCannotParse{err}
}

func (e *ErrCannotParse) Error() string {
	return errors.Wrap(e.err, "failed to parse manifest").Error()
}

type ErrNoSuchManifest struct {
	err error
}

func NewErrNoSuchManifest(err error) error {
	return &ErrNoSuchManifest{err}
}

func (e *ErrNoSuchManifest) Error() string {
	return errors.Wrap(e.err, "failed to locate manifest").Error()
}
