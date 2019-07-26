package password

import (
	"fmt"

	"github.com/muesli/crunchy"
)

// Validator is a wrapper type for our password validation
type Validator struct {
	opts crunchy.Options
	cr   *crunchy.Validator
}

// EmptyError is returned if the validated password is empty or all whitespace.
type EmptyError struct{}

func (*EmptyError) Error() string { return "password is empty or all whitespace" }

// TooShortError is returned if the password is shorter than the required number
// of chars.
type TooShortError struct {
	min int
}

func (e *TooShortError) Error() string {
	return fmt.Sprintf("password is too short (must be at least %d characters)", e.min)
}

// TooFewCharsError is returned if the password has less than the required number
// of distinct characters.
type TooFewCharsError struct {
	min int
}

func (e *TooFewCharsError) Error() string {
	return fmt.Sprintf("password does not contain enough distinct characters (minimum %d)", e.min)
}

// NewValidator instantiates a password.Validator struct
func NewValidator() (*Validator, error) {
	opts := crunchy.Options{
		MinDiff:        3, // distinct characters
		MinLength:      8,
		DictionaryPath: "/dev/null",
	}
	return &Validator{opts: opts, cr: crunchy.NewValidatorWithOpts(opts)}, nil
}

// Validate returns a descriptive error if the passed password fails to meet our
// password policy.
func (v *Validator) Validate(pass string) error {
	return v.translateError(v.cr.Check(pass))
}

// Note 2017/12/21 (sr): crunchy.ErrHashedDictionary cannot happen in our case,
//                         since we don't set up hash functions to check.
func (v *Validator) translateError(err error) error {
	// unwrap dictionary errors (ignore word and actual distance)
	if underlyingErr, ok := err.(*crunchy.DictionaryError); ok {
		err = underlyingErr.Err
	}

	switch err {
	// These are ignored:
	case crunchy.ErrTooSystematic,
		crunchy.ErrDictionary,
		crunchy.ErrMangledDictionary:
		return nil

	case crunchy.ErrEmpty:
		return &EmptyError{}
	case crunchy.ErrTooShort:
		return &TooShortError{min: v.opts.MinLength}
	case crunchy.ErrTooFewChars:
		return &TooFewCharsError{min: v.opts.MinDiff}
	default: // includes err == nil
		return err
	}
}
