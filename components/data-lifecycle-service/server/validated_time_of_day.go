package server

import (
	"bytes"
	"fmt"

	"github.com/pkg/errors"
)

// ValidatedTimeOfDay time is a valid time of day
type ValidatedTimeOfDay struct {
	hour int
	min  int
	sec  int
}

// DefaultTimeOfDay returns a ValidatedTimeOfDay with midnight
func DefaultTimeOfDay() *ValidatedTimeOfDay {
	return &ValidatedTimeOfDay{
		hour: 0,
		min:  0,
		sec:  0}
}

// UnmarshalText unmarshals a text in the format hh:mm:ss to a ValidatedTimeOfDay.
// Hour must be less than 24 and greater than 0
// Min must be less then 60 and greater than 0
// Sec must be less than 60 and greater than 0
// Failing any of the above validation criteria returns an error
func (tod *ValidatedTimeOfDay) UnmarshalText(text []byte) error {
	_, err := fmt.Fscanf(bytes.NewReader(text), "%2d:%2d:%2d", &tod.hour, &tod.min, &tod.sec)

	if err != nil {
		return errors.Wrapf(err, "Could not parse time of day")
	}

	return tod.validate()
}

func (tod *ValidatedTimeOfDay) validate() error {
	if tod.hour >= 24 || tod.hour < 0 {
		return fmt.Errorf("Invalid hour value %d", tod.hour)
	}

	if tod.min >= 60 || tod.min < 0 {
		return fmt.Errorf("Invalid minute value %d", tod.min)
	}

	if tod.sec >= 60 || tod.sec < 0 {
		return fmt.Errorf("Invalid second value %d", tod.sec)
	}

	return nil
}

// Hour returns the hour
func (tod *ValidatedTimeOfDay) Hour() int {
	return tod.hour
}

// Min returns the minute
func (tod *ValidatedTimeOfDay) Min() int {
	return tod.min
}

// Sec returns the second
func (tod *ValidatedTimeOfDay) Sec() int {
	return tod.sec
}
