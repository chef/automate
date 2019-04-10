// uuid4 wraps github.com/gofrs/uuid in a way that enforces version 4
package uuid4

import (
	"errors"

	"github.com/gofrs/uuid"
)

var ErrorNotVersion4 = errors.New("UUID is not version 4")

type UUID struct {
	uuid.UUID
}

func check(w uuid.UUID, err error) (UUID, error) {
	if err != nil {
		return UUID{}, err
	}
	if w.Version() != uuid.V4 {
		return UUID{}, ErrorNotVersion4
	}
	return UUID{UUID: w}, nil
}

func FromString(s string) (UUID, error) {
	return check(uuid.FromString(s))
}

func New() (UUID, error) {
	return check(uuid.NewV4())
}

// NewV4 is a convenience function, making this a nicer-fitting drop-in
// replacement.
func NewV4() (UUID, error) {
	return check(uuid.NewV4())
}

func Must(i UUID, err error) UUID {
	if err != nil {
		panic(err)
	}
	return i
}
