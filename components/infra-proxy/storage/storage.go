package storage

import (
	"context"
	"errors"
	"time"

	"github.com/chef/automate/lib/stringutils"
	uuid "github.com/chef/automate/lib/uuid4"
)

// Storage is the interface provided by our various storage backends.
type Storage interface {
	GetServers(context.Context) ([]Server, error)
}

// Resetter is, if exposed, used for tests to reset the storage backend to a
// pristine state.
type Resetter interface {
	Reset(context.Context) error
}

// Server is the struct ingested and returned by our backend implementations.
type Server struct {
	ID          uuid.UUID
	Name        string
	Fqdn        string
	IpAddress   string
	CreatedAt   time.Time
	UpdatedAt   time.Time
}

// Errors returned from the backend
var (
	// ErrNotFound is returned when a requested server wasn't found
	ErrNotFound = errors.New("not found")

	// ErrConflict is returned when a server there is a clash of server IDs
	ErrConflict = errors.New("conflict")
)
