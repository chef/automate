package types

import (
	"context"
	"time"

	"go.uber.org/zap"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/lib/tls/certs"
)

const LegacyTokenDescription = "Legacy data collector token ported from A1"

// Token represents a token we are able to use to authenticate: the ID is arbitrary,
// the Token will be checked for in incoming requests.
type Token struct {
	ID       string
	Name     string
	Value    string
	Active   bool
	Created  time.Time
	Updated  time.Time
	Projects []string
}

// Storage is the interface for various adapters
type Storage interface {
	CreateToken(ctx context.Context, id, name string, active bool, projects []string) (*Token, error)
	CreateTokenWithValue(ctx context.Context, id, value, name string,
		active bool, projects []string) (*Token, error)
	CreateLegacyTokenWithValue(ctx context.Context, value string) (*Token, error)
	DeleteToken(context.Context, string) error
	UpdateToken(ctx context.Context, id, name string, active bool, projects []string) (*Token, error)
	GetToken(context.Context, string) (*Token, error)
	GetTokenIDWithValue(ctx context.Context, value string) (string, error)
	GetTokens(context.Context) ([]*Token, error)
	PurgeProject(context.Context, string) error
}

// Resetter allows resetting the adapter to factory settings (e.g. deletes all
// data) -- use with caution
type Resetter interface {
	Reset(context.Context) error
}

// ProjectValidator is a more limited interface for authz-service
type ProjectValidator interface {
	ValidateProjectAssignment(context.Context, *authz.ValidateProjectAssignmentReq, ...grpc.CallOption) (*authz.ValidateProjectAssignmentResp, error)
}

// TokenConfig is a configuration that can open a storage adapter
type TokenConfig interface {
	Open(*certs.ServiceCerts, *zap.Logger, ProjectValidator) (Storage, error)
}

// NotFoundError is the error returned when the token wasn't found, to discern this
// from other errors -- failures to reach the backend services, etc.
type NotFoundError struct{}

func (e *NotFoundError) Error() string { return "No token record found" }

// ConflictError is the error returned when the token creation request specifies
// an ID that has already been used
type ConflictError struct{}

func (e *ConflictError) Error() string { return "Conflicting token ID" }
