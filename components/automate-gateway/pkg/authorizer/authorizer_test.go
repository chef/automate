package authorizer_test

import (
	"context"
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authz/common"
	"github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/automate-gateway/api/authz/pairs"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
	"github.com/chef/automate/components/automate-gateway/pkg/authorizer"
)

func TestAuthorizerHandle(t *testing.T) {
	ctx := context.Background()
	args := func() (context.Context, []string, []string, interface{}, v2.Version_VersionNumber) {
		return ctx, []string{"user:local:admin"}, []string{"project1"}, nil, v2.Version_V0
	}

	t.Run("if first succeeds, doesn't call second", func(t *testing.T) {
		a := authorizer.NewAuthorizer(newSuccessAuthorizer(t), newFailureAuthorizer(t))
		_, err := a.Handle(args())
		require.NoError(t, err)
	})

	t.Run("if first fails with some error, doesn't call second", func(t *testing.T) {
		a := authorizer.NewAuthorizer(newFailureAuthorizer(t), newSuccessAuthorizer(t))
		_, err := a.Handle(args())
		require.Error(t, err)
	})

	t.Run("if first fails with some FailedPrecondition, doesn't call second", func(t *testing.T) {
		st := status.New(codes.FailedPrecondition, "hm something is always missing")
		a := authorizer.NewAuthorizer(newFailureAuthorizer(t, st), newSuccessAuthorizer(t))
		_, err := a.Handle(args())
		require.Error(t, err)
	})

	t.Run("if first fails with proper \"use v2\" FailedPrecondition details, calls second", func(t *testing.T) {
		st := shouldUseV2Status(t)
		a := authorizer.NewAuthorizer(newFailureAuthorizer(t, st), newSuccessAuthorizer(t))
		_, err := a.Handle(args())
		require.NoError(t, err)
	})

	t.Run("if first fails with proper \"use v2\" FailedPrecondition details, calls second, if that fails, returns error", func(t *testing.T) {
		st := shouldUseV2Status(t)
		a := authorizer.NewAuthorizer(newFailureAuthorizer(t, st), newFailureAuthorizer(t))
		_, err := a.Handle(args())
		require.Error(t, err)
	})
	// TODO add cases for
	// v1 -> v2.1
	// v2 -> v2.1
	// v2.1 -> v2
	// v2.1 -> v1
}

func TestAuthorizerIsAuthorized(t *testing.T) {
	ctx := context.Background()
	args := func() (context.Context, []string, string, string, string, string) {
		return ctx, []string{"user:local:admin"}, "resource:v1", "v1action", "svc:type:v2resource", "svc:type:v2action"
	}

	t.Run("if first succeeds, doesn't call second", func(t *testing.T) {
		a := authorizer.NewAuthorizer(newSuccessAuthorizer(t), newFailureAuthorizer(t))
		r, err := a.IsAuthorized(args())
		require.NoError(t, err)
		assert.NoError(t, r.Err())
	})

	t.Run("if first fails in unexpected way, doesn't call second", func(t *testing.T) {
		a := authorizer.NewAuthorizer(newFailureAuthorizer(t), newSuccessAuthorizer(t))
		_, err := a.IsAuthorized(args())
		require.Error(t, err)
	})

	t.Run("if first fails with some FailedPrecondition, doesn't call second", func(t *testing.T) {
		st := status.New(codes.FailedPrecondition, "hm something is always missing")
		a := authorizer.NewAuthorizer(newFailureAuthorizer(t, st), newSuccessAuthorizer(t))
		_, err := a.IsAuthorized(args())
		require.Error(t, err)
	})

	t.Run("if first fails with proper \"use v2\" FailedPrecondition details, calls second", func(t *testing.T) {
		st := shouldUseV2Status(t)
		a := authorizer.NewAuthorizer(newFailureAuthorizer(t, st), newSuccessAuthorizer(t))
		_, err := a.IsAuthorized(args())
		require.NoError(t, err)
	})

	t.Run("if first fails with proper \"use v2\" FailedPrecondition details, calls second, if that fails, returns error", func(t *testing.T) {
		st := shouldUseV2Status(t)
		a := authorizer.NewAuthorizer(newFailureAuthorizer(t, st), newFailureAuthorizer(t))
		_, err := a.IsAuthorized(args())
		require.Error(t, err)
	})
}

func shouldUseV2Status(t *testing.T) *status.Status {
	t.Helper()
	st := status.New(codes.FailedPrecondition, "this string doesn't matter")
	st, err := st.WithDetails(&common.ErrorShouldUseV2{})
	require.NoError(t, err)
	return st
}

func newSuccessAuthorizer(t *testing.T) middleware.AuthorizationHandler {
	t.Helper()
	return &success{}
}

func newFailureAuthorizer(t *testing.T, st ...*status.Status) middleware.AuthorizationHandler {
	t.Helper()
	if len(st) == 1 {
		return &failure{Status: st[0]}
	}
	return &failure{Status: status.New(codes.Internal, "failed")}
}

type (
	success struct{}
	failure struct{ *status.Status }
)

func (*success) Handle(ctx context.Context, _ []string, _ []string, _ interface{}, _ v2.Version_VersionNumber) (context.Context, error) {
	return ctx, nil
}

func (s *success) IsAuthorized(context.Context, []string, string, string) (middleware.AuthorizationResponse, error) {
	return authzSuccess, nil
}

func (*success) FilterAuthorizedPairs(context.Context, []string, []*pairs.Pair) ([]*pairs.Pair, error) {
	return nil, errors.New("not implemented")
}

func (*success) FilterAuthorizedProjects(context.Context, []string, []*pairs.Pair) ([]string, error) {
	return nil, errors.New("not implemented")
}

func (f *failure) Handle(ctx context.Context, _ []string, _ []string, _ interface{}, _ v2.Version_VersionNumber) (context.Context, error) {
	return ctx, f.Err()
}

func (f *failure) IsAuthorized(context.Context, []string, string, string) (middleware.AuthorizationResponse, error) {
	return nil, f.Err()
}

func (*failure) FilterAuthorizedPairs(context.Context, []string, []*pairs.Pair) ([]*pairs.Pair, error) {
	return nil, errors.New("not implemented")
}

func (*failure) FilterAuthorizedProjects(context.Context, []string, []*pairs.Pair) ([]string, error) {
	return nil, errors.New("not implemented")
}

type authorized struct{}
type notAuthorized struct{}

var authzSuccess = &authorized{}
var authzFailure = &notAuthorized{}

func (*authorized) GetAuthorized() bool {
	return true
}

func (*notAuthorized) GetAuthorized() bool {
	return false
}
