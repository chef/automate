package server

import (
	"context"
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/lib/grpc/auth_context"
)

func TestGetUserValFromCtx(t *testing.T) {
	ctx := context.Background()

	subs := []string{"something_else", "user:local:alice"}
	ctx = auth_context.NewContext(ctx, subs, []string{}, "", "")
	assert.Equal(t, "alice", getUserValFromCtx(ctx))

	subs = []string{"something_else", "user:saml:alice"}
	ctx = auth_context.NewContext(ctx, subs, []string{}, "", "")
	assert.Equal(t, "alice", getUserValFromCtx(ctx))

	subs = []string{"something_else", "not:right"}
	ctx = auth_context.NewContext(ctx, subs, []string{}, "", "")
	assert.Equal(t, "", getUserValFromCtx(ctx))
}

func TestValidateProfilesFormat(t *testing.T) {
	err := validateProfilesFormat([]string{"compliance://admin/linux-baseline#2.2.2"})
	assert.Equal(t, nil, err)

	err = validateProfilesFormat([]string{"compliance://admin/linux-baseline"})
	assert.Equal(t, status.Error(codes.InvalidArgument, fmt.Sprintf("Invalid job: profile version must be specified. Profile url provided: compliance://admin/linux-baseline")), err)

	err = validateProfilesFormat([]string{"compliance://linux-baseline#2.2.2"})
	assert.Equal(t, status.Error(codes.InvalidArgument, fmt.Sprintf("Invalid job: profile owner must be specified. Profile url provided: compliance://linux-baseline#2.2.2")), err)

	err = validateProfilesFormat([]string{"compliance://admin/linux-baseline#2.2.2", "compliance://admin/linux-baseline", "https://github.com/dev-sec/linux-baseline/archive/master.tar.gz"})
	assert.Equal(t, status.Error(codes.InvalidArgument, fmt.Sprintf("Invalid job: profile version must be specified. Profile url provided: compliance://admin/linux-baseline")), err)
}
