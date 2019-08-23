package server

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/lib/grpc/auth_context"
)

func TestGetUserValFromCtx(t *testing.T) {
	ctx := context.Background()

	subs := []string{"something_else", "user:local:alice"}
	ctx = auth_context.NewContext(ctx, subs, []string{}, "", "", "")
	assert.Equal(t, "alice", getUserValFromCtx(ctx))

	subs = []string{"something_else", "user:saml:alice"}
	ctx = auth_context.NewContext(ctx, subs, []string{}, "", "", "")
	assert.Equal(t, "alice", getUserValFromCtx(ctx))

	subs = []string{"something_else", "not:right"}
	ctx = auth_context.NewContext(ctx, subs, []string{}, "", "", "")
	assert.Equal(t, "", getUserValFromCtx(ctx))
}
