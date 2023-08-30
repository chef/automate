package gcputils_test

import (
	"context"
	"testing"

	"cloud.google.com/go/storage"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/gcputils"
	"github.com/stretchr/testify/assert"
)

func TestGCPUtilsImpl_NewSessionWithOptions(t *testing.T) {
	gcpu := gcputils.GCPUtilsImpl{}
	ctx := context.Background()
	cred := &models.GcpServiceAccount{}
	result, err := gcpu.NewSessionWithOptions(ctx, cred)

	assert.Nil(t, result)
	assert.Error(t, err)
}

func TestGCPUtilsImpl_DeleteObject(t *testing.T) {
	gcpu := gcputils.GCPUtilsImpl{}
	ctx := context.Background()

	oh := &storage.ObjectHandle{}
	err := gcpu.DeleteObject(ctx, oh)

	assert.Error(t, err)
}
