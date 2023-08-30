package gcputils_test

import (
	"context"
	"testing"

	"cloud.google.com/go/storage"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/gcputils"
	"github.com/stretchr/testify/assert"
)

func TestDeleteObject(t *testing.T) {
	au := gcputils.NewGCPUtils()
	client, err := storage.NewClient(context.Background())
	res, err := au.DeleteObject(client, "", "")
	assert.NoError(t, err)
	assert.Nil(t, res)
}

func TestListObjectsV2(t *testing.T) {
	au := gcputils.NewGCPUtils()
	client, err := storage.NewClient(context.Background())
	res, err := au.ListObjectsV2(client, "")
	assert.NoError(t, err)
	assert.Nil(t, res)
}

func TestListBuckets(t *testing.T) {
	au := gcputils.NewGCPUtils()
	client, err := storage.NewClient(context.Background())
	res, err := au.ListBuckets(client)
	assert.NoError(t, err)
	assert.Nil(t, res)
}

func TestNewSessionWithOptions(t *testing.T) {
	au := gcputils.NewGCPUtils()
	cl, err := au.NewSessionWithOptions(context.Background(), nil)
	assert.Error(t, err)
	assert.Nil(t, cl)
}
