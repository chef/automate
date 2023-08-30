package gcputils_test

import (
	"context"
	"errors"
	"testing"

	"cloud.google.com/go/storage"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/gcputils"
	"github.com/stretchr/testify/assert"
)

func TestMockGCPUtils_NewSessionWithOptions(t *testing.T) {
	mock := &gcputils.MockGCPUtils{
		NewSessionWithOptionsFunc: func(ctx context.Context, gsa *models.GcpServiceAccount) (*storage.Client, error) {
			return nil, errors.New("mock err")
		},
	}

	ctx := context.Background()
	cred := &models.GcpServiceAccount{}
	result, err := mock.NewSessionWithOptions(ctx, cred)

	assert.Nil(t, result)
	assert.Error(t, err)
}

func TestMockGCPUtils_ListObjects(t *testing.T) {
	mock := &gcputils.MockGCPUtils{
		ListObjectsFunc: func(ctx context.Context, bucket *storage.BucketHandle, query *storage.Query) error {
			return nil
		},
	}
	err := mock.ListObjects(context.Background(), nil, nil)
	assert.NoError(t, err)
}

func TestMockGCPUtils_NewUploader(t *testing.T) {
	mock := &gcputils.MockGCPUtils{
		NewUploaderFunc: func(ctx context.Context, uploadObject *storage.ObjectHandle) (*models.Checks, error) {
			return &models.Checks{}, nil
		},
	}

	result, err := mock.NewUploader(context.Background(), nil)

	assert.NotNil(t, result)
	assert.NoError(t, err)
}
