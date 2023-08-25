package gcputils_test

import (
	"testing"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/client"
	"github.com/aws/aws-sdk-go/aws/client/metadata"
	"github.com/aws/aws-sdk-go/aws/request"
	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/awsutils"
	"github.com/stretchr/testify/assert"
)

func TestNewSessionWithOptions(t *testing.T) {
	au := awsutils.NewAwsUtils()
	_, err := au.NewSessionWithOptions("", "", "", "")
	assert.NoError(t, err)
}

func TestDeleteObject(t *testing.T) {
	au := awsutils.NewAwsUtils()
	_, err := au.DeleteObject(&s3.S3{
		Client: client.New(
			aws.Config{},
			metadata.ClientInfo{},
			request.Handlers{},
		),
	}, "", "")
	assert.NoError(t, err)
}

func TestListObjectsV2(t *testing.T) {
	au := awsutils.NewAwsUtils()
	_, err := au.ListObjectsV2(&s3.S3{
		Client: client.New(
			aws.Config{},
			metadata.ClientInfo{},
			request.Handlers{},
		),
	}, "", "")
	assert.NoError(t, err)
}

func TestListBuckets(t *testing.T) {
	au := awsutils.NewAwsUtils()
	_, err := au.ListBuckets(&s3.S3{
		Client: client.New(
			aws.Config{},
			metadata.ClientInfo{},
			request.Handlers{},
		),
	})
	assert.NoError(t, err)
}
