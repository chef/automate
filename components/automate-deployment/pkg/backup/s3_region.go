package backup

import (
	"context"
	"strings"

	"github.com/aws/aws-sdk-go/aws/endpoints"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3/s3manager"
	"github.com/pkg/errors"
)

func s3EndpointRegion(endpoint string) string {
	if !strings.HasSuffix(endpoint, ".amazonaws.com") {
		return ""
	}

	resolver := endpoints.DefaultResolver()
	partitions := resolver.(endpoints.EnumPartitions).Partitions()

	for _, p := range partitions {
		for id := range p.Regions() {
			if strings.Contains(endpoint, id+".amazonaws.com") {
				return id
			}
		}
	}

	if strings.Contains(endpoint, "s3.amazonaws.com") || strings.Contains(endpoint, "s3-external-1.amazonaws.com") {
		return "us-east-1"
	}

	return ""
}

func s3BucketLocation(ctx context.Context, bucketName string) (string, error) {
	sess, err := session.NewSession()
	if err != nil {
		return "", errors.Wrap(err, "could not determine bucket region")
	}

	// I have no idea why you need to specify a region hint to get the region. If
	// I knew the region, I wouldn't be asking. But that is the way it is.
	region, err := s3manager.GetBucketRegion(ctx, sess, bucketName, "us-east-1")
	if err != nil {
		return "", errors.Wrap(err, "could not determine bucket region")
	}

	return region, nil
}
