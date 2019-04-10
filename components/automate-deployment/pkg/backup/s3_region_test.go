package backup

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestMatchesUSEast1(t *testing.T) {
	for _, endpoint := range []string{
		"s3.amazonaws.com",
		"s3.us-east-1.amazonaws.com",
		"s3-external-1.amazonaws.com",
		"s3.dualstack.us-east-1.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "us-east-1", region)
	}
}

func TestMatchesUSEast2(t *testing.T) {
	for _, endpoint := range []string{
		"s3.us-east-2.amazonaws.com",
		"s3-us-east-2.amazonaws.com",
		"s3.dualstack.us-east-2.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "us-east-2", region)
	}
}

func TestMatchesUSWest1(t *testing.T) {
	for _, endpoint := range []string{
		"s3.us-west-1.amazonaws.com",
		"s3-us-west-1.amazonaws.com",
		"s3.dualstack.us-west-1.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "us-west-1", region)
	}
}

func TestMatchesUSWest2(t *testing.T) {
	for _, endpoint := range []string{
		"s3.us-west-2.amazonaws.com",
		"s3-us-west-2.amazonaws.com",
		"s3.dualstack.us-west-2.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "us-west-2", region)
	}
}

func TestMatchesCACentral1(t *testing.T) {
	for _, endpoint := range []string{
		"s3.ca-central-1.amazonaws.com",
		"s3-ca-central-1.amazonaws.com",
		"s3.dualstack.ca-central-1.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "ca-central-1", region)
	}
}

func TestMatchesAPSouth1(t *testing.T) {
	for _, endpoint := range []string{
		"s3.ap-south-1.amazonaws.com",
		"s3-ap-south-1.amazonaws.com",
		"s3.dualstack.ap-south-1.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "ap-south-1", region)
	}
}

func TestMatchesAPNortheast1(t *testing.T) {
	for _, endpoint := range []string{
		"s3.ap-northeast-1.amazonaws.com",
		"s3-ap-northeast-1.amazonaws.com",
		"s3.dualstack.ap-northeast-1.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "ap-northeast-1", region)
	}
}

func TestMatchesAPNortheast2(t *testing.T) {
	for _, endpoint := range []string{
		"s3.ap-northeast-2.amazonaws.com",
		"s3-ap-northeast-2.amazonaws.com",
		"s3.dualstack.ap-northeast-2.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "ap-northeast-2", region)
	}
}

func TestMatchesAPSoutheast1(t *testing.T) {
	for _, endpoint := range []string{
		"s3.ap-southeast-1.amazonaws.com",
		"s3-ap-southeast-1.amazonaws.com",
		"s3.dualstack.ap-southeast-1.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "ap-southeast-1", region)
	}
}

func TestMatchesAPSoutheast2(t *testing.T) {
	for _, endpoint := range []string{
		"s3.ap-southeast-2.amazonaws.com",
		"s3-ap-southeast-2.amazonaws.com",
		"s3.dualstack.ap-southeast-2.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "ap-southeast-2", region)
	}
}

func TestMatchesEUCentral1(t *testing.T) {
	for _, endpoint := range []string{
		"s3.eu-central-1.amazonaws.com",
		"s3-eu-central-1.amazonaws.com",
		"s3.dualstack.eu-central-1.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "eu-central-1", region)
	}
}

func TestMatchesEUWest1(t *testing.T) {
	for _, endpoint := range []string{
		"s3.eu-west-1.amazonaws.com",
		"s3-eu-west-1.amazonaws.com",
		"s3.dualstack.eu-west-1.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "eu-west-1", region)
	}
}

func TestMatchesEUWest2(t *testing.T) {
	for _, endpoint := range []string{
		"s3.eu-west-2.amazonaws.com",
		"s3-eu-west-2.amazonaws.com",
		"s3.dualstack.eu-west-2.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "eu-west-2", region)
	}
}

func TestMatchesEUWest3(t *testing.T) {
	for _, endpoint := range []string{
		"s3.eu-west-3.amazonaws.com",
		"s3-eu-west-3.amazonaws.com",
		"s3.dualstack.eu-west-3.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "eu-west-3", region)
	}
}

func TestMatchesSAEast1(t *testing.T) {
	for _, endpoint := range []string{
		"s3.sa-east-1.amazonaws.com",
		"s3-sa-east-1.amazonaws.com",
		"s3.dualstack.sa-east-1.amazonaws.com",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "sa-east-1", region)
	}
}

func TestNoMatch(t *testing.T) {
	for _, endpoint := range []string{
		"blahamazonaws.com",
		"127.0.0.1:10111",
	} {
		region := s3EndpointRegion(endpoint)
		require.Equal(t, "", region)
	}
}
