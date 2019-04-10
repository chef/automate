package integration_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
)

func TestVersionReturnsInfo(t *testing.T) {
	ctx := context.Background()
	req := &request.VersionInfo{}
	expected := &response.VersionInfo{
		Name:    "config-mgmt-service",
		Version: "unknown",
		SHA:     "unknown",
		Built:   "unknown",
	}
	res, err := cfgmgmt.GetVersion(ctx, req)
	assert.Nil(t, err)
	assert.Equal(t, expected, res)
}
