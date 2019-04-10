//
//  Author:: Kyleen MacGugan <kmacgugan@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"strings"
	"testing"
	"time"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/stretchr/testify/assert"
)

func TestGetPolicyCookbooksEmptyRequestReturnsError(t *testing.T) {
	ctx := context.Background()
	req := new(request.PolicyRevision)
	// Expect an empty response
	expected := new(response.PolicyCookbooks)

	res, err := cfgmgmt.GetPolicyCookbooks(ctx, req)
	if assert.NotNil(t, err) {
		assert.Equal(t, status.Errorf(codes.InvalidArgument, "Parameter revision_id not provided"), err)
	}
	assert.Equal(t, expected, res)
}

func TestGetPolicyCookbooksWhenNoActionFoundReturnsError(t *testing.T) {
	ctx := context.Background()
	req := &request.PolicyRevision{RevisionId: "nope"}
	// Expect an empty response
	expected := new(response.PolicyCookbooks)

	res, err := cfgmgmt.GetPolicyCookbooks(ctx, req)

	// Error should not be nil
	if assert.NotNil(t, err) {
		assert.Equal(t, status.Errorf(codes.NotFound, `type=ActionNotFound msg="No policy action found for revision ID: nope"`), err)
	}
	assert.Equal(t, expected, res)
}
func TestPolicyCookbooksWithOneAction(t *testing.T) {
	revisionId := "6c215da3266a20fd7a56ae9f1e3073e47c124f713a0e1eb74619a035325cd482"
	recordedAt, _ := time.Parse(time.RFC3339, "2017-11-09T00:23:29Z")
	data := `{
		"revision_id": "6c215da3266a20fd7a56ae9f1e3073e47c124f713a0e1eb74619a035325cd482",
		"name": "cd-infrastructure-base",
		"run_list": [
			"recipe[chefops-base::default]",
			"recipe[audit::default]"
		],
		"cookbook_locks": {
			"chefops-base": {
				"version": "0.1.38",
				"identifier": "9b2832d8802559fd9611ae1e9bc6ee20bf258a40",
				"dotted_decimal_identifier": "43672820235904345.71378171787647942.261824413272640",
				"cache_key": "chefops-base-0.1.38-ops-supermarket.chef.co",
				"origin": "https://ops-supermarket.chef.co:443/api/v1/cookbooks/chefops-base/versions/0.1.38/download",
				"source_options": {
					"artifactserver": "https://ops-supermarket.chef.co:443/api/v1/cookbooks/chefops-base/versions/0.1.38/download",
					"version": "0.1.38"
				}
			},
			"audit": {
				"version": "5.0.3",
				"identifier": "9778186fc1326e8901c3f37942d2cecc5d1d04f0",
				"dotted_decimal_identifier": "42634767832789614.38564012924420818.227377130833136",
				"cache_key": "audit-5.0.3-supermarket.chef.io",
				"origin": "https://supermarket.chef.io:443/api/v1/cookbooks/audit/versions/5.0.3/download",
				"source_options": {
					"artifactserver": "https://supermarket.chef.io:443/api/v1/cookbooks/audit/versions/5.0.3/download",
					"version": "5.0.3"
				}
			}
		}
	}`

	data = strings.Replace(data, "\n", "", -1)
	data = strings.Replace(data, "\t", "", -1)
	action := []iBackend.InternalChefAction{
		iBackend.InternalChefAction{
			RecordedAt: recordedAt,
			RevisionId: revisionId,
			Data:       data,
		},
	}

	suite.IngestActions(action)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	req := &request.PolicyRevision{RevisionId: revisionId}
	expected := &response.PolicyCookbooks{
		PolicyName: "cd-infrastructure-base",
		CookbookLocks: []*response.CookbookLock{
			&response.CookbookLock{
				Cookbook:         "chefops-base",
				PolicyIdentifier: "9b2832d8802559fd9611ae1e9bc6ee20bf258a40",
			},
			&response.CookbookLock{
				Cookbook:         "audit",
				PolicyIdentifier: "9778186fc1326e8901c3f37942d2cecc5d1d04f0",
			},
		},
	}

	res, err := cfgmgmt.GetPolicyCookbooks(ctx, req)

	assert.Nil(t, err)
	assert.Equal(t, expected.PolicyName, res.PolicyName)
	// Verify that they contains all the expected CookbookLocks
	// We don't do 'assert.Equal()' because that checks order
	assert.ElementsMatch(t, res.CookbookLocks, expected.CookbookLocks)
}
