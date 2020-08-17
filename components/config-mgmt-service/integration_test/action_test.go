//
//  Author:: Kyleen MacGugan <kmacgugan@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"testing"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/gofrs/uuid"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
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

	_, err := cfgmgmt.GetPolicyCookbooks(ctx, req)

	assert.Error(t, err)
}
func TestPolicyCookbooksWithOneAction(t *testing.T) {
	inputReq := &request.PolicyUpdateAction{
		PolicyName:         "cd-infrastructure-base",
		PolicyGroup:        "none",
		ChefServerFqdn:     "chef.example.org",
		ChefServerOrgname:  "org1",
		ChefServerUsername: "bob",
		PolicyRevisionId:   uuid.Must(uuid.NewV4()).String(),
		Cookbooks: []*request.PolicyCookbookLock{
			{
				CookbookName: "chefops-base",
				PolicyId:     uuid.Must(uuid.NewV4()).String(),
			},
			{
				CookbookName: "audit",
				PolicyId:     uuid.Must(uuid.NewV4()).String(),
			},
		},
	}

	ctx := context.Background()

	_, err := cfgmgmt.HandlePolicyUpdateAction(ctx, inputReq)
	require.NoError(t, err)

	expected := &response.PolicyCookbooks{
		PolicyName: inputReq.PolicyName,
		CookbookLocks: []*response.CookbookLock{
			{
				Cookbook:         inputReq.Cookbooks[0].CookbookName,
				PolicyIdentifier: inputReq.Cookbooks[0].PolicyId,
			},
			{
				Cookbook:         inputReq.Cookbooks[1].CookbookName,
				PolicyIdentifier: inputReq.Cookbooks[1].PolicyId,
			},
		},
	}

	res, err := cfgmgmt.GetPolicyCookbooks(ctx, &request.PolicyRevision{RevisionId: inputReq.PolicyRevisionId})

	require.NoError(t, err)
	assert.Equal(t, expected.PolicyName, res.PolicyName)
	// Verify that they contains all the expected CookbookLocks
	// We don't do 'assert.Equal()' because that checks order
	assert.ElementsMatch(t, res.CookbookLocks, expected.CookbookLocks)
}
