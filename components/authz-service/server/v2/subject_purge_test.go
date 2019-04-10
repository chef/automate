package v2_test

import (
	"context"
	"fmt"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"
	"github.com/stretchr/testify/assert"

	api_v1 "github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/authz/common"
	api_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/authz-service/prng"
	v2 "github.com/chef/automate/components/authz-service/server/v2"
	"github.com/chef/automate/lib/logger"
)

type vee1 struct {
	expected string
	returned []string
}
type vee2 struct {
	expected string
	returned []string
}

func (v *vee1) PurgeSubjectFromPolicies(_ context.Context,
	req *api_v1.PurgeSubjectFromPoliciesReq) (*api_v1.PurgeSubjectFromPoliciesResp, error) {
	if req.Subject != v.expected {
		return nil, fmt.Errorf("v1: unexpected argument: %q (expected %q)", req.Subject, v.expected)
	}
	return &api_v1.PurgeSubjectFromPoliciesResp{Ids: v.returned}, nil
}

func (v *vee2) PurgeSubjectFromPolicies(_ context.Context,
	req *api_v2.PurgeSubjectFromPoliciesReq) (*api_v2.PurgeSubjectFromPoliciesResp, error) {
	if req.Subject != v.expected {
		return nil, fmt.Errorf("v2: unexpected argument: %q (expected %q)", req.Subject, v.expected)
	}
	return &api_v2.PurgeSubjectFromPoliciesResp{Ids: v.returned}, nil
}

func TestCommonSubjectPurgeService(t *testing.T) {
	parameters := gopter.DefaultTestParametersWithSeed(prng.GenSeed(t))
	properties := gopter.NewProperties(parameters)

	ctx := context.Background()
	l := logger.NewTestLogger()

	properties.Property("calls both with subject, returns output", prop.ForAll(
		func(sub string, v1Pols, v2Pols []string) bool {
			s, err := v2.NewSubjectPurgeServer(ctx, l,
				&vee1{expected: sub, returned: v1Pols},
				&vee2{expected: sub, returned: v2Pols})
			if err != nil {
				return false
			}
			resp, err := s.PurgeSubjectFromPolicies(ctx,
				&common.PurgeSubjectFromPoliciesReq{Subject: sub})
			if err != nil {
				return false
			}
			return assert.ElementsMatch(t, v1Pols, resp.PoliciesV1) &&
				assert.ElementsMatch(t, v2Pols, resp.PoliciesV2)
		},
		gen.Identifier(),
		gen.SliceOf(gen.Identifier()),
		gen.SliceOf(gen.Identifier()),
	))

	properties.TestingRun(t)
}
