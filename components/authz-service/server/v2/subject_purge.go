package v2

import (
	"context"

	api_v1 "github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/authz/common"
	api_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/lib/logger"
)

type v1SubjectPurger interface {
	PurgeSubjectFromPolicies(context.Context,
		*api_v1.PurgeSubjectFromPoliciesReq) (*api_v1.PurgeSubjectFromPoliciesResp, error)
}

type v2SubjectPurger interface {
	PurgeSubjectFromPolicies(context.Context,
		*api_v2.PurgeSubjectFromPoliciesReq) (*api_v2.PurgeSubjectFromPoliciesResp, error)
}

type purger struct {
	log logger.Logger
	v1  v1SubjectPurger
	v2  v2SubjectPurger
}

func NewSubjectPurgeServer(_ context.Context,
	l logger.Logger,
	v1 v1SubjectPurger,
	v2 v2SubjectPurger,
) (common.SubjectPurgeServer, error) {
	return &purger{log: l, v1: v1, v2: v2}, nil
}

func (p *purger) PurgeSubjectFromPolicies(ctx context.Context,
	req *common.PurgeSubjectFromPoliciesReq) (*common.PurgeSubjectFromPoliciesResp, error) {

	v1Resp, err := p.v1.PurgeSubjectFromPolicies(ctx, (*api_v1.PurgeSubjectFromPoliciesReq)(req))
	if err != nil {
		return nil, err
	}
	v2Resp, err := p.v2.PurgeSubjectFromPolicies(ctx, (*api_v2.PurgeSubjectFromPoliciesReq)(req))
	if err != nil {
		return nil, err
	}
	return &common.PurgeSubjectFromPoliciesResp{
		PoliciesV1: v1Resp.Ids,
		PoliciesV2: v2Resp.Ids,
	}, nil
}
