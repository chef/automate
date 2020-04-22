package server

import (
	"context"

	"github.com/chef/automate/api/interservice/authz/common"
	api "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/lib/logger"
)

type subjectPurger interface {
	PurgeSubjectFromPolicies(context.Context,
		*api.PurgeSubjectFromPoliciesReq) (*api.PurgeSubjectFromPoliciesResp, error)
}

type purger struct {
	log logger.Logger
	sp  subjectPurger
}

func NewSubjectPurgeServer(_ context.Context,
	l logger.Logger,
	sp subjectPurger,
) (common.SubjectPurgeServer, error) {
	return &purger{log: l, sp: sp}, nil
}

func (p *purger) PurgeSubjectFromPolicies(ctx context.Context,
	req *common.PurgeSubjectFromPoliciesReq) (*common.PurgeSubjectFromPoliciesResp, error) {

	resp, err := p.sp.PurgeSubjectFromPolicies(ctx, (*api.PurgeSubjectFromPoliciesReq)(req))
	if err != nil {
		return nil, err
	}
	return &common.PurgeSubjectFromPoliciesResp{
		PoliciesV2: resp.Ids,
	}, nil
}
