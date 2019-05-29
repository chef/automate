package rules

import (
	"context"
	"fmt"
	"sync"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	authz "github.com/chef/automate/api/interservice/authz/v2"
	pb_common "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/common"
	pb_req "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/request"
	pb_resp "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/response"
)

type Server struct {
	projects authz.ProjectsClient
}

func NewServer(projects authz.ProjectsClient) *Server {
	return &Server{
		projects: projects,
	}
}

func (s *Server) CreateRule(ctx context.Context, req *pb_req.CreateRuleReq) (*pb_resp.CreateRuleResp, error) {
	internalReq, err := fromExternal(req)
	if err != nil {
		return nil, err
	}
	resp, err := s.projects.CreateRule(ctx, internalReq)
	if err != nil {
		return nil, err
	}
	return fromInternal(resp)
}

func fromExternal(req *pb_req.CreateRuleReq) (*authz.CreateRuleReq, error) {
	t, err := fromExternalType(req.Type)
	if err != nil {
		return nil, err
	}
	conditions, err := fromExternalConditions(req.Conditions)
	return &authz.CreateRuleReq{
		Id:         req.Id,
		Name:       req.Name,
		ProjectId:  req.ProjectId,
		Type:       t,
		Conditions: conditions,
	}, nil
}

func fromExternalType(t pb_common.RuleType) (authz.ProjectRuleTypes, error) {
	switch t {
	case pb_common.RuleType_NODE:
		return authz.ProjectRuleTypes_NODE, nil
	case pb_common.RuleType_EVENT:
		return authz.ProjectRuleTypes_EVENT, nil
	case pb_common.RuleType_RULE_TYPE_UNSET:
		return 0, status.Error(codes.InvalidArgument, "rule type not provided")
	default:
		return 0, fmt.Errorf("unknown rule type: %v", t)
	}
}

func fromExternalConditions(cs []*pb_common.Condition) ([]*authz.Condition, error) {
	apiConditions := make([]*authz.Condition, len(cs))
	for i, c := range cs {
		d, err := fromExternalCondition(c)
		if err != nil {
			return nil, err
		}
		apiConditions[i] = d
	}
	return apiConditions, nil
}

func fromExternalCondition(c *pb_common.Condition) (*authz.Condition, error) {
	t, err := fromExternalConditionType(c.Type)
	if err != nil {
		return nil, err
	}
	return &authz.Condition{
		Type:   t,
		Values: c.Values,
	}, nil
}

var externalToAPIConditionTypes = map[pb_common.ConditionType]authz.ProjectRuleConditionTypes{
	pb_common.ConditionType_ROLES:             authz.ProjectRuleConditionTypes_ROLES,
	pb_common.ConditionType_CHEF_SERVERS:      authz.ProjectRuleConditionTypes_CHEF_SERVERS,
	pb_common.ConditionType_CHEF_TAGS:         authz.ProjectRuleConditionTypes_CHEF_TAGS,
	pb_common.ConditionType_CHEF_ENVIRONMENTS: authz.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS,
	pb_common.ConditionType_CHEF_ORGS:         authz.ProjectRuleConditionTypes_CHEF_ORGS,
	pb_common.ConditionType_POLICY_GROUP:      authz.ProjectRuleConditionTypes_POLICY_GROUP,
	pb_common.ConditionType_POLICY_NAME:       authz.ProjectRuleConditionTypes_POLICY_NAME,
}
var apiToExternalConditionTypes = map[authz.ProjectRuleConditionTypes]pb_common.ConditionType{}
var onceReverseConditionTypesMapping sync.Once

func fromExternalConditionType(t pb_common.ConditionType) (authz.ProjectRuleConditionTypes, error) {
	if t == pb_common.ConditionType_CONDITION_TYPE_UNSET {
		return 0, status.Error(codes.InvalidArgument, "rule condition type not provided")
	} else if m, ok := externalToAPIConditionTypes[t]; ok {
		return m, nil
	} else {
		return 0, fmt.Errorf("unknown rule type: %v", t)
	}
}

func fromInternal(resp *authz.CreateRuleResp) (*pb_resp.CreateRuleResp, error) {
	r := resp.Rule
	cs, err := fromInternalConditions(r.Conditions)
	if err != nil {
		return nil, err
	}
	t, err := fromInternalRuleType(r.Type)
	if err != nil {
		return nil, err
	}
	rule := pb_common.Rule{
		Id:         r.Id,
		Name:       r.Name,
		Type:       t,
		ProjectId:  r.ProjectId,
		Conditions: cs,
	}
	return &pb_resp.CreateRuleResp{Rule: &rule}, nil
}

func fromInternalConditions(cs []*authz.Condition) ([]*pb_common.Condition, error) {
	externalConditions := make([]*pb_common.Condition, len(cs))
	for i, c := range cs {
		d, err := fromInternalCondition(c)
		if err != nil {
			return nil, err
		}
		externalConditions[i] = d
	}
	return externalConditions, nil
}

func fromInternalCondition(c *authz.Condition) (*pb_common.Condition, error) {
	t, err := fromInternalConditionType(c.Type)
	if err != nil {
		return nil, err
	}
	return &pb_common.Condition{
		Type:   t,
		Values: c.Values,
	}, nil
}

func fromInternalConditionType(t authz.ProjectRuleConditionTypes) (pb_common.ConditionType, error) {
	onceReverseConditionTypesMapping.Do(func() {
		for k, v := range externalToAPIConditionTypes {
			apiToExternalConditionTypes[v] = k
		}
	})
	if s, ok := apiToExternalConditionTypes[t]; ok {
		return s, nil
	}
	return 0, fmt.Errorf("invalid condition type %s", t.String())
}

func fromInternalRuleType(t authz.ProjectRuleTypes) (pb_common.RuleType, error) {
	switch t {
	case authz.ProjectRuleTypes_NODE:
		return pb_common.RuleType_NODE, nil
	case authz.ProjectRuleTypes_EVENT:
		return pb_common.RuleType_EVENT, nil
	default:
		return 0, fmt.Errorf("unknown internal rule type: %v", t)
	}
}
