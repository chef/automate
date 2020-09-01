package rules

import (
	"context"
	"fmt"
	"sync"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	pb_common "github.com/chef/automate/api/external/iam/v2/common"
	pb_req "github.com/chef/automate/api/external/iam/v2/request"
	pb_resp "github.com/chef/automate/api/external/iam/v2/response"
	"github.com/chef/automate/api/interservice/authz"
)

type Server struct {
	projects authz.ProjectsServiceClient
}

func NewServer(projects authz.ProjectsServiceClient) *Server {
	return &Server{
		projects: projects,
	}
}

func (s *Server) CreateRule(ctx context.Context, req *pb_req.CreateRuleReq) (*pb_resp.CreateRuleResp, error) {
	internalReq, err := fromExternalCreate(req)
	if err != nil {
		return nil, err
	}
	resp, err := s.projects.CreateRule(ctx, internalReq)
	if err != nil {
		return nil, err
	}

	rule, err := fromInternal(resp.Rule)
	if err != nil {
		return nil, err
	}
	return &pb_resp.CreateRuleResp{Rule: rule}, nil
}

func (s *Server) UpdateRule(ctx context.Context, req *pb_req.UpdateRuleReq) (*pb_resp.UpdateRuleResp, error) {
	internalReq, err := fromExternalUpdate(req)
	if err != nil {
		return nil, err
	}

	resp, err := s.projects.UpdateRule(ctx, internalReq)
	if err != nil {
		return nil, err
	}

	rule, err := fromInternal(resp.Rule)
	if err != nil {
		return nil, err
	}
	return &pb_resp.UpdateRuleResp{Rule: rule}, nil
}

func (s *Server) GetRule(ctx context.Context, req *pb_req.GetRuleReq) (*pb_resp.GetRuleResp, error) {
	resp, err := s.projects.GetRule(ctx, &authz.GetRuleReq{Id: req.Id, ProjectId: req.ProjectId})
	if err != nil {
		return nil, err
	}

	rule, err := fromInternal(resp.Rule)
	if err != nil {
		return nil, err
	}
	return &pb_resp.GetRuleResp{Rule: rule}, nil
}

func (s *Server) ListRulesForProject(ctx context.Context, req *pb_req.ListRulesForProjectReq) (*pb_resp.ListRulesForProjectResp, error) {
	resp, err := s.projects.ListRulesForProject(ctx, &authz.ListRulesForProjectReq{Id: req.Id})
	if err != nil {
		return nil, err
	}

	rules := make([]*pb_common.Rule, len(resp.Rules))
	for i, rule := range resp.Rules {
		apiRule, err := fromInternal(rule)
		if err != nil {
			return nil, status.Error(codes.Internal, err.Error())
		}

		rules[i] = apiRule
	}

	return &pb_resp.ListRulesForProjectResp{
		Rules:  rules,
		Status: FromInternalRulesStatus(resp.Status),
	}, nil
}

func (s *Server) DeleteRule(ctx context.Context, req *pb_req.DeleteRuleReq) (*pb_resp.DeleteRuleResp, error) {
	_, err := s.projects.DeleteRule(ctx, &authz.DeleteRuleReq{Id: req.Id, ProjectId: req.ProjectId})
	if err != nil {
		return nil, err
	}

	return &pb_resp.DeleteRuleResp{}, nil
}

func (s *Server) ApplyRulesStart(ctx context.Context, _ *pb_req.ApplyRulesStartReq) (*pb_resp.ApplyRulesStartResp, error) {
	_, err := s.projects.ApplyRulesStart(ctx, &authz.ApplyRulesStartReq{})
	if err != nil {
		return nil, err
	}
	return &pb_resp.ApplyRulesStartResp{}, nil
}

func (s *Server) ApplyRulesCancel(ctx context.Context,
	_ *pb_req.ApplyRulesCancelReq) (*pb_resp.ApplyRulesCancelResp, error) {

	_, err := s.projects.ApplyRulesCancel(ctx, &authz.ApplyRulesCancelReq{})
	if err != nil {
		return nil, err
	}
	return &pb_resp.ApplyRulesCancelResp{}, nil
}

func (s *Server) ApplyRulesStatus(ctx context.Context,
	_ *pb_req.ApplyRulesStatusReq) (*pb_resp.ApplyRulesStatusResp, error) {

	resp, err := s.projects.ApplyRulesStatus(ctx, &authz.ApplyRulesStatusReq{})
	if err != nil {
		return nil, err
	}
	return &pb_resp.ApplyRulesStatusResp{
		EstimatedTimeComplete: resp.EstimatedTimeComplete,
		PercentageComplete:    resp.PercentageComplete,
		Failed:                resp.Failed,
		FailureMessage:        resp.FailureMessage,
		State:                 resp.State,
		Cancelled:             resp.Cancelled,
	}, nil
}

func fromExternalCreate(req *pb_req.CreateRuleReq) (*authz.CreateRuleReq, error) {
	t, err := fromExternalType(req.Type)
	if err != nil {
		return nil, err
	}
	conditions, err := fromExternalConditions(req.Conditions)
	if err != nil {
		return nil, err
	}
	return &authz.CreateRuleReq{
		Id:         req.Id,
		Name:       req.Name,
		ProjectId:  req.ProjectId,
		Type:       t,
		Conditions: conditions,
	}, nil
}

// Wish we had generics
func fromExternalUpdate(req *pb_req.UpdateRuleReq) (*authz.UpdateRuleReq, error) {
	t, err := fromExternalType(req.Type)
	if err != nil {
		return nil, err
	}
	conditions, err := fromExternalConditions(req.Conditions)
	if err != nil {
		return nil, err
	}
	return &authz.UpdateRuleReq{
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
	a, err := fromExternalConditionAttribute(c.Attribute)
	if err != nil {
		return nil, err
	}

	o, err := fromExternalConditionOperator(c.Operator)
	if err != nil {
		return nil, err
	}

	return &authz.Condition{
		Attribute: a,
		Values:    c.Values,
		Operator:  o,
	}, nil
}

var externalToAPIConditionAttributes = map[pb_common.ConditionAttribute]authz.ProjectRuleConditionAttributes{
	pb_common.ConditionAttribute_CHEF_ROLE:         authz.ProjectRuleConditionAttributes_CHEF_ROLE,
	pb_common.ConditionAttribute_CHEF_SERVER:       authz.ProjectRuleConditionAttributes_CHEF_SERVER,
	pb_common.ConditionAttribute_CHEF_TAG:          authz.ProjectRuleConditionAttributes_CHEF_TAG,
	pb_common.ConditionAttribute_ENVIRONMENT:       authz.ProjectRuleConditionAttributes_ENVIRONMENT,
	pb_common.ConditionAttribute_CHEF_ORGANIZATION: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
	pb_common.ConditionAttribute_CHEF_POLICY_GROUP: authz.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
	pb_common.ConditionAttribute_CHEF_POLICY_NAME:  authz.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
}
var apiToExternalConditionAttributes = map[authz.ProjectRuleConditionAttributes]pb_common.ConditionAttribute{}
var onceReverseConditionAttributesMapping sync.Once

func fromExternalConditionAttribute(a pb_common.ConditionAttribute) (authz.ProjectRuleConditionAttributes, error) {
	if a == pb_common.ConditionAttribute_CONDITION_ATTRIBUTE_UNSET {
		return 0, status.Error(codes.InvalidArgument, "rule condition attribute not provided")
	} else if m, ok := externalToAPIConditionAttributes[a]; ok {
		return m, nil
	} else {
		return 0, fmt.Errorf("unknown condition attribute: %v", a)
	}
}

var externalToAPIConditionOperators = map[pb_common.ConditionOperator]authz.ProjectRuleConditionOperators{
	pb_common.ConditionOperator_MEMBER_OF: authz.ProjectRuleConditionOperators_MEMBER_OF,
	pb_common.ConditionOperator_EQUALS:    authz.ProjectRuleConditionOperators_EQUALS,
}
var apiToExternalConditionOperators = map[authz.ProjectRuleConditionOperators]pb_common.ConditionOperator{}
var onceReverseConditionOperatorsMapping sync.Once

func fromExternalConditionOperator(t pb_common.ConditionOperator) (authz.ProjectRuleConditionOperators, error) {
	if t == pb_common.ConditionOperator_CONDITION_OPERATOR_UNSET {
		return 0, status.Error(codes.InvalidArgument, "rule condition operator not provided")
	} else if o, ok := externalToAPIConditionOperators[t]; ok {
		return o, nil
	} else {
		return 0, fmt.Errorf("unknown rule operator: %v", t)
	}
}

func fromInternal(r *authz.ProjectRule) (*pb_common.Rule, error) {
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
		Status:     fromInternalStatus(r.Status),
	}
	return &rule, nil
}

func fromInternalStatus(internalStatus string) pb_common.RuleStatus {
	switch internalStatus {
	case "staged":
		return pb_common.RuleStatus_STAGED
	case "applied":
		return pb_common.RuleStatus_APPLIED
	default:
		return pb_common.RuleStatus_RULE_STATUS_UNSET
	}
}

func FromInternalRulesStatus(internalRulesStatus string) pb_common.ProjectRulesStatus {
	switch internalRulesStatus {
	case "applied":
		return pb_common.ProjectRulesStatus_RULES_APPLIED
	case "edits-pending":
		return pb_common.ProjectRulesStatus_EDITS_PENDING
	case "no-rules":
		return pb_common.ProjectRulesStatus_NO_RULES
	default:
		return pb_common.ProjectRulesStatus_PROJECT_RULES_STATUS_UNSET
	}
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
	a, err := fromInternalConditionAttribute(c.Attribute)
	if err != nil {
		return nil, err
	}

	o, err := fromInternalConditionOperator(c.Operator)
	if err != nil {
		return nil, err
	}
	return &pb_common.Condition{
		Attribute: a,
		Values:    c.Values,
		Operator:  o,
	}, nil
}

func fromInternalConditionAttribute(a authz.ProjectRuleConditionAttributes) (pb_common.ConditionAttribute, error) {
	onceReverseConditionAttributesMapping.Do(func() {
		for k, v := range externalToAPIConditionAttributes {
			apiToExternalConditionAttributes[v] = k
		}
	})
	if s, ok := apiToExternalConditionAttributes[a]; ok {
		return s, nil
	}
	return 0, fmt.Errorf("invalid condition attribute %s", a.String())
}

func fromInternalConditionOperator(t authz.ProjectRuleConditionOperators) (pb_common.ConditionOperator, error) {
	onceReverseConditionOperatorsMapping.Do(func() {
		for k, v := range externalToAPIConditionOperators {
			apiToExternalConditionOperators[v] = k
		}
	})
	if s, ok := apiToExternalConditionOperators[t]; ok {
		return s, nil
	}
	return 0, fmt.Errorf("invalid condition operator %s", t.String())
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
