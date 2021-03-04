package server

import (
	"context"

	"github.com/pkg/errors"

	notifications "github.com/chef/automate/api/interservice/notifications/service"
	"github.com/chef/automate/components/notifications-service2/pkg/storage"
)

func (s *Server) AddRule(ctx context.Context, req *notifications.Rule) (*notifications.RuleAddResponse, error) {
	newRuleQuery, err := storage.NewRuleFromReq(req)
	if err != nil {
		return &notifications.RuleAddResponse{}, errors.Wrap(err, "unable to format AddRule request data into database request")
	}
	if messages, ok := newRuleQuery.ValidateForInsert(); !ok {
		return &notifications.RuleAddResponse{
			Messages: messages,
			Code:     notifications.RuleAddResponse_VALIDATION_ERROR,
		}, nil
	}
	newRuleWithId, err := s.db.AddRule(newRuleQuery)
	if err != nil && storage.IsUniqueConstraintViolation(err) {
		return &notifications.RuleAddResponse{
			Code:     notifications.RuleAddResponse_DUPLICATE_NAME,
			Messages: []string{"A rule with this name already exists"},
		}, nil
	}
	if err != nil {
		return &notifications.RuleAddResponse{}, errors.Wrap(err, "failed to insert rule into the database")
	}

	ruleRet := newRuleWithId.Proto()
	ruleRet.Id = ""
	return &notifications.RuleAddResponse{
		Id:   newRuleWithId.Id,
		Rule: ruleRet,
	}, nil
}
func (s *Server) DeleteRule(ctx context.Context, req *notifications.RuleIdentifier) (*notifications.RuleDeleteResponse, error) {
	err := s.db.DeleteRule(&storage.DeleteRuleQuery{Id: req.Id})
	return &notifications.RuleDeleteResponse{}, err
}
func (s *Server) UpdateRule(context.Context, *notifications.Rule) (*notifications.RuleUpdateResponse, error) {
	return &notifications.RuleUpdateResponse{}, nil
}
func (s *Server) GetRule(ctx context.Context, req *notifications.RuleIdentifier) (*notifications.RuleGetResponse, error) {
	query := &storage.GetRuleQuery{Id: req.Id}
	rule, err := s.db.GetRule(query)
	if err != nil {
		return &notifications.RuleGetResponse{}, errors.Wrap(err, "failed to get rule from the database")
	}
	return &notifications.RuleGetResponse{
		Rule: rule.Proto(),
	}, nil
}
func (s *Server) ListRules(context.Context, *notifications.Empty) (*notifications.RuleListResponse, error) {
	rules, err := s.db.ListRules()
	if err != nil {
		return &notifications.RuleListResponse{}, errors.Wrap(err, "failed to list rules from the database")
	}

	ret := &notifications.RuleListResponse{Rules: []*notifications.Rule{}}

	for _, r := range rules {
		ret.Rules = append(ret.Rules, r.Proto())
	}

	return ret, nil
}
