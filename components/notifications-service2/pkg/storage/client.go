package storage

import (
	"net/url"

	notifications "github.com/chef/automate/api/interservice/notifications/service"
)

// Constants ActionType... map to values of the `rule_action` enum in the schema.
const (
	ActionTypeSlackAlert      = "SlackAlert"
	ActionTypeWebhookAlert    = "WebhookAlert"
	ActionTypeServiceNowAlert = "ServiceNowAlert"
)

// Constants EventType... map to values of the `rule_event` enum in the schema.
const (
	EventTypeCCRSuccess        = "CCRSuccess"
	EventTypeCCRFailure        = "CCRFailure"
	EventTypeComplianceSuccess = "ComplianceSuccess"
	EventTypeComplianceFailure = "ComplianceFailure"
	EventTypeAssets            = "Assets"
)

// Client is the interface for storage operations used by `server.Server`.
// In production, it's a `postgres.Postgres` object. We use an interface here
// for two reasons:
// 1. Facilitate the use of mocks for unit testing
// 2. Expose only a subset of the functionality of the Postgres object's
// methods: we wish to hide low-level and testing-only methods from the
// application code in the Server.
type Client interface {
	AddRule(*Rule) (*Rule, error)
	GetRule(*GetRuleQuery) (*Rule, error)
	ListRules() ([]*Rule, error)
	UpdateRule(*Rule) (*Rule, error)
	DeleteRule(*DeleteRuleQuery) error
}

type Rule struct {
	Id                   string `db:"id"`
	Name                 string `db:"name"`
	Event                string `db:"event"`
	Action               string `db:"action"`
	URL                  string `db:"url"`
	SecretId             string `db:"secret_id"`
	CriticalControlsOnly bool   `db:"critical_controls_only"`
}

func (r *Rule) Proto() *notifications.Rule {
	p := &notifications.Rule{
		Id:   r.Id,
		Name: r.Name,
	}
	switch r.Action {
	case ActionTypeSlackAlert:
		p.Action = &notifications.Rule_SlackAlert{
			SlackAlert: &notifications.SlackAlert{
				Url: r.URL,
			},
		}
	case ActionTypeWebhookAlert:
		p.Action = &notifications.Rule_WebhookAlert{
			WebhookAlert: &notifications.WebhookAlert{
				Url: r.URL,
			},
		}
	case ActionTypeServiceNowAlert:
		p.Action = &notifications.Rule_ServiceNowAlert{
			ServiceNowAlert: &notifications.ServiceNowAlert{
				Url:                  r.URL,
				SecretId:             r.SecretId,
				CriticalControlsOnly: r.CriticalControlsOnly,
			},
		}
	}

	return p
}

type GetRuleQuery struct {
	Id string
}

type DeleteRuleQuery struct {
	Id string
}

type uniqueConstraintViolation struct {
	Err error
}

func IsUniqueConstraintViolation(err error) bool {
	_, ok := err.(*uniqueConstraintViolation)
	return ok
}

func NewUniqueConstraintViolation(err error) error {
	return &uniqueConstraintViolation{Err: err}
}

func (u *uniqueConstraintViolation) Error() string {
	return u.Err.Error()
}

type ruleNotFound struct {
	Err error
}

func IsRuleNotFound(err error) bool {
	_, ok := err.(*ruleNotFound)
	return ok
}

func (r *ruleNotFound) Error() string {
	return r.Err.Error()
}

func NewRuleNotFound(err error) error {
	return &ruleNotFound{Err: err}
}

type action struct {
	// ActionType is what kind of alert it is. The value should match exactly one
	// of the allowed values of the `rule_action` type defined in the database
	// schema. These are defined above as the constants named `ActionType...`
	ActionType string
	// URL of the webhook receiver service.
	URL string
	// SecretId references a secret stored in the secrets service. Not all types
	// of action have this.
	SecretId string
	// Some alert types allow the user to specify whether they only want alerts
	// on critical control failures.
	CriticalControlsOnly bool
}

func NewRuleFromReq(req *notifications.Rule) (*Rule, error) {
	action, err := actionFromReq(req)
	if err != nil {
		return nil, err
	}
	event, err := eventFromReq(req.Event)
	if err != nil {
		return nil, err
	}
	return &Rule{
		Id:                   req.Id,
		Name:                 req.Name,
		Event:                event,
		Action:               action.ActionType,
		URL:                  action.URL,
		SecretId:             action.SecretId,
		CriticalControlsOnly: action.CriticalControlsOnly,
	}, nil
}

func actionFromReq(req *notifications.Rule) (*action, error) {
	if a := req.GetSlackAlert(); a != nil {
		return &action{ActionType: ActionTypeSlackAlert, URL: a.Url}, nil
	}
	if a := req.GetWebhookAlert(); a != nil {
		return &action{ActionType: ActionTypeWebhookAlert, URL: a.Url}, nil
	}
	if a := req.GetServiceNowAlert(); a != nil {
		return &action{ActionType: ActionTypeServiceNowAlert, URL: a.Url, SecretId: a.SecretId}, nil
	}
	return &action{}, nil
}

func eventFromReq(eventReq notifications.Rule_Event) (string, error) {
	switch eventReq {
	case notifications.Rule_CCRFailure:
		return EventTypeCCRFailure, nil
	case notifications.Rule_CCRSuccess:
		return EventTypeCCRSuccess, nil
	case notifications.Rule_ComplianceFailure:
		return EventTypeComplianceFailure, nil
	case notifications.Rule_ComplianceSuccess:
		return EventTypeComplianceSuccess, nil
	default:
		return "", nil
	}
}

func (r *Rule) ValidateForInsert() ([]string, bool) {
	messages := []string{}
	if r.Id != "" {
		messages = append(messages, "Rule ID may not be included in an add-rule request")
	}

	fieldValuesErrors := r.validateForInsertOrUpdate()
	messages = append(messages, fieldValuesErrors...)

	ok := (len(messages) == 0)

	return messages, ok
}

func (r *Rule) ValidateForUpdate() ([]string, bool) {
	messages := []string{}
	if r.Id == "" {
		messages = append(messages, "Rule ID must be included from the rule being modified")
	}

	fieldValuesErrors := r.validateForInsertOrUpdate()
	messages = append(messages, fieldValuesErrors...)

	ok := (len(messages) == 0)

	return messages, ok
}

func (r *Rule) validateForInsertOrUpdate() []string {
	messages := []string{}
	if r.Name == "" {
		messages = append(messages, "Rule name must be supplied.")
	}
	if r.Event == "" {
		messages = append(messages, "Event must be supplied.")
	}

	actionErrors := r.validateAction()
	messages = append(messages, actionErrors...)

	return messages
}

func (r *Rule) validateAction() []string {
	messages := []string{}
	if r.Action == "" {
		messages = append(messages, "Action must be set.")
		return messages
	}
	if r.URL == "" {
		messages = append(messages, "A valid action URL must be supplied")
		return messages
	}
	actionURL, err := url.Parse(r.URL)
	if err != nil {
		messages = append(messages, "A valid action URL must be supplied")
		return messages
	}

	if actionURL.Scheme != "http" && actionURL.Scheme != "https" {
		messages = append(messages, "A valid action URL must be supplied")
	}
	if actionURL.Host == "" {
		messages = append(messages, "A valid action URL must be supplied")
	}

	return messages
}
