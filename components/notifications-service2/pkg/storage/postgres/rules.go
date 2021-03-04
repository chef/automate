package postgres

import (
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/notifications-service2/pkg/storage"
	uuid "github.com/chef/automate/lib/uuid4"
)

const (
	selectRuleQuery  = `SELECT id, name, event, action, url, secret_id, critical_controls_only FROM rules WHERE id = $1`
	selectRulesQuery = `SELECT id, name, event, action, url, secret_id, critical_controls_only FROM rules ORDER BY id`
	deleteRuleQuery  = `DELETE FROM rules WHERE id = $1`
)

const (
	errUniqueViolation = `duplicate key value violates unique constraint`
)

func (db *Postgres) AddRule(newRule *storage.Rule) (*storage.Rule, error) {
	id, err := uuid.NewV4()
	if err != nil {
		return nil, errors.Wrap(err, "failed to generate ID for new rule insert")
	}

	newRule.Id = id.String()

	err = db.DbMap.Insert(newRule)
	if (err != nil) && strings.Contains(err.Error(), errUniqueViolation) {
		return nil, storage.NewUniqueConstraintViolation(errors.Wrapf(err, "insert failed due to duplicate name value %q", newRule.Name))
	}
	if err != nil {
		return nil, errors.Wrapf(err, "failed to insert rule data %+v", newRule)
	}

	return newRule, nil
}

func (db *Postgres) GetRule(q *storage.GetRuleQuery) (*storage.Rule, error) {
	rule := &storage.Rule{}
	err := db.DbMap.SelectOne(rule, selectRuleQuery, q.Id)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to select rule with query data %+v", q)
	}

	return rule, nil
}

func (db *Postgres) ListRules() ([]*storage.Rule, error) {
	rules := []*storage.Rule{}
	_, err := db.DbMap.Select(&rules, selectRulesQuery)
	if err != nil {
		return nil, errors.Wrap(err, "failed to select rules")
	}

	return rules, nil
}

func (db *Postgres) DeleteRule(q *storage.DeleteRuleQuery) error {
	_, err := db.DbMap.Exec(deleteRuleQuery, q.Id)
	return err
}
