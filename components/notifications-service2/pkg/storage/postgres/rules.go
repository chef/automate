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
	// PG normally returns number of rows updated - so if we find a match but
	// there is no change to it, it will return 0 rows updated.  To work around
	// that we declare "RETURNING id" - this makes sure if the record is found,
	// the ID is returned even if nothing changes.  When the record is not found,
	// this will not be included in the response.
	updateRuleQuery = `UPDATE rules SET name = $2, event = $3, action = $4, url = $5, secret_id = $6, critical_controls_only = $7 WHERE id = $1 RETURNING id`
)

const (
	errUniqueViolation = `duplicate key value violates unique constraint`
	errInvalidUUID     = `invalid input syntax for uuid`
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
	if (err != nil) && strings.Contains(err.Error(), errInvalidUUID) {
		return nil, storage.NewRuleNotFound(err)
	}
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

func (db *Postgres) UpdateRule(r *storage.Rule) (*storage.Rule, error) {
	// updateRuleQuery = `UPDATE rules SET name = $2, event = $3, action = $4, url = $5, secret_id = $6, critical_controls_only = $7 WHERE id = $1 RETURNING id`
	updatedId, err := db.DbMap.SelectStr(updateRuleQuery, r.Id, r.Name, r.Event, r.Action, r.URL, r.SecretId, r.CriticalControlsOnly)
	if (err != nil) && (strings.Contains(err.Error(), errUniqueViolation)) {
		return nil, storage.NewUniqueConstraintViolation(errors.Wrapf(err, "update failed due to duplicate name value %q", r.Name))
	}
	if (err != nil) && (strings.Contains(err.Error(), errInvalidUUID)) {
		return nil, storage.NewRuleNotFound(err)
	}
	if err != nil {
		return nil, errors.Wrapf(err, "failed to update rule data %+v", r)
	}
	if (updatedId != r.Id) || (updatedId == "") {
		return nil, storage.NewRuleNotFound(errors.Errorf("could not find rule with id %q for update", r.Id))
	}
	return r, nil
}

func (db *Postgres) DeleteRule(q *storage.DeleteRuleQuery) error {
	_, err := db.DbMap.Exec(deleteRuleQuery, q.Id)
	if (err != nil) && strings.Contains(err.Error(), errInvalidUUID) {
		return storage.NewRuleNotFound(err)
	}
	return err
}
