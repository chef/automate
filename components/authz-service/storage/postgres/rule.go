package postgres

import (
	"context"
	"database/sql"
	"fmt"

	"github.com/lib/pq"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/lib/stringutils"
)

// These must match what SQL function query_rule_table_associations returns.
const (
	pgApplied = "applied"
	pgStaged  = "staged"
)

func (p *pg) CreateRule(ctx context.Context, rule *storage.Rule) (*storage.Rule, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, err
	}
	allProjectsAlowed := len(projectsFilter) == 0
	if !allProjectsAlowed && !stringutils.SliceContains(projectsFilter, rule.ProjectID) {
		return nil, fmt.Errorf("project with ID %q not found", rule.ProjectID)
	}

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, p.processError(err)
	}

	assocMap, err := p.getMapOfRuleAssociations(ctx, tx, rule.ID, rule.ProjectID)
	if err != nil {
		return nil, p.processError(err)
	}

	// If any associations return, then the rule already exists in current, staged, or both tables
	if len(assocMap) > 0 {
		return nil, storage.ErrConflict
	}

	row := tx.QueryRowContext(ctx,
		`INSERT INTO iam_staged_project_rules (id, project_id, name, type, deleted)
		VALUES ($1, project_db_id($2), $3, $4, false)
		RETURNING db_id`,
		rule.ID, rule.ProjectID, rule.Name, rule.Type.String())
	var ruleDbID string
	if err := row.Scan(&ruleDbID); err != nil {
		return nil, p.processError(err)
	}

	for _, condition := range rule.Conditions {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO iam_staged_rule_conditions (rule_db_id, value, attribute, operator) VALUES ($1, $2, $3, $4);`,
			ruleDbID, pq.Array(condition.Value), condition.Attribute.String(), condition.Operator.String(),
		)
		if err != nil {
			return nil, p.processError(err)
		}
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage.NewTxCommitError(err)
	}

	rule.Status = pgStaged
	return rule, nil
}

func (p *pg) UpdateRule(ctx context.Context, rule *storage.Rule) (*storage.Rule, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, err
	}

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, p.processError(err)
	}

	row := tx.QueryRowContext(ctx,
		`SELECT update_rule($1, $2, $3, $4, $5)`,
		rule.ID, rule.ProjectID, rule.Name, rule.Type.String(), pq.Array(projectsFilter))
	var ruleDbID int
	if err := row.Scan(&ruleDbID); err != nil {
		return nil, p.processError(err)
	}

	// Delete the existing conditions. Don't need to worry about "not found" case since a rule must have conditions.
	_, err = tx.ExecContext(ctx, `DELETE FROM iam_staged_rule_conditions WHERE rule_db_id=$1;`, ruleDbID)
	if err != nil {
		return nil, p.processError(err)
	}

	for _, condition := range rule.Conditions {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO iam_staged_rule_conditions (rule_db_id, value, attribute, operator) VALUES ($1, $2, $3, $4);`,
			ruleDbID, pq.Array(condition.Value), condition.Attribute.String(), condition.Operator.String(),
		)
		if err != nil {
			return nil, p.processError(err)
		}
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage.NewTxCommitError(err)
	}

	rule.Status = pgStaged
	return rule, nil
}

func (p *pg) DeleteRule(ctx context.Context, projectID string, ruleID string) error {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return err
	}

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return p.processError(err)
	}

	assocMap, err := p.getMapOfRuleAssociations(ctx, tx, ruleID, projectID)
	if err != nil {
		return p.processError(err)
	}

	ruleStaged := assocMap[pgStaged]
	ruleApplied := assocMap[pgApplied]

	if !ruleStaged && !ruleApplied {
		return storage.ErrNotFound
	}

	if ruleApplied && ruleStaged {
		res, err := tx.ExecContext(ctx,
			`UPDATE iam_staged_project_rules
				SET deleted=true
				WHERE id=$1 AND project_id=project_db_id($2) AND projects_match_for_rule($2, $3)`,
			ruleID, projectID, pq.Array(projectsFilter),
		)
		if err != nil {
			return p.processError(err)
		}
		err = p.singleRowResultOrNotFoundErr(res)
		if err != nil {
			return err
		}
	} else if ruleApplied {
		res, err := tx.ExecContext(ctx,
			`SELECT db_id FROM iam_project_rules
				WHERE id=$1 AND project_id=project_db_id($2) AND projects_match_for_rule($2, $3)`,
			ruleID, projectID, pq.Array(projectsFilter),
		)
		if err != nil {
			return p.processError(err)
		}
		err = p.singleRowResultOrNotFoundErr(res)
		if err != nil {
			return err
		}

		_, err = tx.ExecContext(ctx,
			`INSERT INTO iam_staged_project_rules (id, project_id, name, type, deleted)
				SELECT a.id, a.project_id, a.name, a.type, 'true'
				FROM iam_project_rules AS a
				WHERE a.id=$1 AND projects_match_for_rule(a.project_id, $2)`,
			ruleID, pq.Array(projectsFilter),
		)
		if err != nil {
			return p.processError(err)
		}
		// Code is built around expectation that rules always have at least one condition,
		// that means even in the case of impending deletion!
		// Value will never be seen, so a dummy value is OK here.
		_, err = tx.ExecContext(ctx,
			`INSERT INTO iam_staged_rule_conditions (rule_db_id, value, attribute, operator)
			 (SELECT db_id, '{dummy}', 'chef-server', 'equals'  FROM iam_staged_project_rules WHERE id=$1)`,
			ruleID,
		)
		if err != nil {
			return p.processError(err)
		}
	} else if ruleStaged {
		res, err := tx.ExecContext(ctx,
			`DELETE FROM iam_staged_project_rules
				WHERE id=$1 AND project_id=project_db_id($2) AND projects_match_for_rule($2, $3)`,
			ruleID, projectID, pq.Array(projectsFilter),
		)
		if err != nil {
			return p.processError(err)
		}
		err = p.singleRowResultOrNotFoundErr(res)
		if err != nil {
			return err
		}
	}

	err = tx.Commit()
	if err != nil {
		return storage.NewTxCommitError(err)
	}

	return nil
}

func (p *pg) GetStagedOrAppliedRule(ctx context.Context, projectID string, ruleID string) (*storage.Rule, error) {
	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, err
	}

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	var rule storage.Rule
	row := p.db.QueryRowContext(ctx, "SELECT query_staged_or_applied_rule($1, $2, $3)",
		ruleID, projectID, pq.Array(projectsFilter),
	)
	err = row.Scan(&rule)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, storage.ErrNotFound
		}
		return nil, p.processError(err)
	}

	return &rule, nil
}

func (p *pg) FetchAppliedRulesByProjectIDs(ctx context.Context) (map[string][]*storage.Rule, error) {
	rules, err := p.listRulesUsingFunction(ctx, "SELECT query_rules($1)", false)
	if err != nil {
		return nil, err
	}

	projectRules := make(map[string][]*storage.Rule, len(rules))
	for _, rule := range rules {
		projectRules[rule.ProjectID] = append(projectRules[rule.ProjectID], rule)
	}

	return projectRules, nil
}

func (p *pg) ListRules(ctx context.Context) ([]*storage.Rule, error) {
	return p.listRulesUsingFunction(ctx, "SELECT query_rules($1)", true)
}

func (p *pg) ListStagedAndAppliedRules(ctx context.Context) ([]*storage.Rule, error) {
	return p.listRulesUsingFunction(ctx, "SELECT query_staged_and_applied_rules($1)", true)
}

func (p *pg) listRulesUsingFunction(ctx context.Context, query string, filterProjects bool) ([]*storage.Rule, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	projectsFilter := []string{}
	if filterProjects {
		var err error
		projectsFilter, err = projectsListFromContext(ctx)
		if err != nil {
			return nil, err
		}
	}

	var rules []*storage.Rule
	rows, err := p.db.QueryContext(ctx, query, pq.Array(projectsFilter))
	if err != nil {
		return nil, p.processError(err)
	}

	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	for rows.Next() {
		var rule storage.Rule
		if err := rows.Scan(&rule); err != nil {
			return nil, p.processError(err)
		}
		rules = append(rules, &rule)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return rules, nil
}

func (p *pg) ListRulesForProject(ctx context.Context, projectID string) ([]*storage.Rule, storage.ProjectRulesStatus, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	projectsFilter, err := projectsListFromContext(ctx)
	if err != nil {
		return nil, storage.RulesStatusError, err
	}

	// in our other APIs we use a a postgres query to do filtering
	// however in this case, we can't automatically assume NoRows means NotFound
	// because we want to differentiate between a project that is not in the project filter
	// and a project that has no rules
	if len(projectsFilter) > 0 {
		var projectInFilter bool
		for _, id := range projectsFilter {
			if id == projectID {
				projectInFilter = true
				break
			}
		}
		if !projectInFilter {
			return nil, storage.RulesStatusError, storage.ErrNotFound
		}
	}

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, storage.RulesStatusError, p.processError(err)
	}

	var rules []*storage.Rule
	rows, err := tx.QueryContext(ctx, "SELECT query_rules_for_project($1, $2)",
		projectID, pq.Array(projectsFilter))
	if err != nil {
		return nil, storage.RulesStatusError, p.processError(err)
	}

	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	anyStagedRules := false
	for rows.Next() {
		var rule storage.Rule
		if err := rows.Scan(&rule); err != nil {
			return nil, storage.RulesStatusError, p.processError(err)
		}
		if rule.Status == pgStaged {
			anyStagedRules = true
		}
		rules = append(rules, &rule)
	}
	if err := rows.Err(); err != nil {
		return nil, storage.RulesStatusError, errors.Wrap(err, "error retrieving result rows")
	}

	err = tx.Commit()
	if err != nil {
		return nil, storage.RulesStatusError, storage.NewTxCommitError(err)
	}

	rulesStatus := storage.Applied
	if len(rules) == 0 {
		rulesStatus = storage.NoRules
	}
	if anyStagedRules {
		rulesStatus = storage.EditsPending
	}

	return rules, rulesStatus, nil
}

// ApplyStagedRules begins a db transaction, locks the rule tables, moves all staged rule updates
// and deletes into the applied rule table, and returns the database transaction. The transaction is returned
// so that other non-database concerns can be completed before freeing the lock to avoid race conditions.
func (p *pg) ApplyStagedRules(ctx context.Context) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return p.processError(err)
	}

	_, err = tx.ExecContext(ctx,
		`LOCK TABLE iam_project_rules;
			LOCK TABLE iam_rule_conditions;
			LOCK TABLE iam_staged_project_rules;
			LOCK TABLE iam_staged_rule_conditions; `,
	)
	if err != nil {
		return p.processError(err)
	}

	// Upsert all staged rules into applied rules marked for update, returning the id and db_id
	// of all rules affected so we can update their conditions below.
	rows, err := tx.QueryContext(ctx,
		`INSERT INTO iam_project_rules (id, project_id, name, type)
				SELECT s.id, s.project_id, s.name, s.type
					FROM iam_staged_project_rules AS s
					WHERE deleted=false
				ON CONFLICT (id) DO UPDATE
				SET name=excluded.name, type=excluded.type
				RETURNING id, db_id;`)
	if err != nil {
		return p.processError(err)
	}
	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	// For every staged rule updated, we need to update conditions.
	ids := make(map[string]string)
	for rows.Next() {
		var id string
		var dbID string
		err = rows.Scan(&id, &dbID)
		if err != nil {
			return p.processError(err)
		}
		ids[id] = dbID
	}
	if err := rows.Err(); err != nil {
		return errors.Wrap(err, "error retrieving result rows")
	}

	for id, dbID := range ids {
		_, err = tx.ExecContext(ctx,
			`DELETE FROM iam_rule_conditions WHERE rule_db_id=$1;`, dbID)
		if err != nil {
			return p.processError(err)
		}

		_, err = tx.ExecContext(ctx,
			`INSERT INTO iam_rule_conditions (rule_db_id, value, attribute, operator)
					SELECT $2, cond.value, cond.attribute, cond.operator
						FROM iam_staged_project_rules AS r
					LEFT OUTER JOIN iam_staged_rule_conditions AS cond
						ON rule_db_id=r.db_id
						WHERE r.id=$1;`,
			id, dbID,
		)
		if err != nil {
			return p.processError(err)
		}
	}

	_, err = tx.ExecContext(ctx,
		`DELETE FROM iam_project_rules
				WHERE id IN (SELECT id FROM iam_staged_project_rules WHERE deleted)`)
	if err != nil {
		return p.processError(err)
	}

	_, err = tx.ExecContext(ctx, `DELETE FROM iam_staged_project_rules;`)
	if err != nil {
		return p.processError(err)
	}

	err = tx.Commit()
	if err != nil {
		return storage.NewTxCommitError(err)
	}

	return nil
}
