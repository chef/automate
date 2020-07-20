package postgres

import (
	"context"
	"net/url"
	"strings"
	"time"

	"github.com/lib/pq"
	"github.com/pkg/errors"
)

type NewRollout struct {
	Id                   int32  `db:"id, primarykey, autoincrement"`
	PolicyName           string `db:"policy_name"`
	PolicyNodeGroup      string `db:"policy_node_group"`
	PolicyRevisionId     string `db:"policy_revision_id"`
	PolicyDomainURL      string `db:"policy_domain_url"`
	PolicyDomainUsername string `db:"policy_domain_username"`
	SCMType              string `db:"scm_type"`
	SCMWebType           string `db:"scm_web_type"`
	PolicySCMURL         string `db:"policy_scm_url"`
	PolicySCMWebURL      string `db:"policy_scm_web_url"`
	PolicySCMCommit      string `db:"policy_scm_commit"`
	SCMAuthorName        string `db:"scm_author_name"`
	SCMAuthorEmail       string `db:"scm_author_email"`
	Description          string `db:"description"`
	CiJobId              string `db:"ci_job_id"`
	CiJobUrl             string `db:"ci_job_url"`
}

type Rollout struct {
	NewRollout
	StartTime *time.Time `db:"start_time"`
	EndTime   *time.Time `db:"end_time"`
}

type RolloutWithOrderIndex struct {
	Rollout
	OrderIndex int `db:"order_index"` // present in query for last N rollouts per segment
}

const getAllRollouts = `
  SELECT *
    FROM rollouts AS r
ORDER BY policy_domain_url ASC,
         policy_name ASC,
				 policy_node_group ASC,
				 start_time DESC
`

// CreateRollout takes the given rollout attributes and attempts to store the
// rollout in the database.
func (p *Postgres) CreateRollout(ctx context.Context, r *NewRollout) (*Rollout, error) {
	// TODO/FIXME:
	// - along with adding this, we need to make the other create rollout method work as follows:
	// if it doesn't exist, then create
	// if it does exist, then update the metadata fields
	if _, err := url.Parse(r.PolicyDomainURL); err != nil {
		return nil, errors.Wrapf(err, "Invalid policy domain URL %q", r.PolicyDomainURL)
	}

	err := p.mapper.WithContext(ctx).Insert(r)
	if err != nil {
		return nil, err
	}

	// We have to fetch the row back to get the values of any fields the database
	// itself set or modified.
	return p.FindRolloutByID(ctx, r.Id)
}

func (p *Postgres) FindRolloutByID(ctx context.Context, id int32) (*Rollout, error) {
	row, err := p.mapper.WithContext(ctx).Get(Rollout{}, id)
	if err != nil {
		return nil, err
	}
	createdRollout := row.(*Rollout)

	return createdRollout, nil
}

func (p *Postgres) GetRollouts(ctx context.Context) ([]*Rollout, error) {
	var rollouts []*Rollout
	_, err := p.mapper.Select(&rollouts, getAllRollouts)
	if err != nil {
		return nil, err
	}
	return rollouts, nil
}

func (p *Postgres) FindRolloutByNodeSegmentAndTime(nodeSegment *NodeSegment, time *time.Time) (*Rollout, error) {
	/*
	   Lookup for a Run:
	   - take name/group/domain/revision_id+timestamp. Find the rollouts where
	   	name/group/domain/revision_id match, then handle the timestamp. Need a
	   	tradeoff between these cases:
	   	- clock skew between nodes and automate. If using Chef Servers, this is
	   	probably limited to 15m but in other cases it could be more
	   	- there was a rollout way in the past with the given quad, which is the real
	   	one for that run, our record of that was deleted, then at some point in the
	   	future we rolled back to the same revision id (this seems kinda unlikely? or
	   	we can ignore it if we require retention of rollouts to be longer than CCRs)

	*/
	return nil, nil
}

func (p *Postgres) CreateRolloutFromChefAction(ctx context.Context, r *NewRollout) error {
	// From an end-user view, there are two ways to create rollouts. One is with
	// a metadata collector that runs on workstations and/or Ci, the other is via
	// Chef Server Actions (notifications). The metadata collector method is
	// preferred because Chef Server Actions only has the bare minimum data (the
	// metadata collector collects git and Ci information that the Chef Server
	// can't). The Chef Server Actions method provides a fallback if metadata
	// collection isn't configured correctly. It should have the following
	// behaviors under different circumstances:
	// - When creating a truly new rollout (revision id, policy name, policy group
	//   name, and policy domain URL DO NOT match an existing "current" rollout),
	//   then a new rollout row is inserted.
	// - When the rollout-to-be-created is already in the database (e.g., it was
	//   created via a different API/mechanism), ignore.
	if _, err := url.Parse(r.PolicyDomainURL); err != nil {
		return errors.Wrapf(err, "Invalid policy domain URL %q", r.PolicyDomainURL)
	}

	err := p.mapper.WithContext(ctx).Insert(r)
	if err != nil {
		// if this is a conflict with existing rollout, don't return an error, Chef
		// Server can't do anything about it and the outcome is what the user
		// wanted (rollout is created).
		if err, isPqError := err.(*pq.Error); isPqError {
			// this error comes from a `RAISE` inside a trigger function, so there's
			// not a great alternative to string matching. But it should be stable if
			// we don't muck with the trigger function. See 02_add_rollouts.up.sql in
			// the schema/sql dir.
			if strings.HasPrefix(err.Message, "current rollout exists") {
				return nil
			}
		}
		return err
	}

	return nil
}
