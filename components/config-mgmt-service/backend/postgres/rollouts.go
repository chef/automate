package postgres

import (
	"context"
	"time"
)

type NodeSegment struct {
	PolicyName      string
	PolicyNodeGroup string
	PolicyDomainURL string
}

type NewRollout struct {
	Id               int32  `db:"id, primarykey, autoincrement"`
	PolicyName       string `db:"policy_name"`
	PolicyNodeGroup  string `db:"policy_node_group"`
	PolicyRevisionId string `db:"policy_revision_id"`
	PolicyDomainURL  string `db:"policy_domain_url"`
	SCMType          string `db:"scm_type"`
	SCMWebType       string `db:"scm_web_type"`
	PolicySCMURL     string `db:"policy_scm_url"`
	PolicySCMWebURL  string `db:"policy_scm_web_url"`
	PolicySCMCommit  string `db:"policy_scm_commit"`
	Description      string `db:"description"`
	CiJobId          string `db:"ci_job_id"`
	CiJobUrl         string `db:"ci_job_url"`
}

type Rollout struct {
	NewRollout
	StartTime *time.Time `db:"start_time"`
	EndTime   *time.Time `db:"end_time"`
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
