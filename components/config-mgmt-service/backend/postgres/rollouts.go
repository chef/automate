package postgres

import (
	"time"

	"github.com/chef/automate/api/external/cfgmgmt/request"
	"github.com/chef/automate/api/external/cfgmgmt/response"
)

type NodeSegment struct {
	PolicyName      string
	PolicyNodeGroup string
	PolicyDomainURL string
}

type NewRollout struct {
	request.CreateRollout
}

type Rollout struct {
	response.Rollout
}

// CreateRollout takes the given rollout attributes and attempts to store the
// rollout in the database.
func (db *Postgres) CreateRollout(newRollout *NewRollout) (*Rollout, error) {
	return nil, nil
}

func (db *Postgres) FindRolloutByID(id string) (*Rollout, error) {
	return nil, nil
}

func (db *Postgres) FindRolloutByNodeSegmentAndTime(nodeSegment *NodeSegment, time *time.Time) (*Rollout, error) {
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
