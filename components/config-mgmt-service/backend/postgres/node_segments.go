package postgres

import (
	"time"
)

type NodeSegment struct {
	PolicyName      string
	PolicyNodeGroup string
	PolicyDomainURL string
}

type NodeSegmentWithRollouts struct {
	NodeSegment
	Rollouts []*RolloutForSegmentProgress
}

type RolloutForSegmentProgress struct {
	Id               int32      `db:"id"`
	PolicyName       string     `db:"policy_name"`
	PolicyNodeGroup  string     `db:"policy_node_group"`
	PolicyRevisionId string     `db:"policy_revision_id"`
	PolicyDomainURL  string     `db:"policy_domain_url"`
	StartTime        *time.Time `db:"start_time"`
	EndTime          *time.Time `db:"end_time"`
}

const mostRecentRolloutsForSegments = `
  SELECT id, policy_node_group, policy_name, policy_domain_url, policy_revision_id, start_time, end_time
    FROM (
         SELECT *, rank()
				   OVER (
					        PARTITION BY policy_node_group, policy_name, policy_domain_url
									    ORDER BY start_time DESC, id
								) AS i
           FROM rollouts r
         ) AS rollouts_by_node_segment
   WHERE i <= 5
ORDER BY policy_node_group ASC, policy_name ASC, policy_domain_url ASC;
`

func (p *Postgres) ListNodeSegmentsForRolloutProgress() ([]*NodeSegmentWithRollouts, error) {
	// for each distinct node segment, need the last 5 rollouts
	var rollouts []*RolloutForSegmentProgress
	_, err := p.mapper.Select(&rollouts, mostRecentRolloutsForSegments)
	if err != nil {
		return nil, err
	}

	var segmentsWithRollouts []*NodeSegmentWithRollouts
	n := &NodeSegmentWithRollouts{}
	for _, r := range rollouts {
		if r.PolicyName != n.PolicyName ||
			r.PolicyNodeGroup != n.PolicyNodeGroup ||
			r.PolicyDomainURL != n.PolicyDomainURL {
			n = &NodeSegmentWithRollouts{
				NodeSegment: NodeSegment{
					PolicyName:      r.PolicyName,
					PolicyNodeGroup: r.PolicyNodeGroup,
					PolicyDomainURL: r.PolicyDomainURL,
				},
			}
			segmentsWithRollouts = append(segmentsWithRollouts, n)
		}
		n.Rollouts = append(n.Rollouts, r)
	}
	return segmentsWithRollouts, nil
}
