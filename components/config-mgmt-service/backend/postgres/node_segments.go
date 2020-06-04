package postgres

type NodeSegment struct {
	PolicyName      string
	PolicyNodeGroup string
	PolicyDomainURL string
}

type NodeSegmentWithRollouts struct {
	NodeSegment
	Rollouts []*Rollout
}

const mostRecentRolloutsForSegments = `
  SELECT rollouts_by_node_segment.*
    FROM (
         SELECT r.*, rank()
				   OVER (
					        PARTITION BY policy_node_group, policy_name, policy_domain_url
									    ORDER BY start_time DESC, id
								) AS order_index
           FROM rollouts r
         ) AS rollouts_by_node_segment
   WHERE order_index <= 5
ORDER BY policy_node_group ASC, policy_name ASC, policy_domain_url ASC;
`

func (p *Postgres) ListNodeSegmentsForRolloutProgress() ([]*NodeSegmentWithRollouts, error) {
	// for each distinct node segment, need the last 5 rollouts
	var indexedRollouts []*RolloutWithOrderIndex
	_, err := p.mapper.Select(&indexedRollouts, mostRecentRolloutsForSegments)
	if err != nil {
		return nil, err
	}

	var segmentsWithRollouts []*NodeSegmentWithRollouts
	n := &NodeSegmentWithRollouts{}
	for _, r := range indexedRollouts {
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
		n.Rollouts = append(n.Rollouts, &r.Rollout)
	}
	return segmentsWithRollouts, nil
}
