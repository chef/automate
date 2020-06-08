package postgres

import (
	"net/url"
	"path"
)

type NodeSegment struct {
	PolicyName      string
	PolicyNodeGroup string
	PolicyDomainURL string
}

type NodeSegmentWithRollouts struct {
	NodeSegment
	Rollouts []*Rollout
}

const normalizedURLScheme = "https"

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

	for _, r := range indexedRollouts {
		normalized, err := normalizeURL(r.PolicyDomainURL)
		// URL is checked on the way into the db so shouldn't see this
		if err != nil {
			return nil, err
		}
		r.PolicyDomainURL = normalized
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

func normalizeURL(urlFromDb string) (string, error) {
	// Showing correct rollouts data in the API/UI requires us to match the
	// policy domain URL (i.e., chef server URL) between records in postgres and
	// elastic, which collect the URLs in different ways from the user. Currently
	// the records in elastic only have the chef server FQDN and organization
	// name, whereas the rollout records in postgres have a user-supplied URL.
	// To ensure we match URLs when joining the two sets of records, we normalize
	// the URLs we get here to match the format we apply to the node-state
	// documents
	url, err := url.Parse(urlFromDb)
	if err != nil {
		return "", err
	}
	url.Path = path.Clean(url.EscapedPath())
	url.Scheme = "https"
	url.User = nil
	url.RawQuery = ""
	return url.String(), nil
}
