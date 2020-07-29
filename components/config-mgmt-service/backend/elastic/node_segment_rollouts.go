package elastic

import (
	"context"
	"encoding/json"
	"fmt"

	b "github.com/chef/automate/components/config-mgmt-service/backend"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"
)

const (
	pnameField      = "policy_name"
	pgroupField     = "policy_group"
	pRevField       = "policy_revision"
	chefServerField = "source_fqdn"
	chefOrgField    = "organization_name"
	// This field was added to the node-state index in
	// https://github.com/chef/automate/pull/3887
	// merge commit: 87185bab5a8994cc4fc230c2c3f5733afa04590a
	// It should contain the last run status even if the node has been marked
	// missing, so the only values we expect to see are "success" or "failed"
	statusField       = "chef_run_status"
	nodeSegmentsField = "node_segments"
)

func (es Backend) GetLatestRunRolloutBreakdownCounts() (*b.NodeSegmentRolloutProgress, error) {
	nodeSegments, err := es.collectAllRolloutBreakdowns()
	if err != nil {
		return nil, err
	}

	allSegments := &b.NodeSegmentRolloutProgress{BySegment: make(map[b.NodeSegment]*b.NodeSegmentRevisionsStatus)}

	for _, s := range nodeSegments {
		ns := s.Key

		segment := b.NodeSegment{
			PolicyName:      ns[pnameField].(string),
			PolicyNodeGroup: ns[pgroupField].(string),
			PolicyDomainURL: fmt.Sprintf("https://%s/organizations/%s", ns[chefServerField], ns[chefOrgField]),
		}

		var nodesInSegment int32
		json.Unmarshal(*s.Aggregations["doc_count"], &nodesInSegment)

		segmentWithStatus := &b.NodeSegmentRevisionsStatus{
			NodeSegment:      segment,
			NodesInSegment:   nodesInSegment,
			ByPolicyRevision: make(map[string]*b.PolicyRevisionNodeStatus),
		}

		allSegments.BySegment[segment] = segmentWithStatus

		nodesPolicyRevCounts, havePolicyRevAgg := s.Aggregations.Terms(pRevField)

		if !havePolicyRevAgg {
			return nil, errors.New("elasticsearch result did not contain expected aggregation data for policy_revision")
		}
		for _, policyRev := range nodesPolicyRevCounts.Buckets {

			var total int
			json.Unmarshal(*policyRev.Aggregations["doc_count"], &total)

			revId := policyRev.Key.(string)

			statusStats := &b.PolicyRevisionNodeStatus{
				PolicyRevisionID: revId,
				Total:            total,
			}

			segmentWithStatus.ByPolicyRevision[revId] = statusStats

			statusCounts, haveStatusCounts := policyRev.Aggregations.Terms(statusField)
			if !haveStatusCounts {
				return nil, errors.New("elasticsearch result did not contain expected aggregation data for run status")
			}
			for _, statusInfo := range statusCounts.Buckets {
				var countWithStatus int
				json.Unmarshal(*statusInfo.Aggregations["doc_count"], &countWithStatus)
				status := statusInfo.Key.(string)
				switch status {
				case "success":
					statusStats.Success = countWithStatus
				case "failure":
					statusStats.Errored = countWithStatus
				default:
					log.WithFields(log.Fields{
						"policy_name":                segment.PolicyName,
						"policy_group":               segment.PolicyNodeGroup,
						"policy_domain_url":          segment.PolicyDomainURL,
						"unexpected_chef_run_status": status,
					}).Error("Encountered unexpected value of chef_run_status in rollouts status stats aggregation")
				}
			}
		}
	}

	return allSegments, nil
}

func (es Backend) collectAllRolloutBreakdowns() ([]*elastic.AggregationBucketCompositeItem, error) {
	pctx := &paginationContext{}

	for haveAllResults := false; !haveAllResults; {
		err := pctx.IncrementIterations()
		if err != nil {
			return nil, err
		}
		err = es.getNextRollouts(pctx)
		if err != nil {
			return nil, err
		}

		haveAllResults = pctx.HaveAllResults()
	}

	return pctx.results, nil
}

func (es Backend) getNextRollouts(pctx *paginationContext) error {
	log.WithFields(log.Fields{
		"requestCount": pctx.requestCount,
		"resultCount":  len(pctx.results),
		"afterKey":     pctx.afterKey,
	}).Debug("requesting next page of node segments")

	nodeSegmentsAgg := elastic.NewCompositeAggregation()

	nodeSegmentsAgg.Sources(
		elastic.NewCompositeAggregationTermsValuesSource(pnameField).Field(pnameField),
		elastic.NewCompositeAggregationTermsValuesSource(pgroupField).Field(pgroupField),
		elastic.NewCompositeAggregationTermsValuesSource(chefServerField).Field(chefServerField),
		elastic.NewCompositeAggregationTermsValuesSource(chefOrgField).Field(chefOrgField),
	)

	nodeSegmentsAgg.Size(2)

	policyRevsAgg := elastic.NewTermsAggregation().Field(pRevField)
	runStatusAgg := elastic.NewTermsAggregation().Field(statusField)

	policyRevsAgg.SubAggregation(statusField, runStatusAgg)
	nodeSegmentsAgg.SubAggregation(pRevField, policyRevsAgg)

	var result *elastic.SearchResult

	if len(pctx.afterKey) != 0 {
		nodeSegmentsAgg.AggregateAfter(pctx.afterKey)
	}

	result, err := es.client.Search().
		Index(IndexNodeState).
		Aggregation(nodeSegmentsField, nodeSegmentsAgg).
		Size(0).
		Do(context.Background())

	if err != nil {
		return err
	}

	aggData, haveAgg := result.Aggregations.Composite(nodeSegmentsField)
	if !haveAgg {
		newErr := errors.New("elasticsearch aggregate query response did not contain expected aggregation data")
		log.WithError(newErr).WithFields(log.Fields{
			"expected_agg_name":    nodeSegmentsField,
			"actual_aggregations":  result.Aggregations,
			"last_after_key_value": pctx.afterKey,
			"page_request_count":   pctx.requestCount,
			"results_so_far_count": len(pctx.results),
		}).Error("aborting aggregation request")
		return newErr
	}
	pctx.HandleResults(aggData)
	return nil
}

type paginationContext struct {
	afterKey     map[string]interface{}
	requestCount int
	results      []*elastic.AggregationBucketCompositeItem
}

func (p *paginationContext) IncrementIterations() error {
	if p.requestCount >= 100 {
		message := "Reached maximum number of paginated requests for node segments aggregation"
		newErr := errors.New(message)
		log.WithError(newErr).WithFields(log.Fields{
			"request_count":            p.requestCount,
			"aggregation_bucket_count": len(p.results),
			"last_after_key_value":     p.afterKey,
		}).Error("aborting aggregation request")
		return newErr
	}

	p.requestCount++
	return nil
}

func (p *paginationContext) HandleResults(aggregationData *elastic.AggregationBucketCompositeItems) {
	p.results = append(p.results, aggregationData.Buckets...)
	p.afterKey = aggregationData.AfterKey
}

func (p *paginationContext) HaveAllResults() bool {
	return len(p.afterKey) == 0
}
