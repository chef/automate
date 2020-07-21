package elastic

import (
	"context"
	"encoding/json"
	"fmt"

	log "github.com/sirupsen/logrus"
	elastic "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/components/config-mgmt-service/errors"
)

// GetPolicyCookbooks Returns most recent results for policy name and list of cookbook locks given a revision id
func (es Backend) GetPolicyCookbooks(revisionID string) (backend.PolicyCookbooks, error) {
	var policyAction backend.Action
	var policyData map[string]interface{}
	var policyCookbooks backend.PolicyCookbooks
	boolQuery := elastic.NewBoolQuery()
	boolQuery = boolQuery.Must(elastic.NewTermQuery("revision_id", revisionID))

	searchResult, err := es.client.Search().
		Query(boolQuery).
		Index(IndexAction).
		Sort(ActionFieldTimestamp, false). //Want more recent first (ascending = false)
		Do(context.Background())

	// Return an error if the search was not successful
	if err != nil {
		return policyCookbooks, err
	}

	if searchResult.Hits.TotalHits == 0 {
		return policyCookbooks, errors.New(errors.ActionNotFound, fmt.Sprintf("No policy action found for revision ID: %s", revisionID))
	}

	source := searchResult.Hits.Hits[0].Source
	err = json.Unmarshal(*source, &policyAction)
	if err != nil {
		log.WithFields(log.Fields{
			"object": source,
		}).WithError(err).Debug("Unable to unmarshal the policy action object")
		return policyCookbooks, err
	}
	err = json.Unmarshal([]byte(policyAction.Data), &policyData)
	if err != nil {
		log.WithFields(log.Fields{
			"object": policyAction.Data,
		}).WithError(err).Debug("Unable to unmarshal the action data object for a policy action")
		return policyCookbooks, err
	}
	cookbookLocksData := policyData["cookbook_locks"].(map[string]interface{})
	policyCookbookLocks := make([]backend.PolicyCookbookLock, 0, len(cookbookLocksData))
	for cookbook, cData := range cookbookLocksData {
		cDataMap := cData.(map[string]interface{})
		cl := backend.PolicyCookbookLock{
			CookbookName: cookbook,
			PolicyID:     EmptyStringIfNil(cDataMap["identifier"]),
		}
		policyCookbookLocks = append(policyCookbookLocks, cl)
	}
	policyCookbooks.PolicyName = EmptyStringIfNil(policyData["name"])
	policyCookbooks.CookbookLocks = policyCookbookLocks
	return policyCookbooks, nil
}
