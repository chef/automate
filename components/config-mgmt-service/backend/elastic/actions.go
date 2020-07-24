package elastic

import (
	"github.com/chef/automate/components/config-mgmt-service/backend"
)

// GetPolicyCookbooks Returns most recent results for policy name and list of cookbook locks given a revision id
func (es Backend) GetPolicyCookbooks(revisionID string) (backend.PolicyCookbooks, error) {
	var policyCookbooks backend.PolicyCookbooks

	// pull cookbooks from postgresql

	return policyCookbooks, nil
}
