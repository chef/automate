package pgdb

import (
	"testing"

	"github.com/chef/automate/components/compliance-service/api/common"

	"github.com/stretchr/testify/assert"
)

// That this tests for source_id is arbitrary, there just needed to be a basic success test.
func TestValidateNodeFiltersReturnsNilWithSourceID(t *testing.T) {
	err := validateNodeFilters([]*common.Filter{
		{Key: "source_id", Values: []string{"some-source-id"}},
	})
	assert.Nil(t, err)
}

func TestValidateNodeFiltersReturnsNilWithTags(t *testing.T) {
	err := validateNodeFilters([]*common.Filter{
		{Key: "tags:tacos", Values: []string{"pork adobo"}},
	})
	assert.Nil(t, err)
}

func TestValidateNodeFiltersReturnsNilWithManagerIDFilter(t *testing.T) {
	err := validateNodeFilters([]*common.Filter{
		{Key: "manager_id", Values: []string{"some-manager-id"}},
	})
	assert.Nil(t, err)
}

func TestValidateNodeFiltersReturnsNilWithNameFilter(t *testing.T) {
	err := validateNodeFilters([]*common.Filter{
		{Key: "name", Values: []string{"foo"}},
	})
	assert.Nil(t, err)
}
