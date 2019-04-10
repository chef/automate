//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package params_test

import (
	"testing"

	subject "github.com/chef/automate/components/config-mgmt-service/params"
	"github.com/stretchr/testify/assert"
)

func TestValidSuggestionParam(t *testing.T) {
	// Valid
	assert.True(t, subject.ValidSuggestionParam("platform"))
	assert.True(t, subject.ValidSuggestionParam("name"))
	assert.True(t, subject.ValidSuggestionParam("environment"))
	assert.True(t, subject.ValidSuggestionParam("policy_group"))
	assert.True(t, subject.ValidSuggestionParam("policy_name"))
	assert.True(t, subject.ValidSuggestionParam("policy_revision"))
	assert.True(t, subject.ValidSuggestionParam("cookbook"))
	assert.True(t, subject.ValidSuggestionParam("recipe"))
	assert.True(t, subject.ValidSuggestionParam("resource_name"))
	assert.True(t, subject.ValidSuggestionParam("attribute"))
	assert.True(t, subject.ValidSuggestionParam("role"))
	assert.True(t, subject.ValidSuggestionParam("chef_version"))
	assert.True(t, subject.ValidSuggestionParam("chef_tags"))

	// Invalid
	assert.False(t, subject.ValidSuggestionParam("foo"))
	assert.False(t, subject.ValidSuggestionParam("zelda"))
	assert.False(t, subject.ValidSuggestionParam("resource")) // Not resource_name, which is valid
}
