//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package backend_test

import (
	"testing"

	subject "github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/stretchr/testify/assert"
)

func TestConstants(t *testing.T) {
	assert.Equal(t, "entity_uuid", subject.Id)
	assert.Equal(t, "node_name", subject.Name)
	assert.Equal(t, "organization_name", subject.Organization)
	assert.Equal(t, "cookbooks", subject.Cookbook)
	assert.Equal(t, "recipes", subject.Recipe)
	assert.Equal(t, "resource_names", subject.ResourceName)
	assert.Equal(t, "attributes", subject.Attribute)
	assert.Equal(t, "roles", subject.Role)
	assert.Equal(t, "service_hostname", subject.ActionSourceFQDN)
	assert.Equal(t, "organization_name", subject.ActionOrganization)
}

func TestNodeFieldToJson(t *testing.T) {
	assert.Equal(t, "entity_uuid", subject.NodeFieldToJson("EntityUuid"))
	assert.Equal(t, "node_name", subject.NodeFieldToJson("NodeName"))
	assert.Equal(t, "fqdn", subject.NodeFieldToJson("Fqdn"))
	assert.Equal(t, "checkin", subject.NodeFieldToJson("Checkin"))
	assert.Equal(t, "uptime_seconds", subject.NodeFieldToJson("UptimeSeconds"))
	assert.Equal(t, "organization_name", subject.NodeFieldToJson("OrganizationName"))
	assert.Equal(t, "environment", subject.NodeFieldToJson("Environment"))
	assert.Equal(t, "platform", subject.NodeFieldToJson("Platform"))
	assert.Equal(t, "platform_family", subject.NodeFieldToJson("PlatformFamily"))
	assert.Equal(t, "platform_version", subject.NodeFieldToJson("PlatformVersion"))
	assert.Equal(t, "policy_name", subject.NodeFieldToJson("PolicyName"))
	assert.Equal(t, "policy_group", subject.NodeFieldToJson("PolicyGroup"))
	assert.Equal(t, "policy_revision", subject.NodeFieldToJson("PolicyRevision"))
	assert.Equal(t, "status", subject.NodeFieldToJson("Status"))
	assert.Equal(t, "latest_run_id", subject.NodeFieldToJson("LatestRunID"))
	assert.Equal(t, "source_fqdn", subject.NodeFieldToJson("SourceFqdn"))
	assert.Equal(t, "chef_version", subject.NodeFieldToJson("ChefVersion"))
	assert.Equal(t, "chef_tags", subject.NodeFieldToJson("ChefTags"))
}

func TestSuggestionFieldArray(t *testing.T) {
	// Array fields
	assert.True(t, subject.SuggestionFieldArray(subject.Role))
	assert.True(t, subject.SuggestionFieldArray(subject.Attribute))
	assert.True(t, subject.SuggestionFieldArray(subject.ResourceName))
	assert.True(t, subject.SuggestionFieldArray(subject.Recipe))
	assert.True(t, subject.SuggestionFieldArray(subject.Cookbook))
	assert.True(t, subject.SuggestionFieldArray(subject.ChefTags))

	// Non Array (string)
	assert.False(t, subject.SuggestionFieldArray("chef_version"))
	assert.False(t, subject.SuggestionFieldArray("environment"))
	assert.False(t, subject.SuggestionFieldArray("organization_name"))
}
