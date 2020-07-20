package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/event-feed-service/pkg/feed"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	event "github.com/chef/automate/components/event-service/config"
)

func TestProjectUpdatePainlessElasticsearchScript(t *testing.T) {
	var (
		ctx = context.Background()
	)

	cases := []struct {
		description string
		entry       feed.FeedEntry
		projects    map[string]*authz.ProjectRules
		projectIDs  []string
	}{
		// Orgs
		{
			description: "Org: Single rule matching update",
			entry: feed.FeedEntry{
				ChefOrganization:   "org1",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "chef_server",
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Type: authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "Org: no matching actions",
			entry: feed.FeedEntry{
				ChefOrganization:   "org1",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "chef_server",
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Type: authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org2"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "Org: project rules match current event project tags",
			entry: feed.FeedEntry{
				ChefOrganization:   "org1",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "chef_server",
			},
			projects: map[string]*authz.ProjectRules{
				"old_tag": {
					Rules: []*authz.ProjectRule{
						{
							Type: authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"old_tag"},
		},

		// chefServers
		{
			description: "chefServers: Single rule matching",
			entry: feed.FeedEntry{
				ChefInfraServer:    "chef-server.org",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "chef_server",
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Type: authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server.org"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "chefServers: Single rule not matching",
			entry: feed.FeedEntry{
				ChefInfraServer:    "chef-server2.org",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "chef_server",
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Type: authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server.org"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "chefServers: Single rule differing case not matching",
			entry: feed.FeedEntry{
				ChefInfraServer:    "Chef-server.org",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "chef_server",
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Type: authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server.org"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "chefServers: Single rule with two values on a condition",
			entry: feed.FeedEntry{
				ChefInfraServer:    "chef-server.org",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "chef_server",
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Type: authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server.org", "chef-server2.org"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "chefServers: two rules both matching on different fields",
			entry: feed.FeedEntry{
				ChefInfraServer:    "chef-server.org",
				ChefOrganization:   "org1",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "chef_server",
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Type: authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server.org"},
								},
							},
						},
						{
							Type: authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "chefServers: two rules with only one matching",
			entry: feed.FeedEntry{
				ChefInfraServer:    "chef-server.org",
				ChefOrganization:   "org1",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "chef_server",
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Type: authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server2.org"},
								},
							},
						},
						{
							Type: authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{"project9"},
		},
		{
			description: "chefServers: one rule two conditions with only one matching",
			entry: feed.FeedEntry{
				ChefInfraServer:    "chef-server.org",
				ChefOrganization:   "org1",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "chef_server",
			},
			projects: map[string]*authz.ProjectRules{
				"project9": {
					Rules: []*authz.ProjectRule{
						{
							Type: authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values:    []string{"chef-server2.org"},
								},
								{
									Attribute: authz.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values:    []string{"org1"},
								},
							},
						},
					},
				},
			},
			projectIDs: []string{},
		},

		// General
		{
			description: "project rules does not have any rules",
			entry: feed.FeedEntry{
				ChefInfraServer:    "chef-server.org",
				ChefOrganization:   "org1",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "chef_server",
			},
			projects: map[string]*authz.ProjectRules{
				"old_tag": {
					Rules: []*authz.ProjectRule{},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "project rule does not have any conditions",
			entry: feed.FeedEntry{
				ChefInfraServer:    "chef-server.org",
				ChefOrganization:   "org1",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "chef_server",
			},
			projects: map[string]*authz.ProjectRules{
				"old_tag": {
					Rules: []*authz.ProjectRule{
						{
							Type:       authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{},
						},
					},
				},
			},
			projectIDs: []string{},
		},

		{
			description: "clear all projects from non-chef-server events",
			entry: feed.FeedEntry{
				ChefInfraServer:    "for-testing-only",
				ChefOrganization:   "for-testing-only",
				Projects:           []string{"old_tag"},
				ProducerObjectType: "profile",
			},
			projects: map[string]*authz.ProjectRules{
				"old_tag": {
					Rules: []*authz.ProjectRule{
						{
							Type:       authz.ProjectRuleTypes_EVENT,
							Conditions: []*authz.Condition{},
						},
					},
				},
			},
			projectIDs: []string{},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("match: %s", test.description),
			func(t *testing.T) {

				test.entry.Published = time.Now()
				test.entry.ProducerName = "Fred"
				test.entry.ProducerID = "environment"
				test.entry.ProducerTags = []string{"mycompany", "engineering department", "compliance team"}
				test.entry.FeedType = "event"
				test.entry.EventType = event.ScanJobUpdatedEventName
				test.entry.ActorID = "urn:mycompany:user:fred"
				test.entry.ActorObjectType = "User"
				test.entry.ActorName = "Fred"
				test.entry.Verb = "update"
				test.entry.ObjectID = "urn:chef:compliance:scan-job"
				test.entry.ObjectObjectType = "profile"
				test.entry.ObjectName = "Scan Job"
				test.entry.TargetID = "urn:mycompany:environment:production"
				test.entry.TargetObjectType = "Environment"
				test.entry.TargetName = "Production"
				test.entry.Created = time.Now().UTC()

				testSuite.feedBackend.CreateFeedEntry(&test.entry)

				testSuite.RefreshIndices(persistence.IndexNameFeeds)

				defer testSuite.DeleteAllDocuments()
				// Send a project rules update event
				esJobIDs, err := testSuite.feedBackend.UpdateProjectTags(ctx, test.projects)
				require.Nil(t, err)

				require.Equal(t, 1, len(esJobIDs))

				esJobID := esJobIDs[0]

				jobStatus, err := testSuite.feedBackend.JobStatus(ctx, esJobID)
				require.Nil(t, err)
				for !jobStatus.Completed {
					time.Sleep(time.Millisecond * 5)
					jobStatus, err = testSuite.feedBackend.JobStatus(ctx, esJobID)
					require.Nil(t, err, "testing elasticsearch job complete")
				}

				testSuite.RefreshIndices(persistence.IndexNameFeeds)

				// assert the node's project IDs
				entries, count, err := testSuite.feedBackend.GetFeed(&feed.FeedQuery{
					Size:      100,
					Ascending: false,
				})
				require.Nil(t, err)
				require.Equal(t, 1, int(count), "wrong number of actions retrieved")

				entry := entries[0]

				require.ElementsMatch(t, test.projectIDs, entry.Projects)
			})
	}
}
