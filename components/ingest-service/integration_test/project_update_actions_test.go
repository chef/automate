package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/require"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	automate_event_type "github.com/chef/automate/components/event-service/server"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	project_update_tags "github.com/chef/automate/lib/authz"
	"github.com/golang/mock/gomock"
	"github.com/golang/protobuf/ptypes"
	_struct "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"
)

func TestProjectUpdateActionsPainlessElasticsearchScript(t *testing.T) {
	var (
		ctx = context.Background()
	)

	cases := []struct {
		description string
		action      iBackend.InternalChefAction
		projects    map[string]*iam_v2.ProjectRules
		projectIDs  []string
	}{
		// Orgs
		{
			description: "Org: Single rule matching update",
			action: iBackend.InternalChefAction{
				OrganizationName: "org1",
				Projects:         []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
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
			action: iBackend.InternalChefAction{
				OrganizationName: "org1",
				Projects:         []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
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
			description: "Org: project rules match current action project tags",
			action: iBackend.InternalChefAction{
				OrganizationName: "org1",
				Projects:         []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"old_tag": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Attribute: iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
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
			action: iBackend.InternalChefAction{
				RemoteHostname: "chef-server.org",
				Projects:       []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values: []string{"chef-server.org"},
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
			action: iBackend.InternalChefAction{
				RemoteHostname: "chef-server2.org",
				Projects:       []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values: []string{"chef-server.org"},
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
			action: iBackend.InternalChefAction{
				RemoteHostname: "Chef-server.org",
				Projects:       []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values: []string{"chef-server.org"},
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
			action: iBackend.InternalChefAction{
				RemoteHostname: "chef-server.org",
				Projects:       []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values: []string{"chef-server.org", "chef-server2.org"},
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
			action: iBackend.InternalChefAction{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org1",
				Projects:         []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values: []string{"chef-server.org"},
								},
							},
						},
						{
							Conditions: []*iam_v2.Condition{
								{
									Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values: []string{"org1"},
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
			action: iBackend.InternalChefAction{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org1",
				Projects:         []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values: []string{"chef-server2.org"},
								},
							},
						},
						{
							Conditions: []*iam_v2.Condition{
								{
									Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values: []string{"org1"},
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
			action: iBackend.InternalChefAction{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org1",
				Projects:         []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"project9": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{
								{
									Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_SERVER,
									Values: []string{"chef-server2.org"},
								},
								{
									Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values: []string{"org1"},
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
			action: iBackend.InternalChefAction{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org1",
				Projects:         []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"old_tag": {
					Rules: []*iam_v2.ProjectRule{},
				},
			},
			projectIDs: []string{},
		},
		{
			description: "project rule does not have any conditions",
			action: iBackend.InternalChefAction{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org1",
				Projects:         []string{"old_tag"},
			},
			projects: map[string]*iam_v2.ProjectRules{
				"old_tag": {
					Rules: []*iam_v2.ProjectRule{
						{
							Conditions: []*iam_v2.Condition{},
						},
					},
				},
			},
			projectIDs: []string{},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("action match: %s", test.description),
			func(t *testing.T) {
				test.action.RecordedAt = time.Now()
				suite.IngestActions([]iBackend.InternalChefAction{test.action})
				defer suite.DeleteAllDocuments()
				// Send a project rules update event
				esJobID, err := suite.ingest.UpdateActionProjectTags(ctx, test.projects)
				require.Nil(t, err)

				jobStatus, err := suite.ingest.JobStatus(ctx, esJobID)
				require.Nil(t, err)
				for !jobStatus.Completed {
					time.Sleep(time.Millisecond * 5)
					jobStatus, err = suite.ingest.JobStatus(ctx, esJobID)
					require.Nil(t, err, "testing elasticsearch job complete")
				}

				suite.RefreshIndices(fmt.Sprintf("%s-%s", mappings.Actions.Index, "*"))

				// assert the node's project IDs
				actualActions, err := suite.GetActions(100)
				require.Nil(t, err)
				require.Equal(t, 1, len(actualActions), "wrong number of actions retrieved")

				actualAction := actualActions[0]

				require.ElementsMatch(t, test.projectIDs, actualAction.Projects)
			})
	}
}

// Test if action are being updated when a project update is ran.
//
// Add action with default project tag
// run update
// test if the action project tag was update
func TestProjectUpdateRunsOnActions(t *testing.T) {

	projectID := "1e27fff8-c78b-4f11-9a2a-ae993a642c83"
	updatedProjectTag := "project9"
	var eventsSent []*automate_event.EventMsg
	localSuite := NewLocalSuite(t)
	defer localSuite.GlobalTeardown()

	localSuite.eventServiceClientMock.EXPECT().Publish(gomock.Any(), gomock.Any()).AnyTimes().DoAndReturn(
		func(ctx context.Context, in *automate_event.PublishRequest) (*automate_event.PublishResponse, error) {
			eventsSent = append(eventsSent, in.Msg)
			return &automate_event.PublishResponse{}, nil
		})
	localSuite.projectsClient.EXPECT().ListRulesForAllProjects(gomock.Any(), gomock.Any()).AnyTimes().Return(
		&iam_v2.ListRulesForAllProjectsResp{
			ProjectRules: map[string]*iam_v2.ProjectRules{
				updatedProjectTag: {
					Rules: []*iam_v2.ProjectRule{
						{
							Type: iam_v2.ProjectRuleTypes_EVENT,
							Conditions: []*iam_v2.Condition{
								{
									Attribute:   iam_v2.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
									Values: []string{"org1"},
								},
							},
						},
					},
				},
			},
		}, nil)

	// Add action with default project tag
	actions := []iBackend.InternalChefAction{
		{
			OrganizationName: "org1",
			Projects:         []string{"old_tag"},
			RecordedAt:       time.Now(),
		},
	}
	suite.IngestActions(actions)
	defer suite.DeleteAllDocuments()

	event := &automate_event.EventMsg{
		EventID:   "5462ebf6-3b5b-40f8-b308-fda4745ce49f",
		Type:      &automate_event.EventType{Name: automate_event_type.ProjectRulesUpdate},
		Published: ptypes.TimestampNow(),
		Data: &_struct.Struct{
			Fields: map[string]*_struct.Value{
				project_update_tags.ProjectUpdateIDTag: {
					Kind: &_struct.Value_StringValue{
						StringValue: projectID,
					},
				},
			},
		},
	}

	// run update
	_, err := localSuite.EventHandlerServer.HandleEvent(context.Background(), event)
	assert.NoError(t, err)

	complete := false
	// Wait for job to complete
	for !complete {
		time.Sleep(time.Millisecond * 100)

		for _, event := range eventsSent {
			assert.Equal(t, event.Type.Name, automate_event_type.ProjectRulesUpdateStatus)
			if event.Data.Fields["Completed"].GetBoolValue() {
				complete = true
			}
		}
	}

	suite.RefreshIndices(suite.Indices()...)

	// Test if the action was updated
	actualActions, err := suite.GetActions(100)
	require.Nil(t, err)
	require.Equal(t, 1, len(actualActions), "wrong number of actions retrieved")

	actualAction := actualActions[0]

	require.ElementsMatch(t, []string{updatedProjectTag}, actualAction.Projects)
}
