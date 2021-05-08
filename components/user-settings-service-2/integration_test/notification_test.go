package integration_test

import (
	"encoding/json"
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/external/common/query"
	"github.com/chef/automate/api/external/secrets"
	api "github.com/chef/automate/api/interservice/notifications/service"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const exampleURL = "https://slack.example"

var (
	// timestamps for sample data. Use semi-static values so we can match them
	// exactly in test assertions
	startTime = time.Now().UTC().Add(-5 * time.Minute).Round(time.Second)
	endTime   = time.Now().UTC().Round(time.Second)
)

// Test Matrix for Notifications:
// 4 Event Types:
//	 Rule_CCRFailure        Rule_Event = 0
//	 Rule_CCRSuccess        Rule_Event = 1
//	 Rule_ComplianceFailure Rule_Event = 2
//	 Rule_ComplianceSuccess Rule_Event = 3
//
// 3 Alert types (plus additional behaviors within):
// * slack
//   * 1 failed compliance control
//   * > 1 failed control in one profile
//   * failures in multiple profiles
// * webhook
// * servicenow
//
// Dispatch Scenarios:
// * when there are no rules, no alerts are sent
// * when there are rules but the event types don't match, no alerts are sent
// * when there is one rule with a matching event type, one alert is sent
// * when there are multiple rules matching the event type multiple alerts are sent
//
// Prefiltering:
// * alerts are never sent for CCR Success or ComplianceSuccess
// * alerts are never sent if there are no critical control failures in compliance failure
// NOTE: there's a critical controls only setting on the service now alert type
// but this cannot possibly do anything because the prefilter would discard the event first.

func TestNotificationDispatchWithNoRules(t *testing.T) {
	t.Run("with no rules, no alerts are sent", func(t *testing.T) {
		err := suite.DeleteEverything()
		require.NoError(t, err)

		_, err = suite.Client.Notify(ctx, ccrFailureEvent())
		require.NoError(t, err)

		_, err = suite.Client.Notify(ctx, ccrSuccessEvent())
		require.NoError(t, err)

		_, err = suite.Client.Notify(ctx, complianceFailureEvent())
		require.NoError(t, err)

		_, err = suite.Client.Notify(ctx, complianceSuccessEvent())
		require.NoError(t, err)
	})
}

func TestNotificationDispatchWithOneSlackAlertRuleForAllEvents(t *testing.T) {
	err := suite.DeleteEverything()
	require.NoError(t, err)

	ts := newTestServer()
	defer ts.Close()

	// NOTE: the elixir notifications service caches the rules and refreshes them
	// every 5s, which means we have to wait that long for the rules to take
	// effect before we test their behavior. To speed things up, we group tests
	// that can share the same alert rules state.
	addRules(t,
		slackAlertRuleFor(api.Rule_CCRFailure, ts.SlackURL()),
		slackAlertRuleFor(api.Rule_ComplianceFailure, ts.SlackURL()),
		// Success events are unsupported, which we test for below
		slackAlertRuleFor(api.Rule_ComplianceSuccess, ts.SlackURL()),
		slackAlertRuleFor(api.Rule_CCRSuccess, ts.SlackURL()),
	)

	t.Run("no alerts are sent for CCRSuccess, they are always filtered", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, ccrSuccessEvent())
		require.NoError(t, err)
		post := ts.CheckForPost()
		assert.Nil(t, post)
	})

	t.Run("no alerts are sent for ComplianceSuccess, they are always filtered", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, complianceSuccessEvent())
		require.NoError(t, err)
		post := ts.CheckForPost()
		assert.Nil(t, post)
	})

	t.Run("CCRFailure alerts are sent to the Slack URL", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, ccrFailureEvent())
		require.NoError(t, err)
		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)
	})

	t.Run("CCRFailure alerts are properly formatted", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, ccrFailureEvent())
		require.NoError(t, err)
		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)

		var actual SlackMessage
		err = json.Unmarshal(postData.BodyBytes, &actual)
		require.NoError(t, err)
		assert.Equal(t, "Chef Automate", actual.Username)
		assert.Equal(t, "<https://automate.example/nodes/0/runs/1|Chef client run failure on chefnode.example>\n", actual.Text)
		assert.Equal(t, "https://docs.chef.io/images/chef-icon.png", actual.IconURL)
		require.Len(t, actual.Attachments, 1)
		attach := actual.Attachments[0]
		assert.Equal(t, "Example Exception Occurs\n```Error: Chef::ExampleException\nMessage: EXAMPLE EXCEPTION MESSAGE\n(cookbook_example::recipe_example line 13)\nFile: recipe_example\nLine: 13```", attach.Text)
		assert.Equal(t, "", attach.Pretext)
		assert.Equal(t, []string{"text"}, attach.MarkdownIn)
		assert.Equal(t, "Chef client run failed on chefnode.example", attach.Fallback)
		assert.Equal(t, "warning", attach.Color)
		assert.Len(t, attach.Fields, 2)
		expectedField1 := &SlackAttachmentField{
			Value: "chefnode.example",
			Title: "Node",
			Short: false,
		}
		assert.Contains(t, attach.Fields, expectedField1)
		expectedField2 := &SlackAttachmentField{
			Value: "cookbook_example::recipe_example",
			Title: "Cookbook::Recipe",
			Short: false,
		}
		assert.Contains(t, attach.Fields, expectedField2)
	})

	t.Run("ComplianceFailure alerts are sent to the Slack URL", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, complianceFailureEvent())
		require.NoError(t, err)

		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)

	})

	// The formatter for compliance failures behaves differently if there are
	// 1 vs. 2+ failed profiles
	// 1 vs. 2+ failed controls
	// It examines the fields in the data to find critical failed controls as
	// here:
	// https://github.com/chef/automate/blob/5b457d40fb8ec494d69678bc595c264ca4b9bf94/components/notifications-service/server/lib/formatters/utils.ex#L27
	t.Run("ComplianceFailure alerts are formatted correctly when one control fails", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, complianceFailureEvent())
		require.NoError(t, err)

		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)

		var actual SlackMessage
		err = json.Unmarshal(postData.BodyBytes, &actual)
		require.NoError(t, err)
		assert.Equal(t, "Chef Automate", actual.Username)
		assert.Equal(t, "<https://chefautomate.example/compliance_failure/0|InSpec found a critical control failure on node chefnode.example>", actual.Text)
		assert.Equal(t, "https://docs.chef.io/images/chef-icon.png", actual.IconURL)
		require.Len(t, actual.Attachments, 1)
		attach := actual.Attachments[0]
		assert.Equal(t, "```EXAMPLE CONTROL FAILED MESSAGE```\n", attach.Text)
		assert.Equal(t, "", attach.Pretext)
		assert.Equal(t, []string{"text", "pretext"}, attach.MarkdownIn)
		assert.Equal(t, "InSpec critical control failure on node chefnode.example.\nEXAMPLE CONTROL FAILED MESSAGE", attach.Fallback)
		assert.Equal(t, "warning", attach.Color)
		assert.Len(t, attach.Fields, 3)
		expectedField1 := &SlackAttachmentField{
			Value: "Example Control::Simulated Failure",
			Title: "Control ID::Title",
			Short: false,
		}
		assert.Contains(t, attach.Fields, expectedField1)
		expectedField2 := &SlackAttachmentField{
			Value: "Example Profile",
			Title: "Profile",
			Short: false,
		}
		assert.Contains(t, attach.Fields, expectedField2)
		expectedField3 := &SlackAttachmentField{
			Value: "chefnode.example",
			Title: "Node",
			Short: false,
		}
		assert.Contains(t, attach.Fields, expectedField3)
	})
	t.Run("ComplianceFailure alerts are formatted correctly when N > 1 controls fail in one profile", func(t *testing.T) {
		event := complianceFailureEvent()
		cf := event.GetComplianceFailure()
		cf.TestTotals.Failed = 2
		cf.TestTotals.Critical = 2
		cf.TestTotals.CriticalFailed = 2
		profile := cf.FailedProfiles[0]
		profile.Stats.NumTests = 2
		profile.Stats.NumFailedTests = 2

		failedControl2 := &api.Profile_Control{
			Id:     "Example Control",
			Impact: 7.7,
			Title:  "Simulated Failure 2",
			Desc:   "An example control from an integration_test (2)",
			Stats: &api.Profile_Control_ResultTotals{
				NumTests:        2,
				NumFailedTests:  1,
				NumSkippedTests: 0,
				NumPassedTests:  1,
			},
			FailedResults: []*api.Profile_Control_Result{
				&api.Profile_Control_Result{
					Status:   "failed",
					CodeDesc: "not good",
					Message:  "EXAMPLE CONTROL FAILED MESSAGE TWO",
				},
			},
		}

		profile.FailedControls = append(profile.FailedControls, failedControl2)

		_, err := suite.Client.Notify(ctx, event)
		require.NoError(t, err)

		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)

		// uncomment to see the message the service sent
		// fmt.Println(string(postData.BodyBytes))

		var actual SlackMessage
		err = json.Unmarshal(postData.BodyBytes, &actual)
		require.NoError(t, err)
		assert.Equal(t, "Chef Automate", actual.Username)
		assert.Equal(t, "<https://chefautomate.example/compliance_failure/0|InSpec found a critical control failure on node chefnode.example>", actual.Text)
		assert.Equal(t, "https://docs.chef.io/images/chef-icon.png", actual.IconURL)
		require.Len(t, actual.Attachments, 1)
		attach := actual.Attachments[0]
		assert.Equal(t, "```2 of 4 tests failed. View in Chef Automate for full details.```\n", attach.Text)
		assert.Equal(t, "", attach.Pretext)
		assert.Equal(t, []string{"text", "pretext"}, attach.MarkdownIn)
		assert.Equal(t, "InSpec critical control failure on node chefnode.example.\n2 of 4 tests failed. View in Chef Automate for full details.", attach.Fallback)
		assert.Equal(t, "warning", attach.Color)

		assert.Len(t, attach.Fields, 3)
		expectedField1 := &SlackAttachmentField{
			Value: "Multiple",
			Title: "Control ID::Title",
			Short: false,
		}
		assert.Contains(t, attach.Fields, expectedField1)
		expectedField2 := &SlackAttachmentField{
			Value: "Example Profile",
			Title: "Profile",
			Short: false,
		}
		assert.Contains(t, attach.Fields, expectedField2)
		expectedField3 := &SlackAttachmentField{
			Value: "chefnode.example",
			Title: "Node",
			Short: false,
		}
		assert.Contains(t, attach.Fields, expectedField3)
	})
	t.Run("ComplianceFailure alerts are formatted correctly when N > 1 controls fail in M > 1 profiles", func(t *testing.T) {
		event := complianceFailureEvent()
		cf := event.GetComplianceFailure()
		cf.TestTotals.Failed = 2
		cf.TestTotals.Critical = 2
		cf.TestTotals.CriticalFailed = 2

		failedProfile2 := &api.Profile{
			Name:  "Example Profile 2",
			Title: "Example Profile 2",
			Stats: &api.Profile_ControlTotals{
				NumTests:        1,
				NumFailedTests:  1,
				NumSkippedTests: 0,
				NumPassedTests:  0,
			},
			FailedControls: []*api.Profile_Control{
				&api.Profile_Control{
					Id:     "Example Control in Profile 2",
					Impact: 7.5,
					Title:  "Simulated Failure in Profile 2",
					Desc:   "An example control from an integration_test (2)",
					Stats: &api.Profile_Control_ResultTotals{
						NumTests:        2,
						NumFailedTests:  1,
						NumSkippedTests: 0,
						NumPassedTests:  1,
					},
					FailedResults: []*api.Profile_Control_Result{
						&api.Profile_Control_Result{
							Status:   "failed",
							CodeDesc: "not good",
							Message:  "EXAMPLE CONTROL FAILED MESSAGE (2)",
						},
					},
				},
			},
		}
		cf.FailedProfiles = append(cf.FailedProfiles, failedProfile2)

		_, err := suite.Client.Notify(ctx, event)
		require.NoError(t, err)

		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)

		// uncomment to see the message the service sent
		// fmt.Println(string(postData.BodyBytes))

		var actual SlackMessage
		err = json.Unmarshal(postData.BodyBytes, &actual)
		require.NoError(t, err)
		assert.Equal(t, "Chef Automate", actual.Username)
		assert.Equal(t, "<https://chefautomate.example/compliance_failure/0|InSpec found a critical control failure on node chefnode.example>", actual.Text)
		assert.Equal(t, "https://docs.chef.io/images/chef-icon.png", actual.IconURL)
		require.Len(t, actual.Attachments, 1)
		attach := actual.Attachments[0]
		assert.Equal(t, "```2 of 4 tests failed. View in Chef Automate for full details.```\n", attach.Text)
		assert.Equal(t, "", attach.Pretext)
		assert.Equal(t, []string{"text", "pretext"}, attach.MarkdownIn)
		assert.Equal(t, "InSpec critical control failure on node chefnode.example.\n2 of 4 tests failed. View in Chef Automate for full details.", attach.Fallback)
		assert.Equal(t, "warning", attach.Color)

		assert.Len(t, attach.Fields, 3)
		expectedField1 := &SlackAttachmentField{
			Value: "Multiple",
			Title: "Control ID::Title",
			Short: false,
		}
		assert.Contains(t, attach.Fields, expectedField1)
		expectedField2 := &SlackAttachmentField{
			Value: "Multiple",
			Title: "Profile",
			Short: false,
		}
		assert.Contains(t, attach.Fields, expectedField2)
		expectedField3 := &SlackAttachmentField{
			Value: "chefnode.example",
			Title: "Node",
			Short: false,
		}
		assert.Contains(t, attach.Fields, expectedField3)
	})
}

func TestNotificationDispatchWithOneServiceNowAlertRuleForAllEvents(t *testing.T) {
	err := suite.DeleteEverything()
	require.NoError(t, err)

	ts := newTestServer()
	defer ts.Close()

	// NOTE: the elixir notifications service caches the rules and refreshes them
	// every 5s, which means we have to wait that long for the rules to take
	// effect before we test their behavior. To speed things up, we group tests
	// that can share the same alert rules state.
	addRules(t,
		serviceNowAlertRuleFor(api.Rule_CCRFailure, ts.ServiceNowURL(), ""),
		serviceNowAlertRuleFor(api.Rule_ComplianceFailure, ts.ServiceNowURL(), ""),
		// Success events are unsupported, which we test for below
		serviceNowAlertRuleFor(api.Rule_ComplianceSuccess, ts.ServiceNowURL(), ""),
		serviceNowAlertRuleFor(api.Rule_CCRSuccess, ts.ServiceNowURL(), ""),
	)

	t.Run("CCRFailure alerts are sent to the ServiceNow URL", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, ccrFailureEvent())
		require.NoError(t, err)
		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)
	})

	t.Run("CCRFailure alerts are formatted properly", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, ccrFailureEvent())
		require.NoError(t, err)
		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)

		// uncomment to see the message the service sent
		//fmt.Println(string(postData.BodyBytes))

		var actual ServiceNowMessage
		err = json.Unmarshal(postData.BodyBytes, &actual)
		require.NoError(t, err)

		assert.Equal(t, "converge_failure", actual.Type)
		assert.Equal(t, "chefnode.example", actual.NodeName)
		// AutomateFQDN is populated from the A2 config, so it could be anything.
		// In the hab studio, it should be a2-dev.test
		assert.NotEmpty(t, actual.AutomateFQDN)
		assert.Equal(t, "https://automate.example/nodes/0/runs/1", actual.AutomateFailureURL)
		assert.Equal(t, "cookbook_example", actual.Cookbook)
		expectedFailureSnippet :=
			`Chef client run failure on [https://automate.example/nodes/0] chefnode.example : https://automate.example/nodes/0/runs/1
Example Exception Occurs
EXAMPLE EXCEPTION MESSAGE
(cookbook_example::recipe_example line 13) 
`
		assert.Equal(t, expectedFailureSnippet, actual.FailureSnippet)
		assert.Equal(t, "Example Exception Occurs", actual.ExceptionTitle)
		assert.Equal(t, "EXAMPLE EXCEPTION MESSAGE\n(cookbook_example::recipe_example line 13)", actual.ExceptionMessage)
		expectedBT := []string{"/path/to/recipe_example.rb: 42", "/path/to/recipe_example.rb: 23"}
		assert.Equal(t, expectedBT, actual.ExceptionBacktrace)
		assertRFC3339Equal(t, actual.StartTimeUTC, startTime)
		assertRFC3339Equal(t, actual.EndTimeUTC, endTime)
		assertRFC3339Equal(t, actual.TimestampUTC, endTime)
	})
	t.Run("ComplianceFailure alerts are sent to the ServiceNow URL", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, complianceFailureEvent())
		require.NoError(t, err)
		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)
	})
	t.Run("ComplianceFailure alerts are formatted properly", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, complianceFailureEvent())
		require.NoError(t, err)
		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)

		// uncomment to see the message the service sent
		//fmt.Println(string(postData.BodyBytes))

		var actual ServiceNowComplianceFailureMessage
		err = json.Unmarshal(postData.BodyBytes, &actual)
		require.NoError(t, err)

		assert.Equal(t, "compliance_failure", actual.Type)
		assert.Equal(t, "chefnode.example", actual.NodeName)
		assert.Equal(t, "11111111-2222-3333-4444-555555555555", actual.NodeUUID)
		assert.Equal(t, "0", actual.InspecVersion)
		// AutomateFQDN is populated from the A2 config, so it could be anything.
		// In the hab studio, it should be a2-dev.test
		assert.NotEmpty(t, actual.AutomateFQDN)
		assert.Equal(t, "https://chefautomate.example/compliance_failure/0", actual.AutomateFailureURL)
		expectedFailureSnippet := "InSpec found a control failure on [chefnode.example](https://chefautomate.example/compliance_failure/0)"
		assert.Equal(t, expectedFailureSnippet, actual.FailureSnippet)

		assertRFC3339Equal(t, actual.EndTimeUTC, endTime)
		assertRFC3339Equal(t, actual.TimestampUTC, startTime)

		assert.Equal(t, 1, actual.TotalNumberOfTests)
		assert.Equal(t, 0, actual.TotalNumberOfSkippedTests)
		assert.Equal(t, 0, actual.TotalNumberOfPassedTests)
		assert.Equal(t, 1, actual.TotalNumberOfFailedTests)
		assert.Equal(t, 1, actual.NumberOfFailedCriticalTests)
		assert.Equal(t, 1, actual.NumberOfCriticalTests)

		require.Len(t, actual.FailedCriticalProfiles, 1)
		profileActual := actual.FailedCriticalProfiles[0]

		assert.Equal(t, "Example Profile", profileActual.Name)
		assert.Equal(t, "Example Profile", profileActual.Title)
		assert.Equal(t, "An example profile for integration testing", profileActual.Summary)
		assert.Equal(t, "5.23.42", profileActual.Version)
		assert.Equal(t, "123abc", profileActual.SHA256)
		assert.Equal(t, "automate@chef.io", profileActual.Maintainer)
		assert.Equal(t, "Apache 2.0", profileActual.License)
		assert.Equal(t, "Chef Software, Inc.", profileActual.Copyright)
		assert.Equal(t, "legal@chef.io", profileActual.CopyrightEmail)
		assert.Equal(t, 1, profileActual.NumberOfControls)

		require.Len(t, profileActual.Supports, 1)
		supportsActual := profileActual.Supports[0]
		assert.Equal(t, "84", supportsActual.Release)
		assert.Equal(t, "plan9", supportsActual.OSName)
		assert.Equal(t, "Esoterica", supportsActual.OSFamily)
		assert.Equal(t, "99", supportsActual.InSpec)

		require.Len(t, profileActual.Attributes, 1)
		attrActual := profileActual.Attributes[0]

		assert.Equal(t, "example_attr", attrActual.Name)
		assert.Equal(t, "example attribute in an integration test", attrActual.Options.Description)

		require.Len(t, profileActual.Controls, 1)
		controlActual := profileActual.Controls[0]

		assert.Equal(t, "Example Control", controlActual.ID)
		assert.Equal(t, "Simulated Failure", controlActual.Title)
		assert.Equal(t, "An example control from an integration_test", controlActual.Desc)
		assert.Equal(t, "failed", controlActual.Status)
		assert.Equal(t, "halt -p", controlActual.Code)
		assert.Equal(t, 2, controlActual.NumberOfTests)
		assert.Equal(t, 1, controlActual.NumberOfFailedTests)
		assert.Equal(t, 7.5, controlActual.Impact)

		assert.Equal(t, "example_profile.rb", controlActual.SourceLocation.Ref)
		assert.Equal(t, 5, controlActual.SourceLocation.Line)

		require.Len(t, controlActual.Refs, 1)
		refsActual := controlActual.Refs[0]
		assert.Equal(t, "http://compliancerules.example.net", refsActual.URL)
		assert.Equal(t, "http://compliancerules.example", refsActual.URI)

		require.Len(t, controlActual.Results, 1)
		resultActual := controlActual.Results[0]

		assert.Equal(t, "failed", resultActual.Status)
		assert.Equal(t, "EXAMPLE CONTROL FAILED MESSAGE", resultActual.Message)
		assert.Equal(t, "example skip message", resultActual.SkipMessage)
		assert.Equal(t, "not good", resultActual.CodeDesc)
		assert.Equal(t, 3.5, resultActual.RunTime)

		assertRFC3339Equal(t, resultActual.StartTime, startTime)
	})
}

func TestNotificationDispatchWithOneServiceNowAlertRuleWithASecretID(t *testing.T) {
	err := suite.DeleteEverything()
	require.NoError(t, err)

	ts := newTestServer()
	defer ts.Close()

	sres, err := suite.SecretsClient.Create(ctx, &secrets.Secret{
		Id:   "integration_test_secret_id",
		Name: "integration_test_secret_id",
		Type: "service_now",
		Data: []*query.Kv{
			&query.Kv{Key: "username", Value: "integration_test_username_secretstore"},
			&query.Kv{Key: "password", Value: "integration_test_password_secretstore"},
		},
	})
	require.NoError(t, err)
	require.NotEmpty(t, sres.Id)

	secretID := sres.Id
	defer func() {
		suite.SecretsClient.Delete(ctx, &secrets.Id{Id: secretID})
	}()

	// NOTE: the elixir notifications service caches the rules and refreshes them
	// every 5s, which means we have to wait that long for the rules to take
	// effect before we test their behavior. To speed things up, we group tests
	// that can share the same alert rules state.
	addRules(t,
		serviceNowAlertRuleFor(api.Rule_CCRFailure, ts.ServiceNowURL(), secretID),
		serviceNowAlertRuleFor(api.Rule_ComplianceFailure, ts.ServiceNowURL(), secretID),
	)

	t.Run("CCRFailure alerts are sent to the ServiceNow URL with the secret username+password", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, ccrFailureEvent())
		require.NoError(t, err)

		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)
		assert.True(t, postData.BasicAuthUsed)
		assert.Equal(t, "integration_test_username_secretstore", postData.BasicAuthUsername)
		assert.Equal(t, "integration_test_password_secretstore", postData.BasicAuthPassword)
	})

	t.Run("ComplianceFailure alerts are sent to the ServiceNow URL with the secret username+password", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, complianceFailureEvent())
		require.NoError(t, err)

		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)
		assert.True(t, postData.BasicAuthUsed)
		assert.Equal(t, "integration_test_username_secretstore", postData.BasicAuthUsername)
		assert.Equal(t, "integration_test_password_secretstore", postData.BasicAuthPassword)
	})
}

func TestNotificationDispatchWithOneWebhookAlertRuleForAllEvents(t *testing.T) {
	err := suite.DeleteEverything()
	require.NoError(t, err)

	ts := newTestServer()
	defer ts.Close()

	// NOTE: the elixir notifications service caches the rules and refreshes them
	// every 5s, which means we have to wait that long for the rules to take
	// effect before we test their behavior. To speed things up, we group tests
	// that can share the same alert rules state.
	addRules(t,
		webhookAlertRuleFor(api.Rule_CCRFailure, ts.WebhookURL()),
		webhookAlertRuleFor(api.Rule_ComplianceFailure, ts.WebhookURL()),
		// Success events are unsupported, which we test for below
		webhookAlertRuleFor(api.Rule_ComplianceSuccess, ts.WebhookURL()),
		webhookAlertRuleFor(api.Rule_CCRSuccess, ts.WebhookURL()),
	)

	t.Run("CCRFailure alerts are sent to the Webhook URL", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, ccrFailureEvent())
		require.NoError(t, err)
		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)
	})

	t.Run("CCRFailure alerts are formatted properly", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, ccrFailureEvent())
		require.NoError(t, err)
		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)

		// uncomment to see the message the service sent
		//fmt.Println(string(postData.BodyBytes))

		var actual WebhookMessage
		err = json.Unmarshal(postData.BodyBytes, &actual)
		require.NoError(t, err)

		assert.Equal(t, "converge_failure", actual.Type)
		assert.Equal(t, "chefnode.example", actual.NodeName)

		expectedSnippet := "Chef client run failure on [https://automate.example/nodes/0] chefnode.example : https://automate.example/nodes/0/runs/1\nExample Exception Occurs\nEXAMPLE EXCEPTION MESSAGE\n(cookbook_example::recipe_example line 13) \n"
		assert.Equal(t, expectedSnippet, actual.FailureSnippet)
		assert.Equal(t, "Example Exception Occurs", actual.ExceptionTitle)
		assert.Equal(t, "EXAMPLE EXCEPTION MESSAGE\n(cookbook_example::recipe_example line 13)", actual.ExceptionMessage)

		expectedBT := []string{
			"/path/to/recipe_example.rb: 42",
			"/path/to/recipe_example.rb: 23",
		}
		assert.Equal(t, expectedBT, actual.ExceptionBacktrace)
		assert.NotEmpty(t, actual.AutomateFQDN)
		assert.Equal(t, "https://automate.example/nodes/0/runs/1", actual.AutomateFailureURL)

		assertRFC3339Equal(t, actual.StartTimeUTC, startTime)
		assertRFC3339Equal(t, actual.EndTimeUTC, endTime)
	})

	t.Run("ComplianceFailure alerts are sent to the Webhook URL", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, complianceFailureEvent())
		require.NoError(t, err)
		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)
	})
	t.Run("ComplianceFailure alerts are formatted properly", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, complianceFailureEvent())
		require.NoError(t, err)
		postData, err := ts.GetLastPost()
		require.NoError(t, err)
		assert.NotNil(t, postData)

		// uncomment to see the message the service sent
		//fmt.Println(string(postData.BodyBytes))

		var actual WebhookComplianceFailureMessage
		err = json.Unmarshal(postData.BodyBytes, &actual)
		require.NoError(t, err)

		assert.Equal(t, "compliance_failure", actual.Type)
		assert.Equal(t, "chefnode.example", actual.NodeName)
		assert.Equal(t, "11111111-2222-3333-4444-555555555555", actual.NodeUUID)
		assert.Equal(t, "0", actual.InspecVersion)
		// AutomateFQDN is populated from the A2 config, so it could be anything.
		// In the hab studio, it should be a2-dev.test
		assert.NotEmpty(t, actual.AutomateFQDN)
		assert.Equal(t, "https://chefautomate.example/compliance_failure/0", actual.AutomateFailureURL)
		expectedFailureSnippet := "InSpec found a critical control failure on [chefnode.example](https://chefautomate.example/compliance_failure/0)"
		assert.Equal(t, expectedFailureSnippet, actual.FailureSnippet)

		assert.Equal(t, 1, actual.TotalNumberOfTests)
		assert.Equal(t, 0, actual.TotalNumberOfSkippedTests)
		assert.Equal(t, 0, actual.TotalNumberOfPassedTests)
		assert.Equal(t, 1, actual.TotalNumberOfFailedTests)
		assert.Equal(t, 1, actual.NumberOfFailedCriticalTests)
		assert.Equal(t, 1, actual.NumberOfCriticalTests)

		require.Len(t, actual.FailedCriticalProfiles, 1)
		profileActual := actual.FailedCriticalProfiles[0]

		assert.Equal(t, "Example Profile", profileActual.Name)
		assert.Equal(t, "Example Profile", profileActual.Title)
		assert.Equal(t, "An example profile for integration testing", profileActual.Summary)
		assert.Equal(t, "5.23.42", profileActual.Version)
		assert.Equal(t, "123abc", profileActual.SHA256)
		assert.Equal(t, "automate@chef.io", profileActual.Maintainer)
		assert.Equal(t, "Apache 2.0", profileActual.License)
		assert.Equal(t, "Chef Software, Inc.", profileActual.Copyright)
		assert.Equal(t, "legal@chef.io", profileActual.CopyrightEmail)
		assert.Equal(t, 1, profileActual.NumberOfControls)

		require.Len(t, profileActual.Supports, 1)
		supportsActual := profileActual.Supports[0]
		assert.Equal(t, "84", supportsActual.Release)
		assert.Equal(t, "plan9", supportsActual.OSName)
		assert.Equal(t, "Esoterica", supportsActual.OSFamily)
		assert.Equal(t, "99", supportsActual.InSpec)

		require.Len(t, profileActual.Attributes, 1)
		attrActual := profileActual.Attributes[0]

		assert.Equal(t, "example_attr", attrActual.Name)
		assert.Equal(t, "example attribute in an integration test", attrActual.Options.Description)

		require.Len(t, profileActual.Controls, 1)
		controlActual := profileActual.Controls[0]

		assert.Equal(t, "Example Control", controlActual.ID)
		assert.Equal(t, "Simulated Failure", controlActual.Title)
		assert.Equal(t, "An example control from an integration_test", controlActual.Desc)
		assert.Equal(t, "failed", controlActual.Status)
		assert.Equal(t, "halt -p", controlActual.Code)
		assert.Equal(t, 2, controlActual.NumberOfTests)
		assert.Equal(t, 1, controlActual.NumberOfFailedTests)
		assert.Equal(t, 7.5, controlActual.Impact)

		assert.Equal(t, "example_profile.rb", controlActual.SourceLocation.Ref)
		assert.Equal(t, 5, controlActual.SourceLocation.Line)

		require.Len(t, controlActual.Refs, 1)
		refsActual := controlActual.Refs[0]
		assert.Equal(t, "http://compliancerules.example.net", refsActual.URL)
		assert.Equal(t, "http://compliancerules.example", refsActual.URI)

		require.Len(t, controlActual.Results, 1)
		resultActual := controlActual.Results[0]

		assert.Equal(t, "failed", resultActual.Status)
		assert.Equal(t, "EXAMPLE CONTROL FAILED MESSAGE", resultActual.Message)
		assert.Equal(t, "example skip message", resultActual.SkipMessage)
		assert.Equal(t, "not good", resultActual.CodeDesc)
		assert.Equal(t, 3.5, resultActual.RunTime)

		assertRFC3339Equal(t, resultActual.StartTime, startTime)
	})
}

func TestNotificationDispatchWithMultipleDestinations(t *testing.T) {
	err := suite.DeleteEverything()
	require.NoError(t, err)

	ts := newTestServer()
	defer ts.Close()

	addRules(t,
		slackAlertRuleFor(api.Rule_CCRFailure, ts.SlackURL()),
		slackAlertRuleFor(api.Rule_ComplianceFailure, ts.SlackURL()),
		serviceNowAlertRuleFor(api.Rule_CCRFailure, ts.ServiceNowURL(), ""),
		serviceNowAlertRuleFor(api.Rule_ComplianceFailure, ts.ServiceNowURL(), ""),
		webhookAlertRuleFor(api.Rule_CCRFailure, ts.WebhookURL()),
		webhookAlertRuleFor(api.Rule_ComplianceFailure, ts.WebhookURL()),
	)
	t.Run("a CCR failure sends one alert to each destination", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, ccrFailureEvent())
		require.NoError(t, err)
		actualURLPaths := make(map[string]bool)
		for i := 0; i < 3; i++ {
			postData, err := ts.GetLastPost()
			require.NoError(t, err)
			assert.NotNil(t, postData)
			actualURLPaths[postData.URL] = true
		}
		expectedURLPaths := map[string]bool{
			"/slackalert/":      true,
			"/servicenowalert/": true,
			"/webhookalert/":    true,
		}

		assert.Equal(t, expectedURLPaths, actualURLPaths)

	})
	t.Run("a compliance failure sends one alert to each destination", func(t *testing.T) {
		_, err := suite.Client.Notify(ctx, complianceFailureEvent())
		require.NoError(t, err)
		actualURLPaths := make(map[string]bool)
		for i := 0; i < 3; i++ {
			postData, err := ts.GetLastPost()
			require.NoError(t, err)
			assert.NotNil(t, postData)
			actualURLPaths[postData.URL] = true
		}
		expectedURLPaths := map[string]bool{
			"/slackalert/":      true,
			"/servicenowalert/": true,
			"/webhookalert/":    true,
		}

		assert.Equal(t, expectedURLPaths, actualURLPaths)
	})
}

// assertRFC3339Equal asserts that the given RFC3339 formatted time string
// matches the expectedTime. A string comparison is likely to fail
// inadvertently because of how the floating point value of the fractional
// seconds is converted to a string in different programming languages. E.g.,
// trying to compare the strings can result in something like this:
//   expected:"2021-01-29T19:11:09Z"
//   actual:  "2021-01-29T19:11:09.000000Z"
// This function converts the string to a time object before comparing. It's
// recommended to truncate your time values to the nearest second to avoid
// floating point shenanigans.
func assertRFC3339Equal(t *testing.T, rfc3339str string, expectedTime time.Time) {
	parsedValue, err := time.Parse(time.RFC3339Nano, rfc3339str)
	assert.NoError(t, err)
	assert.Equal(t, expectedTime, parsedValue)
}

func addRules(t *testing.T, rules ...*api.Rule) {
	for _, r := range rules {
		createResponse, err := suite.Client.AddRule(ctx, r)
		require.NoError(t, err)
		require.Equal(t, api.RuleAddResponse_ADDED, createResponse.Code)
	}
	// elixir implementation keeps a cache of the rules which it refreshes every 5s
	time.Sleep(6 * time.Second)
}

func slackAlertRuleFor(re api.Rule_Event, url string) *api.Rule {
	return &api.Rule{
		Event: re,
		Name:  fmt.Sprintf("SlackAlert_%s", re),
		Action: &api.Rule_SlackAlert{
			SlackAlert: &api.SlackAlert{
				Url: url,
			},
		},
	}
}

func serviceNowAlertRuleFor(re api.Rule_Event, url, secretID string) *api.Rule {
	return &api.Rule{
		Event: re,
		Name:  fmt.Sprintf("ServiceNowAlert_%s", re),
		Action: &api.Rule_ServiceNowAlert{
			ServiceNowAlert: &api.ServiceNowAlert{
				Url:      url,
				SecretId: secretID,
			},
		},
	}
}

func webhookAlertRuleFor(re api.Rule_Event, url string) *api.Rule {
	return &api.Rule{
		Event: re,
		Name:  fmt.Sprintf("WebhookAlert_%s", re),
		Action: &api.Rule_WebhookAlert{
			WebhookAlert: &api.WebhookAlert{
				Url: url,
			},
		},
	}
}

func ccrFailureEvent() *api.Event {
	return &api.Event{
		Id: newEventID(),
		Event: &api.Event_CCRFailure{
			CCRFailure: &api.CCRFailure{
				RunId:    "0",
				RunUrl:   "https://automate.example/nodes/0/runs/1",
				NodeName: "chefnode.example",
				NodeUrl:  "https://automate.example/nodes/0",
				Cookbook: "cookbook_example",
				Recipe:   "recipe_example",
				Exception: &api.ExceptionInfo{
					Title: "Example Exception Occurs",
					Class: "Chef::ExampleException",
					Msg:   "EXAMPLE EXCEPTION MESSAGE\n(cookbook_example::recipe_example line 13)",
					Backtrace: []string{
						"/path/to/recipe_example.rb: 42",
						"/path/to/recipe_example.rb: 23",
					},
				},
				Time: &api.TimeInfo{
					StartTime: startTime.Format(time.RFC3339),
					EndTime:   endTime.Format(time.RFC3339),
				},
				Timestamp: endTime.Format(time.RFC3339),
			},
		},
	}
}

func ccrSuccessEvent() *api.Event {
	return &api.Event{
		Id: newEventID(),
		Event: &api.Event_CCRSuccess{
			CCRSuccess: &api.CCRSuccess{},
		},
	}
}

func complianceFailureEvent() *api.Event {
	return &api.Event{
		Id: newEventID(),
		Event: &api.Event_ComplianceFailure{
			ComplianceFailure: &api.ComplianceFailure{
				Id:            "0",
				NodeName:      "chefnode.example",
				NodeId:        "11111111-2222-3333-4444-555555555555",
				InspecVersion: "0",
				EndTime:       endTime.Format(time.RFC3339),
				Timestamp:     startTime.Format(time.RFC3339),
				ComplianceUrl: "https://chefautomate.example/compliance_failure/0",
				TestTotals: &api.ComplianceFailure_ControlTotals{
					Skipped:        0,
					Passed:         0,
					Failed:         1,
					Critical:       1,
					CriticalFailed: 1,
				},
				FailedProfiles: []*api.Profile{
					&api.Profile{
						Name:           "Example Profile",
						Title:          "Example Profile",
						Version:        "5.23.42",
						Summary:        "An example profile for integration testing",
						Maintainer:     "automate@chef.io",
						Copyright:      "Chef Software, Inc.",
						CopyrightEmail: "legal@chef.io",
						License:        "Apache 2.0",
						Sha256:         "123abc",
						Supports: []*api.PlatformSupport{
							&api.PlatformSupport{
								Inspec:   "99",
								OsName:   "plan9",
								OsFamily: "Esoterica",
								Release:  "84",
							},
						},
						Attributes: []*api.Profile_Attribute{
							&api.Profile_Attribute{
								Name: "example_attr",
								Options: &api.Profile_Attribute_Options{
									Description: "example attribute in an integration test",
								},
							},
						},
						Stats: &api.Profile_ControlTotals{
							NumTests:        1,
							NumFailedTests:  1,
							NumSkippedTests: 0,
							NumPassedTests:  0,
						},
						FailedControls: []*api.Profile_Control{
							&api.Profile_Control{
								Id:     "Example Control",
								Impact: 7.5,
								Title:  "Simulated Failure",
								Desc:   "An example control from an integration_test",
								Refs: []*api.Refs{
									&api.Refs{Uri: "http://compliancerules.example", Url: "http://compliancerules.example.net"},
								},
								Code:           "halt -p",
								SourceLocation: &api.SourceLocation{Ref: "example_profile.rb", Line: 5},
								Stats: &api.Profile_Control_ResultTotals{
									NumTests:        2,
									NumFailedTests:  1,
									NumSkippedTests: 0,
									NumPassedTests:  1,
								},
								FailedResults: []*api.Profile_Control_Result{
									&api.Profile_Control_Result{
										Status:      "failed",
										CodeDesc:    "not good",
										Message:     "EXAMPLE CONTROL FAILED MESSAGE",
										SkipMessage: "example skip message",
										StartTime:   startTime.Format(time.RFC3339),
										RunTime:     3.5,
									},
								},
							},
						},
					},
				},
			},
		},
	}
}

func complianceSuccessEvent() *api.Event {
	return &api.Event{
		Id: newEventID(),
		Event: &api.Event_ComplianceSuccess{
			ComplianceSuccess: &api.ComplianceSuccess{},
		},
	}
}

func newEventID() string {
	return fmt.Sprintf("%d", time.Now().UnixNano())
}

type SlackMessage struct {
	Username    string             `json:"username"`
	Text        string             `json:"text"`
	IconURL     string             `json:"icon_url"`
	Attachments []*SlackAttachment `json:"attachments"`
}

type SlackAttachment struct {
	Text       string                  `json:"text"`
	Pretext    string                  `json:"pretext"`
	MarkdownIn []string                `json:"mrkdwn_in"`
	Fallback   string                  `json:"fallback"`
	Color      string                  `json:"color"`
	Fields     []*SlackAttachmentField `json:"fields"`
}

type SlackAttachmentField struct {
	Value string `json:"value"`
	Title string `json:"title"`
	Short bool   `json:"short"`
}

type ServiceNowMessage struct {
	Type               string   `json:"type"`
	NodeName           string   `json:"node_name"`
	AutomateFQDN       string   `json:"automate_fqdn"`
	AutomateFailureURL string   `json:"automate_failure_url"`
	TimestampUTC       string   `json:"timestamp_utc"`
	StartTimeUTC       string   `json:"start_time_utc"`
	EndTimeUTC         string   `json:"end_time_utc"`
	FailureSnippet     string   `json:"failure_snippet"`
	ExceptionTitle     string   `json:"exception_title"`
	ExceptionMessage   string   `json:"exception_message"`
	Cookbook           string   `json:"cookbook"`
	ExceptionBacktrace []string `json:"exception_backtrace"`
}

type ServiceNowComplianceFailureMessage struct {
	Type                        string                     `json:"type"`
	NodeName                    string                     `json:"node_name"`
	NodeUUID                    string                     `json:"node_uuid"`
	InspecVersion               string                     `json:"inspec_version"`
	AutomateFQDN                string                     `json:"automate_fqdn"`
	AutomateFailureURL          string                     `json:"automate_failure_url"`
	TotalNumberOfTests          int                        `json:"total_number_of_tests"`
	TotalNumberOfSkippedTests   int                        `json:"total_number_of_skipped_tests"`
	TotalNumberOfPassedTests    int                        `json:"total_number_of_passed_tests"`
	TotalNumberOfFailedTests    int                        `json:"total_number_of_failed_tests"`
	NumberOfFailedCriticalTests int                        `json:"number_of_failed_critical_tests"`
	NumberOfCriticalTests       int                        `json:"number_of_critical_tests"`
	FailureSnippet              string                     `json:"failure_snippet"`
	FailedCriticalProfiles      []*ServiceNowFailedProfile `json:"failed_critical_profiles"`
	TimestampUTC                string                     `json:"timestamp_utc"`
	EndTimeUTC                  string                     `json:"end_time_utc"`
}

type ServiceNowFailedProfile struct {
	Name             string                              `json:"name"`
	Title            string                              `json:"title"`
	Summary          string                              `json:"summary"`
	Version          string                              `json:"version"`
	Maintainer       string                              `json:"maintainer"`
	License          string                              `json:"license"`
	Copyright        string                              `json:"copyright"`
	CopyrightEmail   string                              `json:"copyright_email"`
	SHA256           string                              `json:"sha256"`
	NumberOfControls int                                 `json:"number_of_controls"`
	Supports         []*ServiceNowFailedProfilePlatform  `json:"supports"`
	Controls         []*ServiceNowFailedControl          `json:"controls"`
	Attributes       []*ServiceNowFailedProfileAttribute `json:"attributes"`
}

type ServiceNowFailedProfilePlatform struct {
	InSpec   string `json:"inspec"`
	OSName   string `json:"os_name"`
	OSFamily string `json:"os_family"`
	Release  string `json:"release"`
}

type ServiceNowFailedProfileAttribute struct {
	Name    string                                   `json:"name"`
	Options *ServiceNowFailedProfileAttributeOptions `json:"options"`
}

type ServiceNowFailedProfileAttributeOptions struct {
	Description string `json:"description"`
}

type ServiceNowFailedControl struct {
	ID                  string                                 `json:"id"`
	Title               string                                 `json:"title"`
	Desc                string                                 `json:"desc"`
	Status              string                                 `json:"status"`
	Impact              float64                                `json:"impact"`
	NumberOfTests       int                                    `json:"number_of_tests"`
	NumberOfFailedTests int                                    `json:"number_of_failed_tests"`
	SourceLocation      *ServiceNowFailedControlSourceLocation `json:"source_location"`
	Code                string                                 `json:"code"`
	Refs                []*ServiceNowFailedControlRefs         `json:"refs"`
	Results             []*ServiceNowResult                    `json:"results"`
}

type ServiceNowFailedControlSourceLocation struct {
	Ref  string `json:"ref"`
	Line int    `json:"line"`
}

type ServiceNowFailedControlRefs struct {
	URI string `json:"uri"`
	URL string `json:"url"`
}

type ServiceNowResult struct {
	Status      string  `json:"status"`
	Message     string  `json:"message"`
	SkipMessage string  `json:"skip_message"`
	CodeDesc    string  `json:"code_desc"`
	RunTime     float64 `json:"run_time"`
	StartTime   string  `json:"start_time"`
}

type WebhookMessage struct {
	Type               string   `json:"type"`
	NodeName           string   `json:"node_name"`
	FailureSnippet     string   `json:"failure_snippet"`
	ExceptionTitle     string   `json:"exception_title"`
	ExceptionMessage   string   `json:"exception_message"`
	ExceptionBacktrace []string `json:"exception_backtrace"`
	AutomateFQDN       string   `json:"automate_fqdn"`
	AutomateFailureURL string   `json:"automate_failure_url"`
	StartTimeUTC       string   `json:"start_time_utc"`
	EndTimeUTC         string   `json:"end_time_utc"`
}

type WebhookComplianceFailureMessage struct {
	Type                        string                  `json:"type"`
	NodeName                    string                  `json:"node_name"`
	NodeUUID                    string                  `json:"node_uuid"`
	InspecVersion               string                  `json:"inspec_version"`
	AutomateFQDN                string                  `json:"automate_fqdn"`
	AutomateFailureURL          string                  `json:"automate_failure_url"`
	TotalNumberOfTests          int                     `json:"total_number_of_tests"`
	TotalNumberOfSkippedTests   int                     `json:"total_number_of_skipped_tests"`
	TotalNumberOfPassedTests    int                     `json:"total_number_of_passed_tests"`
	TotalNumberOfFailedTests    int                     `json:"total_number_of_failed_tests"`
	NumberOfFailedCriticalTests int                     `json:"number_of_failed_critical_tests"`
	NumberOfCriticalTests       int                     `json:"number_of_critical_tests"`
	FailureSnippet              string                  `json:"failure_snippet"`
	FailedCriticalProfiles      []*WebhookFailedProfile `json:"failed_critical_profiles"`
}

type WebhookFailedProfile struct {
	Name             string                           `json:"name"`
	Title            string                           `json:"title"`
	Summary          string                           `json:"summary"`
	Version          string                           `json:"version"`
	Maintainer       string                           `json:"maintainer"`
	License          string                           `json:"license"`
	Copyright        string                           `json:"copyright"`
	CopyrightEmail   string                           `json:"copyright_email"`
	SHA256           string                           `json:"sha256"`
	NumberOfControls int                              `json:"number_of_controls"`
	Supports         []*WebhookFailedProfilePlatform  `json:"supports"`
	Controls         []*WebhookFailedControl          `json:"controls"`
	Attributes       []*WebhookFailedProfileAttribute `json:"attributes"`
}

type WebhookFailedProfilePlatform struct {
	InSpec   string `json:"inspec"`
	OSName   string `json:"os_name"`
	OSFamily string `json:"os_family"`
	Release  string `json:"release"`
}

type WebhookFailedProfileAttribute struct {
	Name    string                                `json:"name"`
	Options *WebhookFailedProfileAttributeOptions `json:"options"`
}

type WebhookFailedProfileAttributeOptions struct {
	Description string `json:"description"`
}

type WebhookFailedControl struct {
	ID                  string                              `json:"id"`
	Title               string                              `json:"title"`
	Desc                string                              `json:"desc"`
	Status              string                              `json:"status"`
	Impact              float64                             `json:"impact"`
	NumberOfTests       int                                 `json:"number_of_tests"`
	NumberOfFailedTests int                                 `json:"number_of_failed_tests"`
	SourceLocation      *WebhookFailedControlSourceLocation `json:"source_location"`
	Code                string                              `json:"code"`
	Refs                []*WebhookFailedControlRefs         `json:"refs"`
	Results             []*WebhookResult                    `json:"results"`
}

type WebhookFailedControlSourceLocation struct {
	Ref  string `json:"ref"`
	Line int    `json:"line"`
}

type WebhookFailedControlRefs struct {
	URI string `json:"uri"`
	URL string `json:"url"`
}

type WebhookResult struct {
	Status      string  `json:"status"`
	Message     string  `json:"message"`
	SkipMessage string  `json:"skip_message"`
	CodeDesc    string  `json:"code_desc"`
	RunTime     float64 `json:"run_time"`
	StartTime   string  `json:"start_time"`
}
