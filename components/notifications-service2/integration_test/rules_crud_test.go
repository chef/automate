package integration_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	api "github.com/chef/automate/api/interservice/notifications/service"
	"github.com/chef/automate/lib/pcmp/passert"
)

func TestRulesCRUDGetVersion(t *testing.T) {
	res, err := suite.Client.Version(ctx, &api.VersionRequest{})
	require.NoError(t, err)
	// In local dev the version string changes to include extra information (in
	// the elixir notifications service). Allow any non-empty string here to be
	// less brittle.
	assert.NotEmpty(t, res.Version)
}

func TestRulesCRUDGetRule(t *testing.T) {
	err := suite.DeleteEverything()
	assert.NoError(t, err)

	t.Run("returns not found when requesting a rule that doesn't exist", func(t *testing.T) {
		res, err := suite.Client.GetRule(ctx, &api.RuleIdentifier{Id: "42"})
		require.NoError(t, err)
		assert.Equal(t, api.RuleGetResponse_NOT_FOUND, res.Code)
		assert.Empty(t, res.Rule)
	})

	t.Run("returns the rule when the rule exists", func(t *testing.T) {
		rule := newValidRule("TestGetRule_success_01")
		createResponse, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		require.NotEmpty(t, createResponse.Id)

		res, err := suite.Client.GetRule(ctx, &api.RuleIdentifier{Id: createResponse.Id})
		require.NoError(t, err)
		assert.Equal(t, api.RuleGetResponse_OK, res.Code)

		rule.Id = createResponse.Id
		passert.Equal(t, rule, res.Rule)
	})
}

func TestRulesCRUDListRules(t *testing.T) {
	err := suite.DeleteEverything()
	require.NoError(t, err)

	t.Run("Returns an empty list when no rules exist", func(t *testing.T) {
		ruleList, err := suite.Client.ListRules(ctx, &api.Empty{})
		require.NoError(t, err)
		assert.Empty(t, ruleList.Rules)
	})

	t.Run("Returns the rules when rules exist", func(t *testing.T) {
		expectedRuleNames := []string{"TestListRules_success_01", "TestListRules_success_02"}
		for _, name := range expectedRuleNames {
			_, err := suite.Client.AddRule(ctx, newValidRule(name))
			require.NoError(t, err)
		}

		ruleList, err := suite.Client.ListRules(ctx, &api.Empty{})
		require.NoError(t, err)
		assert.NotEmpty(t, ruleList.Rules)
		actualRuleNames := []string{}
		for _, r := range ruleList.Rules {
			actualRuleNames = append(actualRuleNames, r.Name)
		}
		assert.ElementsMatch(t, expectedRuleNames, actualRuleNames)
	})
}

func TestRulesCRUDAddRuleSuccessCase(t *testing.T) {
	err := suite.DeleteEverything()
	require.NoError(t, err)

	t.Run("valid SlackAlert rule returns a successful response", func(t *testing.T) {
		rule := newValidRule("TestAddRule_success_01")
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_ADDED, response.Code)
		assert.NotEmpty(t, response.Id)
		passert.Equal(t, rule, response.Rule)
	})
	t.Run("valid ServiceNowAlert rule returns a successful response", func(t *testing.T) {
		rule := newValidRule("TestAddRule_success_02")
		rule.Action = &api.Rule_ServiceNowAlert{
			ServiceNowAlert: &api.ServiceNowAlert{
				Url: "https://valid.example/",
			},
		}
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_ADDED, response.Code)
		assert.NotEmpty(t, response.Id)
		passert.Equal(t, rule, response.Rule)
	})
	t.Run("valid webhook rule returns a successful response", func(t *testing.T) {
		rule := newValidRule("TestAddRule_success_03")
		rule.Action = &api.Rule_WebhookAlert{
			WebhookAlert: &api.WebhookAlert{
				Url: "https://valid.example",
			},
		}
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_ADDED, response.Code)
		assert.NotEmpty(t, response.Id)
		passert.Equal(t, rule, response.Rule)
	})
}

func TestRulesCRUDAddRuleValidations(t *testing.T) {
	err := suite.DeleteEverything()
	require.NoError(t, err)

	t.Run("the Id field must be empty", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_Id")
		rule.Id = "43"
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "Rule ID may not be included in an add-rule request")
	})

	t.Run("the Name field must not be empty", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_Name")
		rule.Name = ""
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "Rule name must be supplied.")
	})

	t.Run("the Name must be unique", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_unique")
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_ADDED, response.Code)
		assert.NotEmpty(t, response.Id)

		response, err = suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_DUPLICATE_NAME, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "A rule with this name already exists")
	})

	t.Run("the Action field must not be empty", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_Action")
		rule.Action = nil
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "Action must be set.")
	})

	t.Run("the URL in a SlackAlert action must not be empty", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_SlackAlertURL")
		rule.GetSlackAlert().Url = ""
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "A valid action URL must be supplied")
	})
	t.Run("the URL in a SlackAlert action must include the protocol/scheme", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_SlackAlertURL")
		rule.GetSlackAlert().Url = "example.com"
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "A valid action URL must be supplied")
	})
	t.Run("the URL in a SlackAlert action must include the hostname", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_SlackAlertURL")
		rule.GetSlackAlert().Url = "https://"
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "A valid action URL must be supplied")
	})

	t.Run("the URL in a WebhookAlert action must not be empty", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_WebhookAlertURL")
		rule.Action = validWebhook()
		rule.GetWebhookAlert().Url = ""
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "A valid action URL must be supplied")
	})
	t.Run("the URL in a WebhookAlert action must include the protocol/scheme", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_WebhookAlertURL")
		rule.Action = validWebhook()
		rule.GetWebhookAlert().Url = "example.com"
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "A valid action URL must be supplied")
	})
	t.Run("the URL in a WebhookAlert action must include the hostname", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_WebhookAlertURL")
		rule.Action = validWebhook()
		rule.GetWebhookAlert().Url = "https://"
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "A valid action URL must be supplied")
	})

	t.Run("the URL in a ServiceNowAlert action must not be empty", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_ServiceNowAlertURL")
		rule.Action = validServiceNowAlert()
		rule.GetServiceNowAlert().Url = ""
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "A valid action URL must be supplied")
	})
	t.Run("the URL in a ServiceNowAlert action must include the protocol/scheme", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_ServiceNowAlertURL")
		rule.Action = validServiceNowAlert()
		rule.GetServiceNowAlert().Url = "example.com"
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "A valid action URL must be supplied")
	})
	t.Run("the URL in a ServiceNowAlert action must include the hostname", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_ServiceNowAlertURL")
		rule.Action = validServiceNowAlert()
		rule.GetServiceNowAlert().Url = "https://"
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "A valid action URL must be supplied")
	})

	t.Run("Multiple error messages are returned when multiple validation rules are not satisfied", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_MuliInvalid")
		rule.Action = nil
		rule.Name = ""
		rule.Id = "42"
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_VALIDATION_ERROR, response.Code)
		assert.Empty(t, response.Id)
		assert.Contains(t, response.Messages, "Action must be set.")
		assert.Contains(t, response.Messages, "Rule name must be supplied.")
		assert.Contains(t, response.Messages, "Rule ID may not be included in an add-rule request")
	})

	// NOTE: this is a test of a validation that doesn't exist
	// NOTE: it's unclear to me if this behavior is correct. It's how the
	// original elixir implementation behaves.
	t.Run("the SecretId in a ServiceNowAlert action can be blank", func(t *testing.T) {
		rule := newValidRule("TestAddRule_validations_ServiceNowAlert_SecretId")
		rule.Action = validServiceNowAlert()
		rule.GetServiceNowAlert().SecretId = ""
		response, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_ADDED, response.Code)
		assert.NotEmpty(t, response.Id)
		assert.Empty(t, response.Messages)
	})

}

func TestRulesCRUDUpdateRuleSuccess(t *testing.T) {
	err := suite.DeleteEverything()
	require.NoError(t, err)

	t.Run("valid rule returns a successful response", func(t *testing.T) {
		rule := newValidRule("TestAddRule_success_01")
		createResponse, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_ADDED, createResponse.Code)

		rule.Id = createResponse.Id
		rule.GetSlackAlert().Url = "https://newurl.example"
		response, err := suite.Client.UpdateRule(ctx, rule)
		require.NoError(t, err)
		assert.Empty(t, response.Messages)
		assert.Equal(t, api.RuleUpdateResponse_OK, response.Code)
	})

	t.Run("update can modify the rule name", func(t *testing.T) {
		rule := newValidRule("TestAddRule_success_02")
		createResponse, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_ADDED, createResponse.Code)

		rule.Id = createResponse.Id
		rule.Name = "TestAddRule_success_02_updated_value"
		response, err := suite.Client.UpdateRule(ctx, rule)
		require.NoError(t, err)
		assert.Empty(t, response.Messages)
		assert.Equal(t, api.RuleUpdateResponse_OK, response.Code)

		updated, err := suite.Client.GetRule(ctx, &api.RuleIdentifier{Id: createResponse.Id})
		require.NoError(t, err)
		require.NotNil(t, updated.Rule)
		assert.Equal(t, "TestAddRule_success_02_updated_value", updated.Rule.Name)
	})

	t.Run("update can modify the action", func(t *testing.T) {
		rule := newValidRule("TestAddRule_success_02")
		createResponse, err := suite.Client.AddRule(ctx, rule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_ADDED, createResponse.Code)

		rule.Id = createResponse.Id
		rule.Action = validServiceNowAlert()

		response, err := suite.Client.UpdateRule(ctx, rule)
		require.NoError(t, err)
		assert.Empty(t, response.Messages)
		assert.Equal(t, api.RuleUpdateResponse_OK, response.Code)

		updated, err := suite.Client.GetRule(ctx, &api.RuleIdentifier{Id: createResponse.Id})
		require.NoError(t, err)
		require.NotNil(t, updated.Rule)
		passert.Equal(t, validServiceNowAlert().ServiceNowAlert, updated.Rule.GetServiceNowAlert())
	})

}

func TestRulesCRUDUpdateRuleValidation(t *testing.T) {
	err := suite.DeleteEverything()
	require.NoError(t, err)

	ruleToCreate := newValidRule("TestUpdateRule_validations")
	created, err := suite.Client.AddRule(ctx, ruleToCreate)
	require.NoError(t, err)
	require.NotNil(t, created.Rule)
	createdRule := created.Rule
	createdRule.Id = created.Id

	t.Run("the Id field must not be empty", func(t *testing.T) {
		updatedRule := copyRule(createdRule)
		updatedRule.Id = ""
		response, err := suite.Client.UpdateRule(ctx, updatedRule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleUpdateResponse_VALIDATION_ERROR, response.Code)
		assert.Contains(t, response.Messages, "Rule ID must be included from the rule being modified")
	})

	t.Run("the Id field must refer to a rule that exists", func(t *testing.T) {
		updatedRule := copyRule(createdRule)
		updatedRule.Id = "42"
		response, err := suite.Client.UpdateRule(ctx, updatedRule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleUpdateResponse_NOT_FOUND, response.Code)
		assert.Contains(t, response.Messages, "The requested rule could not be found")
	})

	t.Run("the Name field must not be empty", func(t *testing.T) {
		updatedRule := copyRule(createdRule)
		updatedRule.Name = ""
		response, err := suite.Client.UpdateRule(ctx, updatedRule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleUpdateResponse_VALIDATION_ERROR, response.Code)
		assert.Contains(t, response.Messages, "Rule name must be supplied.")
	})

	t.Run("the Name must be unique", func(t *testing.T) {
		rule2 := newValidRule("TestAddRule_validations_unique")
		res, err := suite.Client.AddRule(ctx, rule2)
		require.NoError(t, err)
		assert.Equal(t, api.RuleAddResponse_ADDED, res.Code)
		assert.NotEmpty(t, res.Id)

		updatedRule := copyRule(createdRule)
		updatedRule.Name = "TestAddRule_validations_unique"
		response, err := suite.Client.UpdateRule(ctx, updatedRule)
		require.NoError(t, err)
		// NOTE: the elixir implementation does not check for the case that an
		// update would violate the uniqueness constraint on the name column, so it
		// returns an internal error when the db request fails on the constraint
		// violation. This is a bug, but we don't plan to change the behavior until
		// after the elixir implementation is replaced.
		assert.NotEqual(t, api.RuleUpdateResponse_OK, response.Code)
		//assert.Equal(t, api.RuleUpdateResponse_DUPLICATE_NAME, response.Code)
		//assert.Contains(t, response.Messages, "A rule with this name already exists")
	})

	t.Run("the Action field must not be empty", func(t *testing.T) {
		updatedRule := copyRule(createdRule)
		updatedRule.Action = nil
		response, err := suite.Client.UpdateRule(ctx, updatedRule)
		require.NoError(t, err)
		assert.Equal(t, api.RuleUpdateResponse_VALIDATION_ERROR, response.Code)
		assert.Contains(t, response.Messages, "Action must be set.")
	})

	// NOTE: UpdateRule is expected to apply the same validations to the actions
	// as AddRule does. We don't explicitly test those because it's repetitive
}

func TestRulesCRUDDeleteRule(t *testing.T) {
	err := suite.DeleteEverything()
	require.NoError(t, err)

	ruleToCreate := newValidRule("TestUpdateRule_validations")
	created, err := suite.Client.AddRule(ctx, ruleToCreate)
	require.NoError(t, err)

	t.Run("deleting an existing rule succeeds", func(t *testing.T) {
		response, err := suite.Client.DeleteRule(ctx, &api.RuleIdentifier{Id: created.Id})
		require.NoError(t, err)
		assert.Equal(t, api.RuleDeleteResponse_DELETED, response.Code)
	})

	t.Run("deleting a rule that doesn't exist returns an error", func(t *testing.T) {
		response, err := suite.Client.DeleteRule(ctx, &api.RuleIdentifier{Id: "5"})
		require.NoError(t, err)
		assert.Equal(t, api.RuleDeleteResponse_NOT_FOUND, response.Code)
	})
}

func copyRule(r *api.Rule) *api.Rule {
	// NOTE: shallow copy. This will work as long as you only modify r2 by
	// assigning a new value for a field.
	r2 := *r
	return &r2
}

func newValidRule(name string) *api.Rule {
	return &api.Rule{
		Event: api.Rule_CCRFailure,
		Name:  name,
		Action: &api.Rule_SlackAlert{
			SlackAlert: &api.SlackAlert{
				Url: "https://slack.example",
			},
		},
	}
}

func validWebhook() *api.Rule_WebhookAlert {
	return &api.Rule_WebhookAlert{
		WebhookAlert: &api.WebhookAlert{Url: "https://webhook.example"},
	}
}

func validServiceNowAlert() *api.Rule_ServiceNowAlert {
	return &api.Rule_ServiceNowAlert{
		ServiceNowAlert: &api.ServiceNowAlert{
			Url:                  "https://servicenow.example",
			SecretId:             "supersecret",
			CriticalControlsOnly: false,
		},
	}
}
