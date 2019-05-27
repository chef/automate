package v2_test

import (
	"context"
	"reflect"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"

	api "github.com/chef/automate/api/interservice/authz/v2"
)

func TestCreateRuleCreatesRulesInStorage(t *testing.T) {
	ctx := context.Background()
	cl, store, _, seed := setupRules(t)

	// conditionsGenNode := gen.StructPtr(reflect.TypeOf(&api.Condition{}), map[string]gopter.Gen{
	// 	"Type": gen.OneConstOf(
	// 		api.ProjectRuleConditionTypes_CHEF_SERVERS,
	// 		api.ProjectRuleConditionTypes_CHEF_ORGS,
	// 		api.ProjectRuleConditionTypes_CHEF_ENVIRONMENTS,
	// 		api.ProjectRuleConditionTypes_ROLES,
	// 		api.ProjectRuleConditionTypes_CHEF_TAGS,
	// 		api.ProjectRuleConditionTypes_POLICY_GROUP,
	// 		api.ProjectRuleConditionTypes_POLICY_NAME,
	// 	),
	// 	"Values": gen.SliceOf(gen.AnyString()),
	// })
	conditionsGenEvent := gen.StructPtr(reflect.TypeOf(&api.Condition{}), map[string]gopter.Gen{
		"Type": gen.OneConstOf(
			api.ProjectRuleConditionTypes_CHEF_SERVERS,
			api.ProjectRuleConditionTypes_CHEF_ORGS,
		),
		"Values": gen.SliceOf(gen.AnyString()),
	})
	createRuleReqGen := gen.Struct(reflect.TypeOf(&api.CreateRuleReq{}), map[string]gopter.Gen{
		"Id":        gen.RegexMatch("^[a-z0-9-]{1,64}$"),
		"Name":      gen.AnyString(),
		"ProjectId": gen.RegexMatch("^[a-z0-9-]{1,64}$"),
		"Type": gen.OneConstOf(
			api.ProjectRuleTypes_NODE,
			api.ProjectRuleTypes_EVENT,
		),
		"Conditions": gen.SliceOf(conditionsGenEvent),
	})

	params := gopter.DefaultTestParametersWithSeed(seed)
	params.MinSize = 1
	properties := gopter.NewProperties(params)
	properties.Property("creates a rule in storage", prop.ForAll(
		func(req api.CreateRuleReq) bool {
			store.Flush()
			_, err := cl.CreateRule(ctx, &req)
			good := err == nil

			// Note: this could be very noisy, but it's hard to figure out what went
			// wrong without the error
			if !good {
				t.Error(err.Error())
			}
			return good
		},
		createRuleReqGen,
	))
	properties.TestingRun(t)
}
