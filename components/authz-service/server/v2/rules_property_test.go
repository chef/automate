package v2_test

import (
	"context"
	"reflect"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"
	"google.golang.org/grpc/codes"

	api "github.com/chef/automate/api/interservice/authz/v2"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/lib/grpc/grpctest"
)

const (
	idRegex = "^[a-z0-9-]{1,64}$"
)

func TestCreateRuleProperties(t *testing.T) {
	ctx := context.Background()
	cl, store, _, seed := setupRules(t)

	conditionsGenNode := gen.StructPtr(reflect.TypeOf(&api.Condition{}), map[string]gopter.Gen{
		"Type": gen.OneConstOf(
			api.ProjectRuleConditionAttributes_CHEF_SERVERS,
			api.ProjectRuleConditionAttributes_CHEF_ORGS,
			api.ProjectRuleConditionAttributes_CHEF_ENVIRONMENTS,
			api.ProjectRuleConditionAttributes_ROLES,
			api.ProjectRuleConditionAttributes_CHEF_TAGS,
			api.ProjectRuleConditionAttributes_POLICY_GROUP,
			api.ProjectRuleConditionAttributes_POLICY_NAME,
		),
		"Values": gen.SliceOf(gen.AnyString()),
	})
	conditionsGenEvent := gen.StructPtr(reflect.TypeOf(&api.Condition{}), map[string]gopter.Gen{
		"Type": gen.OneConstOf(
			api.ProjectRuleConditionAttributes_CHEF_SERVERS,
			api.ProjectRuleConditionAttributes_CHEF_ORGS,
		),
		"Values": gen.SliceOf(gen.AnyString()),
	})
	createRuleReqGenEvent := gen.Struct(reflect.TypeOf(&api.CreateRuleReq{}), map[string]gopter.Gen{
		"Id":         gen.RegexMatch(idRegex),
		"Name":       gen.AnyString(),
		"ProjectId":  gen.RegexMatch(idRegex),
		"Type":       gen.Const(api.ProjectRuleTypes_EVENT),
		"Conditions": gen.SliceOf(conditionsGenEvent),
	})
	createRuleReqGenNode := gen.Struct(reflect.TypeOf(&api.CreateRuleReq{}), map[string]gopter.Gen{
		"Id":         gen.RegexMatch(idRegex),
		"Name":       gen.AnyString(),
		"ProjectId":  gen.RegexMatch(idRegex),
		"Type":       gen.Const(api.ProjectRuleTypes_NODE),
		"Conditions": gen.SliceOf(conditionsGenNode),
	})

	createRuleReqGen := gopter.CombineGens(
		gen.Bool(), // event or node?
		createRuleReqGenEvent,
		createRuleReqGenNode,
	).Map(func(values []interface{}) api.CreateRuleReq {
		if values[0].(bool) {
			return values[1].(api.CreateRuleReq)
		} else {
			return values[2].(api.CreateRuleReq)
		}
	})

	params := gopter.DefaultTestParametersWithSeed(seed)
	params.MinSize = 1 // otherwise, we'd get zero-length "conditions" slices
	properties := gopter.NewProperties(params)
	properties.Property("creates a rule in storage", prop.ForAll(
		func(req api.CreateRuleReq) bool {
			defer store.Flush()

			_, err := cl.CreateRule(ctx, &req)

			// Note: this could be very noisy, but it's hard to figure out what went
			// wrong without the error
			if err != nil {
				t.Error(err.Error())
				return false // bad run
			}

			i, found := store.Get(req.Id)
			if !found {
				return false
			}
			r := i.(*storage.Rule)
			// Note: we're ignoring type conversion, as asserting that would require us
			// to replicate the mapping logic in tests.
			// Instead, as we add ReadRule, and ListRules etc, methods, we can use this
			// testing approach to write meaningful assertions.
			return len(r.Conditions) == len(req.Conditions) &&
				r.ID == req.Id &&
				r.Name == req.Name &&
				r.ProjectID == req.ProjectId
		},
		createRuleReqGen,
	))
	properties.Property("ensures IDs are unique", prop.ForAll(
		func(reqs []api.CreateRuleReq) bool {
			defer store.Flush()

			if _, err := cl.CreateRule(ctx, &reqs[0]); err != nil {
				return false
			}

			_, err := cl.CreateRule(ctx, &reqs[1])
			return grpctest.AssertCode(t, codes.AlreadyExists, err)
		},
		gopter.CombineGens(createRuleReqGen, createRuleReqGen).Map(func(vals []interface{}) []api.CreateRuleReq {
			// we "fix" the second req to use the same ID as the first one
			r0 := vals[0].(api.CreateRuleReq)
			r1 := vals[1].(api.CreateRuleReq)
			r1.Id = r0.Id
			return []api.CreateRuleReq{r0, r1}
		}),
	))
	properties.TestingRun(t)
}
