package v2_test

import (
	"context"
	"reflect"
	"testing"
	"unicode"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/gen"
	"github.com/leanovate/gopter/prop"
	"golang.org/x/text/unicode/rangetable"
	"google.golang.org/grpc/codes"

	api "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/authz-service/testhelpers"
	"github.com/chef/automate/lib/grpc/grpctest"
)

const (
	idRegex = "^[a-z0-9-]{1,64}$"
)

func TestCreateRuleProperties(t *testing.T) {
	ctx := context.Background()
	testFW := testhelpers.NewTestFramework(t, ctx)
	cl := testFW.Projects
	testDB := testFW.TestDB
	seed := testFW.Seed

	graphicRange := rangetable.Merge(unicode.GraphicRanges...)

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
		"Values": gen.SliceOf(gen.UnicodeString(graphicRange)),
	})
	conditionsGenEvent := gen.StructPtr(reflect.TypeOf(&api.Condition{}), map[string]gopter.Gen{
		"Type": gen.OneConstOf(
			api.ProjectRuleConditionAttributes_CHEF_SERVERS,
			api.ProjectRuleConditionAttributes_CHEF_ORGS,
		),
		"Values": gen.SliceOf(gen.UnicodeString(graphicRange)),
	})
	createRuleReqGenEvent := gen.Struct(reflect.TypeOf(&api.CreateRuleReq{}), map[string]gopter.Gen{
		"Id":         gen.RegexMatch(idRegex),
		"Name":       gen.UnicodeString(graphicRange),
		"ProjectId":  gen.RegexMatch(idRegex),
		"Type":       gen.Const(api.ProjectRuleTypes_EVENT),
		"Conditions": gen.SliceOf(conditionsGenEvent),
	})
	createRuleReqGenNode := gen.Struct(reflect.TypeOf(&api.CreateRuleReq{}), map[string]gopter.Gen{
		"Id":         gen.RegexMatch(idRegex),
		"Name":       gen.UnicodeString(graphicRange),
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

	createProjectReqGen := gen.Struct(reflect.TypeOf(&api.CreateProjectReq{}), map[string]gopter.Gen{
		"Id":   gen.RegexMatch(idRegex),
		"Name": gen.UnicodeString(graphicRange),
	})

	type projectAndRuleReq struct {
		api.CreateProjectReq
		rules []api.CreateRuleReq
	}

	params := gopter.DefaultTestParametersWithSeed(seed)
	params.MinSize = 1 // otherwise, we'd get zero-length "conditions" slices
	properties := gopter.NewProperties(params)
	properties.Property("creates a rule in storage", prop.ForAll(
		func(reqs projectAndRuleReq) bool {
			defer testDB.Flush(t)
			_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
			if err != nil {
				t.Error(err.Error())
				return false // bad run
			}

			_, err = cl.CreateRule(ctx, &reqs.rules[0])
			// Note: this could be very noisy, but it's hard to figure out what went
			// wrong without the error
			if err != nil {
				t.Error(err.Error())
				return false // bad run
			}

			r, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
			if err != nil {
				t.Error(err.Error())
				return false
			}

			// Note: we're ignoring type conversion, as asserting that would require us
			// to replicate the mapping logic in tests.
			// Instead, as we add ReadRule, and ListRules etc, methods, we can use this
			// testing approach to write meaningful assertions.
			return len(r.Rule.Conditions) == len(reqs.rules[0].Conditions) &&
				r.Rule.Id == reqs.rules[0].Id &&
				r.Rule.Name == reqs.rules[0].Name &&
				r.Rule.ProjectId == reqs.rules[0].ProjectId
		},
		gopter.CombineGens(createProjectReqGen, createRuleReqGen).Map(func(vals []interface{}) projectAndRuleReq {
			p := vals[0].(api.CreateProjectReq)
			r := vals[1].(api.CreateRuleReq)
			r.ProjectId = p.Id
			return projectAndRuleReq{
				CreateProjectReq: p,
				rules:            []api.CreateRuleReq{r},
			}
		}),
	))
	properties.Property("ensures IDs are unique", prop.ForAll(
		func(reqs projectAndRuleReq) bool {
			defer testDB.Flush(t)
			_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
			if err != nil {
				t.Error(err.Error())
				return false // bad run
			}

			if _, err := cl.CreateRule(ctx, &reqs.rules[0]); err != nil {
				t.Error(err.Error())
				return false
			}

			_, err = cl.CreateRule(ctx, &reqs.rules[1])
			return grpctest.AssertCode(t, codes.AlreadyExists, err)
		},
		// TODO(sr): deduplicate with the combinegen above?
		gopter.CombineGens(createProjectReqGen, createRuleReqGen, createRuleReqGen).Map(func(vals []interface{}) projectAndRuleReq {
			p := vals[0].(api.CreateProjectReq)
			// we "fix" the second req to use the same ID as the first one
			r0 := vals[1].(api.CreateRuleReq)
			r1 := vals[2].(api.CreateRuleReq)
			r1.Id = r0.Id
			r0.ProjectId = p.Id
			r1.ProjectId = p.Id
			return projectAndRuleReq{
				CreateProjectReq: p,
				rules:            []api.CreateRuleReq{r0, r1},
			}
		}),
	))
	properties.TestingRun(t)
	testFW.Shutdown(t, ctx)
}
