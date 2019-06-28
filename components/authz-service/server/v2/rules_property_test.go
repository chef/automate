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

type projectAndRuleReq struct {
	api.CreateProjectReq
	rules []api.CreateRuleReq
}

func TestCreateRuleProperties(t *testing.T) {
	ctx := context.Background()
	cl, testDB, _, _, seed := testhelpers.SetupProjectsAndRulesWithDB(t)
	properties := getGopterParams(seed)
	createRuleReqGen, createProjectReqGen, createProjectAndRuleGen := getGenerators()

	properties.Property("creates a staged rule", prop.ForAll(
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

			rStaged, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
			if err != nil {
				t.Error(err.Error())
				return false
			}

			// Note: we're ignoring type conversion, as asserting that would require us
			// to replicate the mapping logic in tests.
			// Instead, as we add GetRule, ListRules, etc., we can use this
			// testing approach to write meaningful assertions.
			return rStaged.Rule.Status == "staged" &&
				RuleMatches(reqs.rules[0], *rStaged.Rule)
		},
		createProjectAndRuleGen,
	))

	properties.Property("creates a staged rule and applies it", prop.ForAll(
		func(reqs projectAndRuleReq) bool {
			defer testDB.Flush(t)

			_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
			if err != nil {
				t.Error(err.Error())
				return false
			}

			_, err = cl.CreateRule(ctx, &reqs.rules[0])
			if err != nil {
				t.Error(err.Error())
				return false
			}

			cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

			rApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
			if err != nil {
				t.Error(err.Error())
				return false
			}

			return rApplied.Rule.Status == "applied" &&
				RuleMatches(reqs.rules[0], *rApplied.Rule)
		},
		createProjectAndRuleGen,
	))

	properties.Property("updates a staged rule and applies update", prop.ForAll(
		func(reqs projectAndRuleReq) bool {
			defer testDB.Flush(t)

			_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
			if err != nil {
				t.Error(err.Error())
				return false
			}

			_, err = cl.CreateRule(ctx, &reqs.rules[0])
			if err != nil {
				t.Error(err.Error())
				return false
			}

			updateReq := api.UpdateRuleReq{
				Id:         reqs.rules[0].Id,
				ProjectId:  reqs.rules[0].ProjectId,
				Name:       reqs.rules[0].Name + " updated",
				Type:       reqs.rules[0].Type,
				Conditions: reqs.rules[0].Conditions,
			}
			rUpdated, err := cl.UpdateRule(ctx, &updateReq)

			cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

			rApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
			if err != nil {
				t.Error(err.Error())
				return false
			}

			return rApplied.Rule.Status == "applied" &&
				RuleMatches(updateReq, *rApplied.Rule) &&
				rUpdated.Rule.Status == "" && // TODO: this is wrong; should be "staged"
				RuleMatches(updateReq, *rUpdated.Rule)
		},
		createProjectAndRuleGen,
	))

	properties.Property("updates an applied rule and applies update", prop.ForAll(
		func(reqs projectAndRuleReq) bool {
			defer testDB.Flush(t)

			_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
			if err != nil {
				t.Error(err.Error())
				return false
			}

			_, err = cl.CreateRule(ctx, &reqs.rules[0])
			if err != nil {
				t.Error(err.Error())
				return false
			}

			cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

			rApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
			if err != nil {
				t.Error(err.Error())
				return false
			}
			statusBeforeSecondUpdate := rApplied.Rule.Status

			updateReq := api.UpdateRuleReq{
				Id:         reqs.rules[0].Id,
				ProjectId:  reqs.rules[0].ProjectId,
				Name:       reqs.rules[0].Name + " updated",
				Type:       reqs.rules[0].Type,
				Conditions: reqs.rules[0].Conditions,
			}
			rUpdated, err := cl.UpdateRule(ctx, &updateReq)

			cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

			rApplied, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
			if err != nil {
				t.Error(err.Error())
				return false
			}

			return statusBeforeSecondUpdate == "applied" &&
				rUpdated.Rule.Status == "" && // TODO: this is wrong; should be "staged"
				rApplied.Rule.Status == "applied" &&
				RuleMatches(updateReq, *rApplied.Rule) &&
				RuleMatches(updateReq, *rUpdated.Rule)
		},
		createProjectAndRuleGen,
	))

	properties.Property("deletes a staged rule and applies deletion", prop.ForAll(
		func(reqs projectAndRuleReq) bool {
			defer testDB.Flush(t)

			_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
			if err != nil {
				t.Error(err.Error())
				return false
			}

			_, err = cl.CreateRule(ctx, &reqs.rules[0])
			if err != nil {
				t.Error(err.Error())
				return false
			}

			_, err = cl.DeleteRule(ctx, &api.DeleteRuleReq{Id: reqs.rules[0].Id})
			if err != nil {
				t.Error(err.Error())
				return false
			}

			_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
			found := err == nil

			return !found
		},
		createProjectAndRuleGen,
	))

	properties.Property("deletes an applied rule and applies deletion", prop.ForAll(
		func(reqs projectAndRuleReq) bool {
			defer testDB.Flush(t)

			_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
			if err != nil {
				t.Error(err.Error())
				return false
			}

			_, err = cl.CreateRule(ctx, &reqs.rules[0])
			if err != nil {
				t.Error(err.Error())
				return false
			}

			cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

			_, err = cl.DeleteRule(ctx, &api.DeleteRuleReq{Id: reqs.rules[0].Id})
			if err != nil {
				t.Error(err.Error())
				return false
			}

			rDeleted, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
			if err != nil {
				t.Error(err.Error())
				return false
			}

			cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

			_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
			found := err == nil

			return !found &&
				RuleMatches(reqs.rules[0], *rDeleted.Rule) &&
				rDeleted.Rule.Status == "applied" && // TODO: wrong!
				!rDeleted.Rule.Deleted // TODO: wrong!
		},
		createProjectAndRuleGen,
	))

	properties.Property("ensures rule IDs are unique", prop.ForAll(
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
		gopter.CombineGens(
			createProjectReqGen, createRuleReqGen, createRuleReqGen,
		).Map(func(vals []interface{}) projectAndRuleReq {
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

func getGopterParams(seed int64) *gopter.Properties {
	params := gopter.DefaultTestParametersWithSeed(seed)
	params.MinSize = 1 // otherwise, we'd get zero-length "conditions" slices
	params.MinSuccessfulTests = 5
	return gopter.NewProperties(params)
}

func getGenerators() (gopter.Gen, gopter.Gen, gopter.Gen) {
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

	createProjectAndRuleGen := gopter.CombineGens(
		createProjectReqGen, createRuleReqGen,
	).Map(func(vals []interface{}) projectAndRuleReq {
		p := vals[0].(api.CreateProjectReq)
		r := vals[1].(api.CreateRuleReq)
		r.ProjectId = p.Id
		return projectAndRuleReq{
			CreateProjectReq: p,
			rules:            []api.CreateRuleReq{r},
		}
	})

	return createRuleReqGen, createProjectReqGen, createProjectAndRuleGen
}

func RuleMatches(req interface{}, actual api.ProjectRule) bool {
	if _, ok := req.(api.CreateRuleReq); ok {
		ruleReq, _ := req.(api.CreateRuleReq)
		return len(actual.Conditions) == len(ruleReq.Conditions) &&
			actual.Id == ruleReq.Id &&
			actual.Name == ruleReq.Name &&
			actual.ProjectId == ruleReq.ProjectId
	} else {
		ruleReq, _ := req.(api.UpdateRuleReq)
		return len(actual.Conditions) == len(ruleReq.Conditions) &&
			actual.Id == ruleReq.Id &&
			actual.Name == ruleReq.Name &&
			actual.ProjectId == ruleReq.ProjectId
	}
}
