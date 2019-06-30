package v2_test

import (
	"context"
	"reflect"
	"strings"
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

func TestGetRuleProperties(t *testing.T) {
	ctx := context.Background()
	cl, testDB, _, _, seed := testhelpers.SetupProjectsAndRulesWithDB(t)
	properties := getGopterParams(seed)
	createRuleReqGen, createProjectReqGen, createProjectAndRuleGen := getGenerators()

	properties.Property("creates a staged rule and confirms it is staged",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.CreateRule(ctx, &reqs.rules[0])
				// Note: this could be very noisy, but it's hard to figure out what went
				// wrong without the error
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				rStaged, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				// Note: we're ignoring type conversion, as asserting that would require us
				// to replicate the mapping logic in tests.
				// Instead, as we add GetRule, ListRules, etc., we can use this
				// testing approach to write meaningful assertions.
				return rStaged.Rule.Status == "staged" &&
					ruleMatches(reqs.rules[0], *rStaged.Rule)
			},
			createProjectAndRuleGen,
		))

	properties.Property("creates a staged rule and applies it then confirms it is applied",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.CreateRule(ctx, &reqs.rules[0])
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				rApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return rApplied.Rule.Status == "applied" &&
					ruleMatches(reqs.rules[0], *rApplied.Rule)
			},
			createProjectAndRuleGen,
		))

	properties.Property("ensure creating a rule fails if the rule ID is not unique",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				if _, err := cl.CreateRule(ctx, &reqs.rules[0]); err != nil {
					return reportErrorAndYieldFalse(t, err)
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
}

func TestUpdateRuleProperties(t *testing.T) {
	ctx := context.Background()
	cl, testDB, _, _, seed := testhelpers.SetupProjectsAndRulesWithDB(t)
	properties := getGopterParams(seed)
	_, _, createProjectAndRuleGen := getGenerators()

	properties.Property("updates a staged rule and applies update confirming transition from staged to applied",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.CreateRule(ctx, &reqs.rules[0])
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				updateReq := api.UpdateRuleReq{
					Id:         reqs.rules[0].Id,
					ProjectId:  reqs.rules[0].ProjectId,
					Name:       reqs.rules[0].Name + " updated",
					Type:       reqs.rules[0].Type,
					Conditions: reqs.rules[0].Conditions,
				}
				rStaged, err := cl.UpdateRule(ctx, &updateReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				rApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return rApplied.Rule.Status == "applied" &&
					ruleMatches(updateReq, *rApplied.Rule) &&
					rStaged.Rule.Status == "" && // TODO: this is wrong; should be "staged"
					ruleMatches(updateReq, *rStaged.Rule)
			},
			createProjectAndRuleGen,
		))

	properties.Property("updates an applied rule and applies update confirming transition from applied to staged to applied",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.CreateRule(ctx, &reqs.rules[0])
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				rInitialApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				updateReq := api.UpdateRuleReq{
					Id:         reqs.rules[0].Id,
					ProjectId:  reqs.rules[0].ProjectId,
					Name:       reqs.rules[0].Name + " updated",
					Type:       reqs.rules[0].Type,
					Conditions: reqs.rules[0].Conditions,
				}
				rStaged, err := cl.UpdateRule(ctx, &updateReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				rFinalApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return rInitialApplied.Rule.Status == "applied" &&
					rStaged.Rule.Status == "" && // TODO: this is wrong; should be "staged"
					rFinalApplied.Rule.Status == "applied" &&
					ruleMatches(updateReq, *rStaged.Rule) &&
					ruleMatches(updateReq, *rFinalApplied.Rule)
			},
			createProjectAndRuleGen,
		))

	properties.Property("updates an applied and staged rule and applies update confirming transition from staged to applied",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.CreateRule(ctx, &reqs.rules[0])
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				updateReq := api.UpdateRuleReq{
					Id:         reqs.rules[0].Id,
					ProjectId:  reqs.rules[0].ProjectId,
					Name:       reqs.rules[0].Name + " updated",
					Type:       reqs.rules[0].Type,
					Conditions: reqs.rules[0].Conditions,
				}
				rInitialStaged, err := cl.UpdateRule(ctx, &updateReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				updateReq2 := api.UpdateRuleReq{
					Id:         reqs.rules[0].Id,
					ProjectId:  reqs.rules[0].ProjectId,
					Name:       reqs.rules[0].Name + " updated again",
					Type:       reqs.rules[0].Type,
					Conditions: reqs.rules[0].Conditions,
				}
				rFinalStaged, err := cl.UpdateRule(ctx, &updateReq2)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				rApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return rInitialStaged.Rule.Status == "" && // TODO: this is wrong; should be "staged"
					rFinalStaged.Rule.Status == "" && // TODO: this is wrong; should be "staged"
					rApplied.Rule.Status == "applied" &&
					ruleMatches(updateReq, *rInitialStaged.Rule) &&
					ruleMatches(updateReq2, *rFinalStaged.Rule) &&
					ruleMatches(updateReq2, *rApplied.Rule)
			},
			createProjectAndRuleGen,
		))
	properties.TestingRun(t)
}

func TestDeleteRuleProperties(t *testing.T) {
	ctx := context.Background()
	cl, testDB, _, _, seed := testhelpers.SetupProjectsAndRulesWithDB(t)
	properties := getGopterParams(seed)
	_, _, createProjectAndRuleGen := getGenerators()

	properties.Property("deletes a staged rule and applies deletion confirming rule not found in both states",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.CreateRule(ctx, &reqs.rules[0])
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.DeleteRule(ctx, &api.DeleteRuleReq{Id: reqs.rules[0].Id})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
				deletedWhenStaged := err != nil && strings.Contains(err.Error(), "could not find")

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
				deletedWhenApplied := err != nil && strings.Contains(err.Error(), "could not find")

				return deletedWhenStaged && deletedWhenApplied
			},
			createProjectAndRuleGen,
		))

	properties.Property("deletes an applied rule and applies deletion confirming rule still found while deletion is staged but no longer found once deletion is applied",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.CreateRule(ctx, &reqs.rules[0])
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				_, err = cl.DeleteRule(ctx, &api.DeleteRuleReq{Id: reqs.rules[0].Id})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				// Can still find a staged-deleted rule if the rule was previously applied!
				rDeleted, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
				deletedWhenApplied := err != nil && strings.Contains(err.Error(), "could not find")

				return deletedWhenApplied &&
					ruleMatches(reqs.rules[0], *rDeleted.Rule) &&
					rDeleted.Rule.Status == "applied" && // TODO: wrong!
					!rDeleted.Rule.Deleted // TODO: wrong!
			},
			createProjectAndRuleGen,
		))

	properties.Property("deletes an applied and staged rule and applies deletion confirming rule found but marked for deletion while deletion staged but rule no longer found once deletion is applied",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.CreateRule(ctx, &reqs.rules[0])
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				updateReq := api.UpdateRuleReq{
					Id:         reqs.rules[0].Id,
					ProjectId:  reqs.rules[0].ProjectId,
					Name:       reqs.rules[0].Name + " updated",
					Type:       reqs.rules[0].Type,
					Conditions: reqs.rules[0].Conditions,
				}
				_, err = cl.UpdateRule(ctx, &updateReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.DeleteRule(ctx, &api.DeleteRuleReq{Id: reqs.rules[0].Id})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
				markedforDeletion := err != nil && strings.Contains(err.Error(), "marked for deletion")

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id})
				fullyDeleted := err != nil && strings.Contains(err.Error(), "could not find")

				return markedforDeletion && fullyDeleted
			},
			createProjectAndRuleGen,
		))

	properties.TestingRun(t)
}

func getGopterParams(seed int64) *gopter.Properties {
	params := gopter.DefaultTestParametersWithSeed(seed)
	params.MinSize = 1             // otherwise, we'd get zero-length "conditions" slices
	params.MinSuccessfulTests = 25 // reduced from default 100 to be more speedy
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

func reportErrorAndYieldFalse(t *testing.T, err error) bool {
	t.Helper()
	t.Error(err.Error())
	return false
}

func ruleMatches(req interface{}, actual api.ProjectRule) bool {
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
