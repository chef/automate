package v2_test

import (
	"context"
	"fmt"
	"math"
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
	idRegex        = "^[a-z0-9-]{1,64}$"
	conditionLimit = 10 // help avoid the grpc limit of 4194304
)

type projectAndRuleReq struct {
	api.CreateProjectReq
	rules []api.CreateRuleReq
}

var createRuleReqGen, createProjectReqGen, createProjectAndRuleGen = getGenerators()

func TestCreateRuleProperties(t *testing.T) {
	ctx := context.Background()
	cl, testDB, _, _, seed := testhelpers.SetupProjectsAndRulesWithDB(t)
	properties := getGopterParams(seed)

	properties.Property("newly created rules are staged",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				reportRules(t, reqs.rules)

				respRule, err := createProjectAndRule(ctx, cl, reqs)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return respRule.Status == "staged" &&
					ruleMatches(reqs.rules[0], *respRule)
			},
			createProjectAndRuleGen,
		))

	properties.Property("creating rules with non-unique IDs is prohibited",
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
			gopter.CombineGens(
				createProjectReqGen, createRuleReqGen, createRuleReqGen,
			).Map(func(vals []interface{}) projectAndRuleReq {
				projectAndRule := genStandardProjectAndRules(
					vals[0].(api.CreateProjectReq),
					[]api.CreateRuleReq{vals[1].(api.CreateRuleReq), vals[2].(api.CreateRuleReq)},
				)

				// "fix" the second req to use the same ID as the first one
				projectAndRule.rules[1].Id = projectAndRule.rules[0].Id

				return projectAndRule
			}),
		))

	properties.TestingRun(t)
}

func TestGetRuleProperties(t *testing.T) {
	ctx := context.Background()
	cl, testDB, _, _, seed := testhelpers.SetupProjectsAndRulesWithDB(t)
	properties := getGopterParams(seed)

	properties.Property("newly created rules are reported as staged",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				reportRules(t, reqs.rules)

				_, err := createProjectAndRule(ctx, cl, reqs)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				rStaged, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return rStaged.Rule.Status == "staged" &&
					ruleMatches(reqs.rules[0], *rStaged.Rule)
			},
			createProjectAndRuleGen,
		))

	properties.Property("applied rules are reported as applied",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := createProjectAndRule(ctx, cl, reqs)

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				rApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return rApplied.Rule.Status == "applied" &&
					ruleMatches(reqs.rules[0], *rApplied.Rule)
			},
			createProjectAndRuleGen,
		))

	properties.TestingRun(t)
}

func TestListRuleProperties(t *testing.T) {
	ctx := context.Background()
	cl, testDB, _, _, seed := testhelpers.SetupProjectsAndRulesWithDB(t)
	properties := getGopterParams(seed)

	properties.Property("staged rules are reported as staged",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				reportRules(t, reqs.rules)

				_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				reqs.rules, err = createRules(ctx, cl, reqs.rules)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				resp, err := cl.ListRules(ctx, &api.ListRulesReq{IncludeStaged: true})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return rulesMatch(reqs.rules, resp)

			},
			createProjectAndRuleGen,
		))

	properties.Property("staged rules are not reported as applied",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				reportRules(t, reqs.rules)

				_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				reqs.rules, err = createRules(ctx, cl, reqs.rules)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				resp, err := cl.ListRules(ctx, &api.ListRulesReq{IncludeStaged: false})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return len(resp.Rules) == 0

			},
			createProjectAndRuleGen,
		))

	properties.Property("applied rules are reported as applied",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				reportRules(t, reqs.rules)

				_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				reqs.rules, err = createRules(ctx, cl, reqs.rules)
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				resp, err := cl.ListRules(ctx, &api.ListRulesReq{})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return rulesMatch(reqs.rules, resp)

			},
			createProjectAndRuleGen,
		))

	properties.TestingRun(t)
}

func TestUpdateRuleProperties(t *testing.T) {
	ctx := context.Background()
	cl, testDB, _, _, seed := testhelpers.SetupProjectsAndRulesWithDB(t)
	properties := getGopterParams(seed)

	properties.Property("updating newly created rules remain staged",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := createProjectAndRule(ctx, cl, reqs)

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

				rApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return rApplied.Rule.Status == "applied" &&
					ruleMatches(updateReq, *rApplied.Rule) &&
					rStaged.Rule.Status == "staged" &&
					ruleMatches(updateReq, *rStaged.Rule)
			},
			createProjectAndRuleGen,
		))

	properties.Property("updating applied rules become staged",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := createProjectAndRule(ctx, cl, reqs)

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				rInitialApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
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

				rFinalApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return rInitialApplied.Rule.Status == "applied" &&
					rStaged.Rule.Status == "staged" &&
					rFinalApplied.Rule.Status == "applied" &&
					ruleMatches(updateReq, *rStaged.Rule) &&
					ruleMatches(updateReq, *rFinalApplied.Rule)
			},
			createProjectAndRuleGen,
		))

	properties.Property("updating applied and staged rules remain staged",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := createProjectAndRule(ctx, cl, reqs)

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

				rApplied, err := cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				return rInitialStaged.Rule.Status == "staged" &&
					rFinalStaged.Rule.Status == "staged" &&
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

	properties.Property("deleting newly created rules deletes them immediately",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := createProjectAndRule(ctx, cl, reqs)

				_, err = cl.DeleteRule(ctx, &api.DeleteRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				deletedWhenStaged := grpctest.AssertCode(t, codes.NotFound, err)

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				deletedWhenApplied := grpctest.AssertCode(t, codes.NotFound, err)

				return deletedWhenStaged && deletedWhenApplied
			},
			createProjectAndRuleGen,
		))

	properties.Property("deleting applied rules marks them for deletion and finalizes deletion upon applying rules",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := createProjectAndRule(ctx, cl, reqs)

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				_, err = cl.DeleteRule(ctx, &api.DeleteRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				markedforDeletion := grpctest.AssertCode(t, codes.NotFound, err) && strings.Contains(err.Error(), "marked for deletion")

				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				fullyDeleted := grpctest.AssertCode(t, codes.NotFound, err) && !strings.Contains(err.Error(), "marked for deletion")

				return markedforDeletion && fullyDeleted
			},
			createProjectAndRuleGen,
		))

	properties.Property("deleting applied rules with staged updates marks them for deletion and finalizes deletion upon applying rules",
		prop.ForAll(
			func(reqs projectAndRuleReq) bool {
				defer testDB.Flush(t)

				_, err := createProjectAndRule(ctx, cl, reqs)

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

				_, err = cl.DeleteRule(ctx, &api.DeleteRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				if err != nil {
					return reportErrorAndYieldFalse(t, err)
				}

				_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				markedforDeletion := grpctest.AssertCode(t, codes.NotFound, err) && strings.Contains(err.Error(), "marked for deletion")
				cl.ApplyRulesStart(ctx, &api.ApplyRulesStartReq{})

				_, err = cl.GetRule(ctx, &api.GetRuleReq{Id: reqs.rules[0].Id, ProjectId: reqs.rules[0].ProjectId})
				fullyDeleted := grpctest.AssertCode(t, codes.NotFound, err) && !strings.Contains(err.Error(), "marked for deletion")

				return markedforDeletion && fullyDeleted
			},
			createProjectAndRuleGen,
		))

	properties.TestingRun(t)
}

func getGopterParams(seed int64) *gopter.Properties {
	params := gopter.DefaultTestParametersWithSeed(seed)
	params.MinSize = 1             // otherwise, we'd get zero-length "conditions" slices
	params.MaxSize = 25            // otherwise, we may exceed GRPC capacity in ListRules
	params.MinSuccessfulTests = 25 // reduced from default 100 to avoid timeouts in CI!
	return gopter.NewProperties(params)
}

func getGenerators() (gopter.Gen, gopter.Gen, gopter.Gen) {
	graphicRange := rangetable.Merge(unicode.GraphicRanges...)

	conditionsGenNode := gen.StructPtr(reflect.TypeOf(&api.Condition{}), map[string]gopter.Gen{
		"Type": gen.OneConstOf(
			api.ProjectRuleConditionAttributes_CHEF_SERVER,
			api.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
			api.ProjectRuleConditionAttributes_ENVIRONMENT,
			api.ProjectRuleConditionAttributes_CHEF_ROLE,
			api.ProjectRuleConditionAttributes_CHEF_TAG,
			api.ProjectRuleConditionAttributes_CHEF_POLICY_GROUP,
			api.ProjectRuleConditionAttributes_CHEF_POLICY_NAME,
		),
		"Values": gen.SliceOf(gen.UnicodeString(graphicRange)),
	})
	conditionsGenEvent := gen.StructPtr(reflect.TypeOf(&api.Condition{}), map[string]gopter.Gen{
		"Type": gen.OneConstOf(
			api.ProjectRuleConditionAttributes_CHEF_SERVER,
			api.ProjectRuleConditionAttributes_CHEF_ORGANIZATION,
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
		createProjectReqGen, gen.SliceOf(createRuleReqGen),
	).Map(func(vals []interface{}) projectAndRuleReq {
		p := vals[0].(api.CreateProjectReq)
		rules := vals[1].([]api.CreateRuleReq)
		return genStandardProjectAndRules(p, rules)
	})

	return createRuleReqGen, createProjectReqGen, createProjectAndRuleGen
}

func genStandardProjectAndRules(p api.CreateProjectReq, rules []api.CreateRuleReq) projectAndRuleReq {
	for i, rule := range rules {

		// make the id unique!
		rules[i].Id = fmt.Sprintf("%s-index-%d", rule.Id, i)

		// tie the rule to a real project
		rules[i].ProjectId = p.Id

		// constrain condition count even further than the gopter global param
		var conditions []*api.Condition
		for j := 0; j < int(math.Min(conditionLimit, float64(len(rule.Conditions)))); j++ {
			conditions = append(conditions, rule.Conditions[j])
		}
		rules[i].Conditions = conditions
	}
	return projectAndRuleReq{
		CreateProjectReq: p,
		rules:            rules,
	}
}

func reportErrorAndYieldFalse(t *testing.T, err error) bool {
	t.Helper()
	t.Error(err.Error())
	return false
}

func ruleMatches(req interface{}, actual api.ProjectRule) bool {
	if ruleReq, ok := req.(api.CreateRuleReq); ok {
		return len(actual.Conditions) == len(ruleReq.Conditions) &&
			actual.Id == ruleReq.Id &&
			actual.Name == ruleReq.Name &&
			actual.ProjectId == ruleReq.ProjectId
	} else if ruleReq, ok := req.(api.UpdateRuleReq); ok {
		return len(actual.Conditions) == len(ruleReq.Conditions) &&
			actual.Id == ruleReq.Id &&
			actual.Name == ruleReq.Name &&
			actual.ProjectId == ruleReq.ProjectId
	} else {
		return false // should never happen
	}
}

func rulesMatch(rules []api.CreateRuleReq, resp *api.ListRulesResp) bool {
	if len(rules) != len(resp.Rules) {
		return false
	}
	// Cannot assume DB returns rules in a particular order, so make a lookup table
	lookup := make(map[string]api.CreateRuleReq, len(rules))
	for _, rule := range rules {
		lookup[rule.Id] = rule
	}
	// Now check each returned rule for existence and matching fields
	for _, rule := range resp.Rules {
		if expectedRule, ok := lookup[rule.Id]; ok {
			if !ruleMatches(expectedRule, *rule) {
				return false
			}
		} else {
			return false
		}
	}
	return true
}

func createProjectAndRule(ctx context.Context, cl api.ProjectsClient, reqs projectAndRuleReq) (*api.ProjectRule, error) {
	_, err := cl.CreateProject(ctx, &reqs.CreateProjectReq)
	if err != nil {
		return nil, err
	}

	rule, err := cl.CreateRule(ctx, &reqs.rules[0])
	if err != nil {
		return nil, err
	}
	return rule.Rule, nil
}

func createRules(ctx context.Context, cl api.ProjectsClient, rules []api.CreateRuleReq) ([]api.CreateRuleReq, error) {
	for _, rule := range rules {

		_, err := cl.CreateRule(ctx, &rule)
		// Note: this could be very noisy, but it's hard to figure out what went
		// wrong without the error
		if err != nil {
			return nil, err
		}
	}
	return rules, nil
}

func reportRules(t *testing.T, rules []api.CreateRuleReq) {
	t.Helper()
	condCount := 0
	valueCount := 0
	ruleBytes := 0
	valueBytes := 0
	for _, rule := range rules {
		condCount += len(rule.Conditions)
		ruleBytes += rule.XXX_Size()
		for _, c := range rule.Conditions {
			valueCount += len(c.Values)
			for _, v := range c.Values {
				valueBytes += size(v)
			}
		}
	}
	t.Logf("===> %d rules, bytes=%d, avg/rule=%d, %d conditions, avg/rule=%d, %d values, avg/rule=%d, avg/cond=%d, bytes=%d, avg/value=%d",
		len(rules), ruleBytes, (ruleBytes / len(rules)),
		condCount, condCount/len(rules), valueCount, valueCount/len(rules), valueCount/condCount, valueBytes, valueBytes/valueCount)
}

func size(s string) int {
	return len([]byte(s))
}
