package opa

// nolint:lll
//go:generate go-bindata -pkg $GOPACKAGE -o policy.bindata.go -ignore .*_test.rego -ignore Makefile -ignore README\.md policy/...

import (
	"context"
	"fmt"
	"os"
	"strings"

	"github.com/open-policy-agent/opa/ast"
	"github.com/open-policy-agent/opa/rego"
	"github.com/open-policy-agent/opa/storage"
	"github.com/open-policy-agent/opa/storage/inmem"
	"github.com/open-policy-agent/opa/topdown"
	"github.com/pkg/errors"

	"github.com/chef/automate/components/authz-service/engine"
	"github.com/chef/automate/lib/logger"
)

// State wraps the state of OPA we need to track
type State struct {
	log               logger.Logger
	store             storage.Store
	v2Store           storage.Store
	v2p1Store         storage.Store
	queries           map[string]ast.Body
	compiler          *ast.Compiler
	modules           map[string]*ast.Module
	partialAuth       rego.PartialResult
	v2PartialAuth     rego.PartialResult
	v2PartialProjects rego.PartialResult
}

// this needs to match the hardcoded OPA policy document we've put in place
const (
	authzQuery              = "data.authz.authorized"
	filteredPairsQuery      = "data.authz.introspection.authorized_pair[_]"
	authzV2Query            = "data.authz_v2.authorized"
	authzProjectsV2Query    = "data.authz_v2.authorized_project"
	filteredPairsV2Query    = "data.authz_v2.introspection.authorized_pair[_]"
	rulesForProjectQuery    = "data.rule_mappings.rules_for_project"
	listProjectMapQuery     = "data.rule_mappings.rules_for_all_projects"
	filteredProjectsV2Query = "data.authz_v2.introspection.authorized_project"
)

// OptFunc is the type of functional options to be passed to New()
type OptFunc func(*State)

// New initializes a fresh OPA state, using the default, hardcoded OPA policy
// from policy/authz*.rego unless overridden via an opa.OptFunc.
func New(ctx context.Context, l logger.Logger, opts ...OptFunc) (*State, error) {
	authzQueryParsed, err := ast.ParseBody(authzQuery)
	if err != nil {
		return nil, errors.Wrapf(err, "parse query %q", authzQuery)
	}
	filteredPairsQueryParsed, err := ast.ParseBody(filteredPairsQuery)
	if err != nil {
		return nil, errors.Wrapf(err, "parse query %q", filteredPairsQuery)
	}
	authzV2QueryParsed, err := ast.ParseBody(authzV2Query)
	if err != nil {
		return nil, errors.Wrapf(err, "parse query %q", authzV2Query)
	}
	authzProjectsV2QueryParsed, err := ast.ParseBody(authzProjectsV2Query)
	if err != nil {
		return nil, errors.Wrapf(err, "parse query %q", authzProjectsV2Query)
	}
	filteredPairsV2QueryParsed, err := ast.ParseBody(filteredPairsV2Query)
	if err != nil {
		return nil, errors.Wrapf(err, "parse query %q", filteredPairsV2Query)
	}
	filteredProjectsV2QueryParsed, err := ast.ParseBody(filteredProjectsV2Query)
	if err != nil {
		return nil, errors.Wrapf(err, "parse query %q", filteredProjectsV2Query)
	}
	rulesForProjectQueryParsed, err := ast.ParseBody(rulesForProjectQuery)
	if err != nil {
		return nil, errors.Wrapf(err, "parse query %q", rulesForProjectQuery)
	}
	listProjectMapQueryParsed, err := ast.ParseBody(listProjectMapQuery)
	if err != nil {
		return nil, errors.Wrapf(err, "parse query %q", listProjectMapQuery)
	}
	s := State{
		log:       l,
		store:     inmem.New(),
		v2Store:   inmem.New(),
		v2p1Store: inmem.New(),
		queries: map[string]ast.Body{
			authzQuery:              authzQueryParsed,
			filteredPairsQuery:      filteredPairsQueryParsed,
			authzV2Query:            authzV2QueryParsed,
			authzProjectsV2Query:    authzProjectsV2QueryParsed,
			filteredPairsV2Query:    filteredPairsV2QueryParsed,
			filteredProjectsV2Query: filteredProjectsV2QueryParsed,
			rulesForProjectQuery:    rulesForProjectQueryParsed,
			listProjectMapQuery:     listProjectMapQueryParsed,
		},
	}
	for _, opt := range opts {
		opt(&s)
	}

	if err := s.initModules(); err != nil {
		return nil, errors.Wrap(err, "init OPA modules")
	}
	return &s, nil
}

// WithModules allows for injecting an OPA policy via opa.New() for engine
// initialization.
func WithModules(mods map[string]*ast.Module) OptFunc {
	return func(s *State) {
		s.modules = mods
	}
}

// initModules parses the rego files that have been compiled-in and stores the
// result, to be used in initPartialResult.
func (s *State) initModules() error {
	if len(s.modules) == 0 {
		mods := map[string]*ast.Module{}
		for _, name := range AssetNames() {
			if !strings.HasSuffix(name, ".rego") {
				continue // skip this, whatever has been compiled-in here
			}
			parsed, err := ast.ParseModule(name, string(MustAsset(name)))
			if err != nil {
				return errors.Wrapf(err, "parse policy file %q", name)
			}
			mods[name] = parsed
		}
		s.modules = mods
	}
	compiler, err := s.newCompiler()
	if err != nil {
		return errors.Wrap(err, "init compiler")
	}
	s.compiler = compiler
	return nil
}

// initPartialResult allows caching things that don't change among multiple
// query evaluations. We don't bother for the pairs query, but for
// IsAuthorized(), we want to do as little work per call as possible.
func (s *State) initPartialResult(ctx context.Context) error {
	// Reset compiler to avoid state issues
	// Note: PartialResult will _compile_ the passed module; so when the engine is
	// initialized, everything will also be ready to serve the other, non-partial
	// queries
	compiler, err := s.newCompiler()
	if err != nil {
		return err
	}

	r := rego.New(
		rego.ParsedQuery(s.queries[authzQuery]),
		rego.Compiler(compiler),
		rego.Store(s.store),
	)
	pr, err := r.PartialResult(ctx)
	if err != nil {
		return errors.Wrap(err, "partial eval")
	}
	s.partialAuth = pr
	s.v2PartialAuth = rego.PartialResult{}
	s.v2PartialProjects = rego.PartialResult{}
	return nil
}

func (s *State) initPartialResultV2(ctx context.Context) error {
	// Reset compiler to avoid state issues
	compiler, err := s.newCompiler()
	if err != nil {
		return err
	}

	// Partial eval for authzV2Query.
	r := rego.New(
		rego.ParsedQuery(s.queries[authzV2Query]),
		rego.Compiler(compiler),
		rego.Store(s.v2Store),
	)
	v2Partial, err := r.PartialResult(ctx)
	if err != nil {
		return errors.Wrap(err, "partial eval (authorized)")
	}
	s.v2PartialAuth = v2Partial
	s.partialAuth = rego.PartialResult{}
	s.v2PartialProjects = rego.PartialResult{}
	return nil
}

func (s *State) initPartialResultV2p1(ctx context.Context) error {
	// Partial eval for authzProjectsV2Query.
	// Each partial eval needs a separate compiler.
	compiler, err := s.newCompiler()
	if err != nil {
		return err
	}
	r := rego.New(
		rego.ParsedQuery(s.queries[authzProjectsV2Query]),
		rego.Compiler(compiler),
		rego.Store(s.v2p1Store),
	)
	v2PartialProjects, err := r.PartialResult(ctx)
	if err != nil {
		return errors.Wrap(err, "partial eval (authorized_project)")
	}
	s.v2PartialProjects = v2PartialProjects
	s.partialAuth = rego.PartialResult{}
	s.v2PartialAuth = rego.PartialResult{}
	return nil
}

func (s *State) newCompiler() (*ast.Compiler, error) {
	compiler := ast.NewCompiler()
	compiler.Compile(s.modules)
	if compiler.Failed() {
		return nil, errors.Wrap(compiler.Errors, "compile modules")
	}

	return compiler, nil
}

// DumpData is a bit fast-and-loose when it comes to error checking; it's not meant
// to be used in production
func (s *State) DumpData(ctx context.Context) error {
	return dumpData(ctx, s.store, s.log)
}

func dumpData(ctx context.Context, store storage.Store, l logger.Logger) error {
	txn, err := store.NewTransaction(ctx)
	if err != nil {
		return err
	}
	data, err := store.Read(ctx, txn, storage.Path([]string{}))
	if err != nil {
		return err
	}
	l.Infof("data: %#v", data)
	return store.Commit(ctx, txn)
}

func (s *State) DumpDataV2(ctx context.Context) error {
	return dumpData(ctx, s.v2Store, s.log)
}

func (s *State) DumpDataV2p1(ctx context.Context) error {
	return dumpData(ctx, s.v2p1Store, s.log)
}

// IsAuthorized evaluates whether a given [subject, resource, action] tuple
// is authorized given the service's state
func (s *State) IsAuthorized(
	ctx context.Context,
	subjects engine.Subjects,
	action engine.Action,
	resource engine.Resource) (bool, error) {

	subs := make(ast.Array, len(subjects))
	for i, sub := range subjects {
		subs[i] = ast.NewTerm(ast.String(sub))
	}
	input := ast.NewObject(
		[2]*ast.Term{ast.NewTerm(ast.String("subjects")), ast.NewTerm(subs)},
		[2]*ast.Term{ast.NewTerm(ast.String("resource")), ast.NewTerm(ast.String(resource))},
		[2]*ast.Term{ast.NewTerm(ast.String("action")), ast.NewTerm(ast.String(action))},
	)
	resultSet, err := s.partialAuth.Rego(rego.ParsedInput(input)).Eval(ctx)
	if err != nil {
		return false, &ErrEvaluation{e: err}
	}

	switch len(resultSet) {
	case 0:
		return false, nil
	case 1:
		exps := resultSet[0].Expressions
		if len(exps) != 1 {
			return false, &ErrUnexpectedResultExpression{exps: exps}

		}
		return exps[0].Value == true, nil
	default:
		return false, &ErrUnexpectedResultSet{set: resultSet}
	}
}

// V2IsAuthorized evaluates whether a given [subject, resource, action] tuple
// is authorized given the service's state
func (s *State) V2IsAuthorized(
	ctx context.Context,
	subjects engine.Subjects,
	action engine.Action,
	resource engine.Resource) (bool, error) {

	subs := make(ast.Array, len(subjects))
	for i, sub := range subjects {
		subs[i] = ast.NewTerm(ast.String(sub))
	}
	input := ast.NewObject(
		[2]*ast.Term{ast.NewTerm(ast.String("subjects")), ast.NewTerm(subs)},
		[2]*ast.Term{ast.NewTerm(ast.String("resource")), ast.NewTerm(ast.String(resource))},
		[2]*ast.Term{ast.NewTerm(ast.String("action")), ast.NewTerm(ast.String(action))},
	)
	resultSet, err := s.v2PartialAuth.Rego(rego.ParsedInput(input)).Eval(ctx)
	if err != nil {
		return false, &ErrEvaluation{e: err}
	}

	switch len(resultSet) {
	case 0:
		return false, nil
	case 1:
		exps := resultSet[0].Expressions
		if len(exps) != 1 {
			return false, &ErrUnexpectedResultExpression{exps: exps}

		}
		return exps[0].Value == true, nil
	default:
		return false, &ErrUnexpectedResultSet{set: resultSet}
	}
}

// V2ProjectsAuthorized evaluates whether a given [subject, resource, action,
// projects] tuple is authorized and returns the list of associated allowed
// projects from the set of requested projects passed in.
func (s *State) V2ProjectsAuthorized(
	ctx context.Context,
	subjects engine.Subjects,
	action engine.Action,
	resource engine.Resource,
	projects engine.Projects) ([]string, error) {
	subs := make(ast.Array, len(subjects))
	for i, sub := range subjects {
		subs[i] = ast.NewTerm(ast.String(sub))
	}
	projs := make(ast.Array, len(projects))
	for i, proj := range projects {
		projs[i] = ast.NewTerm(ast.String(proj))
	}
	input := ast.NewObject(
		[2]*ast.Term{ast.NewTerm(ast.String("subjects")), ast.NewTerm(subs)},
		[2]*ast.Term{ast.NewTerm(ast.String("resource")), ast.NewTerm(ast.String(resource))},
		[2]*ast.Term{ast.NewTerm(ast.String("action")), ast.NewTerm(ast.String(action))},
		[2]*ast.Term{ast.NewTerm(ast.String("projects")), ast.NewTerm(projs)},
	)
	resultSet, err := s.v2PartialProjects.Rego(rego.ParsedInput(input)).Eval(ctx)
	if err != nil {
		return []string{}, &ErrEvaluation{e: err}
	}

	return s.projectsFromResults(resultSet)
}

// FilterAuthorizedPairs passes the pairs into OPA, lets it take care of the
// filtering, and returns the result (sub)list
func (s *State) FilterAuthorizedPairs(
	ctx context.Context,
	subjects engine.Subjects,
	pairs []engine.Pair) ([]engine.Pair, error) {

	opaInput := map[string]interface{}{
		"subjects": subjects,
		"pairs":    pairs,
	}

	rs, err := s.evalQuery(ctx, s.queries[filteredPairsQuery], opaInput, s.store)
	if err != nil {
		return nil, &ErrEvaluation{e: err}
	}

	return s.pairsFromResults(rs)
}

// V2FilterAuthorizedPairs passes the pairs into OPA, lets it take care of the
// filtering, and returns the result (sub)list
func (s *State) V2FilterAuthorizedPairs(
	ctx context.Context,
	subjects engine.Subjects,
	pairs []engine.Pair) ([]engine.Pair, error) {

	opaInput := map[string]interface{}{
		"subjects": subjects,
		"pairs":    pairs,
	}

	rs, err := s.evalQuery(ctx, s.queries[filteredPairsV2Query], opaInput, s.v2Store)
	if err != nil {
		return nil, &ErrEvaluation{e: err}
	}

	return s.pairsFromResults(rs)
}

// V2FilterAuthorizedProjects passes the pairs of all action/resources into OPA,
// lets it take care of the filtering,
// and returns the projects associated with the resulting (sub)list.
func (s *State) V2FilterAuthorizedProjects(
	ctx context.Context, subjects engine.Subjects) ([]string, error) {

	opaInput := map[string]interface{}{
		"subjects": subjects,
	}

	rs, err := s.evalQuery(ctx, s.queries[filteredProjectsV2Query], opaInput, s.v2p1Store)
	if err != nil {
		return nil, &ErrEvaluation{e: err}
	}

	return s.projectsFromResults(rs)
}

// Note(sr) Right now, it doesn't seem like this was doing much more than
// retrieving data from OPA's store. However, that's fine -- we'll need those
// mapping rules in OPA's store for other things (most likely), so retrieving
// them from there is a decent approximation of our approach.
func (s *State) RulesForProject(
	ctx context.Context,
	projectID string) ([]engine.Rule, error) {
	opaInput := map[string]interface{}{
		"project_id": projectID,
	}

	rs, err := s.evalQuery(ctx, s.queries[rulesForProjectQuery], opaInput, s.v2p1Store)
	if err != nil {
		return nil, &ErrEvaluation{e: err}
	}
	return s.rulesFromResults(rs)
}

// ListProjectMappings returns a map of all the rules for each projectID.
func (s *State) ListProjectMappings(ctx context.Context) (map[string][]engine.Rule, error) {
	rs, err := s.evalQuery(ctx, s.queries[listProjectMapQuery], map[string]interface{}{}, s.v2Store)
	if err != nil {
		return nil, &ErrEvaluation{e: err}
	}
	return s.projectMappingsFromResults(rs)
}

func (s *State) evalQuery(
	ctx context.Context,
	query ast.Body,
	input interface{},
	store storage.Store) (rego.ResultSet, error) {

	var tracer *topdown.BufferTracer
	// ⓘ DEBUG note: if you want to see what's happening during policy execution
	// in OPA, uncomment the following line
	// tracer = topdown.NewBufferTracer()

	rs, err := rego.New(
		rego.ParsedQuery(query),
		rego.Input(input),
		rego.Compiler(s.compiler),
		rego.Store(store),   // knows OUR policies
		rego.Tracer(tracer), // trace execution for debugging
	).Eval(ctx)
	if err != nil {
		return nil, err
	}

	if tracer.Enabled() {
		topdown.PrettyTrace(os.Stderr, *tracer) //nolint: govet // tracer can be nil only if tracer.Enabled() == false
	}

	return rs, nil
}

func (s *State) pairsFromResults(rs rego.ResultSet) ([]engine.Pair, error) {
	pairs := make([]engine.Pair, len(rs))
	for i, r := range rs {
		if len(r.Expressions) != 1 {
			return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
		}
		m, ok := r.Expressions[0].Value.(map[string]interface{})
		if !ok {
			return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
		}
		res, ok := m["resource"].(string)
		if !ok {
			return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
		}
		act, ok := m["action"].(string)
		if !ok {
			return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
		}
		pairs[i] = engine.Pair{Resource: engine.Resource(res), Action: engine.Action(act)}
	}

	return pairs, nil
}

func (s *State) projectsFromResults(rs rego.ResultSet) ([]string, error) {
	if len(rs) != 1 {
		return nil, &ErrUnexpectedResultSet{set: rs}
	}
	r := rs[0]
	if len(r.Expressions) != 1 {
		return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
	}
	projects, err := s.stringArrayFromResults(r.Expressions)
	if err != nil {
		return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
	}
	return projects, nil
}

func (s *State) stringArrayFromResults(exps []*rego.ExpressionValue) ([]string, error) {
	rawArray, ok := exps[0].Value.([]interface{})
	if !ok {
		return nil, &ErrUnexpectedResultExpression{exps: exps}
	}
	vals := make([]string, len(rawArray))
	for i := range rawArray {
		v, ok := rawArray[i].(string)
		if !ok {
			return nil, errors.New("error casting to string")
		}
		vals[i] = v
	}
	return vals, nil
}

func (s *State) rulesFromResults(rs rego.ResultSet) ([]engine.Rule, error) {
	rules := make([]engine.Rule, 0)
	if len(rs) != 1 {
		return nil, &ErrUnexpectedResultSet{set: rs} // or something similar
	}
	r := rs[0]
	if len(r.Expressions) != 1 {
		return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
	}

	exp := r.Expressions[0]
	ms, ok := exp.Value.([]interface{})
	if !ok {
		return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
	}
	var err error
	rules, err = s.convertRulesList(ms, rules)
	if err != nil {
		return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
	}

	return rules, nil
}

func (s *State) projectMappingsFromResults(rs rego.ResultSet) (map[string][]engine.Rule, error) {
	ruleMap := make(map[string][]engine.Rule)
	for _, r := range rs {
		if len(r.Expressions) != 1 {
			return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
		}

		exp := r.Expressions[0]
		outer, ok := exp.Value.([]interface{})
		if !ok {
			return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
		}
		if len(outer) != 1 {
			return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
		}
		projectMap := outer[0].(map[string]interface{})
		if !ok {
			return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
		}

		for projectID, ms := range projectMap {
			rules := make([]engine.Rule, 0)
			ms, ok := ms.([]interface{})
			if !ok {
				return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
			}
			rules, err := s.convertRulesList(ms, rules)
			if err != nil {
				return nil, &ErrUnexpectedResultExpression{exps: r.Expressions}
			}
			ruleMap[projectID] = rules
		}
	}

	return ruleMap, nil
}

func (s *State) convertRulesList(ms []interface{}, rules []engine.Rule) ([]engine.Rule, error) {
	for i := range ms {
		m, ok := ms[i].(map[string]interface{})
		if !ok {
			return nil, errors.New("error converting map[string]interface{}")
		}
		t, ok := m["type"].(string)
		if !ok {
			return nil, errors.New("error converting type")
		}
		vs, ok := m["values"].([]interface{})
		if !ok {
			return nil, errors.New("error converting values")
		}
		vals := make([]string, len(vs))
		for j := range vs {
			if val, ok := vs[j].(string); ok {
				vals[j] = val
			}
		}
		rules = append(rules, engine.Rule{Type: t, Values: vals})
	}
	return rules, nil
}

// SetPolicies replaces OPA's data with a new set of policies, and resets the
// partial evaluation cache
func (s *State) SetPolicies(ctx context.Context, policies map[string]interface{}) error {
	s.store = inmem.NewFromObject(map[string]interface{}{
		"policies": policies,
	})

	return s.initPartialResult(ctx)
}

// Spike TODO: Can we have a separate method for just setting the rule mappings?
// OR does the entire OPA store have to be re-evaluated at once. IF that's true,
// should we have the same OPA instance in general for rules?
//
// V2SetPolicies replaces OPA's data with a new set of policies and roles,
// and resets the partial evaluation cache for v2
func (s *State) V2SetPolicies(
	ctx context.Context, policyMap map[string]interface{},
	roleMap map[string]interface{}, ruleMap map[string][]interface{}) error {
	// TODO: v2 doesn't care about rules
	s.v2Store = inmem.NewFromObject(map[string]interface{}{
		"policies": policyMap,
		"roles":    roleMap,
		"rules":    ruleMap,
	})

	return s.initPartialResultV2(ctx)
}

// V2p1SetPolicies replaces OPA's data with a new set of policies and roles,
// and resets the partial evaluation cache for v2
func (s *State) V2p1SetPolicies(
	ctx context.Context, policyMap map[string]interface{},
	roleMap map[string]interface{}, ruleMap map[string][]interface{}) error {
	s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
		"policies": policyMap,
		"roles":    roleMap,
		"rules":    ruleMap,
	})

	return s.initPartialResultV2p1(ctx)
}

// ErrUnexpectedResultExpression is returned when one of the result sets
// expressions can't be made sense of
type ErrUnexpectedResultExpression struct {
	exps []*rego.ExpressionValue
}

func (e *ErrUnexpectedResultExpression) Error() string {
	return fmt.Sprintf("unexpected result expressions: %v", e.exps)
}

// ErrUnexpectedResultSet is returned when the result set of an OPA query
// can't be made sense of
type ErrUnexpectedResultSet struct {
	set rego.ResultSet
}

func (e *ErrUnexpectedResultSet) Error() string {
	return fmt.Sprintf("unexpected result set: %v", e.set)
}

// ErrEvaluation is returned when a query evaluation returns an error.
type ErrEvaluation struct {
	e error
}

func (e *ErrEvaluation) Error() string {
	return fmt.Sprintf("error in query evaluation: %s", e.e.Error())
}
