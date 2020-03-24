package opa

// nolint:lll
//go:generate go-bindata -pkg $GOPACKAGE -o policy.bindata.go -ignore .*_test.rego -ignore Makefile -ignore README\.md policy/...

import (
	"context"
	"encoding/json"
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
	log                    logger.Logger
	store                  storage.Store
	v2p1Store              storage.Store
	queries                map[string]ast.Body
	compiler               *ast.Compiler
	modules                map[string]*ast.Module
	partialAuth            rego.PartialResult
	v2PreparedEvalProjects rego.PreparedEvalQuery
}

// this needs to match the hardcoded OPA policy document we've put in place
const (
	authzQuery              = "data.authz.authorized"
	filteredPairsQuery      = "data.authz.introspection.authorized_pair[_]"
	authzProjectsV2Query    = "data.authz_v2.authorized_project[project]"
	filteredPairsV2Query    = "data.authz_v2.introspection.authorized_pair[_]"
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
	s := State{
		log:       l,
		store:     inmem.New(),
		v2p1Store: inmem.New(),
		queries: map[string]ast.Body{
			authzQuery:              authzQueryParsed,
			filteredPairsQuery:      filteredPairsQueryParsed,
			authzProjectsV2Query:    authzProjectsV2QueryParsed,
			filteredPairsV2Query:    filteredPairsV2QueryParsed,
			filteredProjectsV2Query: filteredProjectsV2QueryParsed,
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

	// this compiler is for the ad-hoc queries (those *not* having partial results prepped)
	compiler, err := s.newCompiler()
	if err != nil {
		return errors.Wrap(err, "init compiler")
	}
	s.compiler = compiler
	return nil
}

// Returns a prepared query that can be executed. The result set will contain a
// binding for a variable named 'project' that contains the name (string) of a
// project the subject has access to.
func (s *State) makeAuthorizedProjectPreparedQuery(ctx context.Context) error {
	compiler, err := s.newCompiler()
	if err != nil {
		return err
	}

	r := rego.New(
		rego.Store(s.v2p1Store),
		rego.Compiler(compiler),
		rego.ParsedQuery(s.queries[authzProjectsV2Query]),
		rego.DisableInlining([]string{
			"data.authz_v2.denied_project",
		}),
	)

	// rego.Rego#Partial must be used because the rego.Rego#PartialResult and
	// rego.Rego#PrepareForEval(rego.WithPartialEval()) APIs (currently) do not
	// support queries that ask for a single decision, i.e., they cannot contain
	// variables or multiple expressions.
	pq, err := r.Partial(ctx)
	if err != nil {
		return err
	}

	// Partial evaluation returns a set of queries and support modules that have
	// to be compiled. Add the support modules to compiler.
	for i, module := range pq.Support {
		compiler.Modules[fmt.Sprintf("support%d", i)] = module
	}

	// Make a module/ruleset for the queries returned by partial evaluation.
	// This will be the ruleset that is ultimately queried for authorization
	// checks. Add the module to the compiler.
	main := &ast.Module{
		Package: ast.MustParsePackage("package __partialauthzv2"),
	}

	for i := range pq.Queries {
		rule := &ast.Rule{
			Module: main,
			Head:   ast.NewHead("authorized_project", ast.VarTerm("project")),
			Body:   pq.Queries[i],
		}
		main.Rules = append(main.Rules, rule)
	}

	compiler.Modules["__partialauthzv2"] = main

	// Finally, compile everything and make a prepared query that can be
	// executed for authorization checks.
	compiler.Compile(compiler.Modules)

	if compiler.Failed() {
		return compiler.Errors
	}

	r2 := rego.New(
		rego.Store(s.v2p1Store),
		rego.Compiler(compiler),
		rego.Query("data.__partialauthzv2.authorized_project[project]"),
	)

	query, err := r2.PrepareForEval(ctx)
	if err != nil {
		return errors.Wrap(err, "prepare query for eval (authorized_project)")
	}

	s.v2PreparedEvalProjects = query

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

// DumpData is a bit fast-and-loose when it comes to error checking; it's not
// meant to be used in production. Anywhere you have an OPA engine struct (i.e.
// `State`), you can use either one of these on it and it'll log the store
// contents.
func (s *State) DumpData(ctx context.Context) error {
	return dumpData(ctx, s.store, s.log)
}

func (s *State) DumpDataV2p1(ctx context.Context) error {
	return dumpData(ctx, s.v2p1Store, s.log)
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

	jsonData, err := json.Marshal(data)
	if err != nil {
		return err
	}

	l.Infof("data: %s", jsonData)
	return store.Commit(ctx, txn)
}

// ProjectsAuthorized evaluates whether a given [subject, resource, action,
// projects] tuple is authorized and returns the list of associated allowed
// projects from the set of requested projects passed in.
func (s *State) ProjectsAuthorized(
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
	resultSet, err := s.v2PreparedEvalProjects.Eval(ctx, rego.EvalParsedInput(input))
	if err != nil {
		return []string{}, &EvaluationError{e: err}
	}

	return s.projectsFromPreparedEvalQuery(resultSet)
}

// FilterAuthorizedPairs passes the pairs into OPA, lets it take care of the
// filtering, and returns the result (sub)list
func (s *State) FilterAuthorizedPairs(
	ctx context.Context,
	subjects engine.Subjects,
	pairs []engine.Pair,
) ([]engine.Pair, error) {

	opaInput := map[string]interface{}{
		"subjects": subjects,
		"pairs":    pairs,
	}

	rs, err := s.evalQuery(ctx, s.queries[filteredPairsV2Query], opaInput, s.v2p1Store)
	if err != nil {
		return nil, &EvaluationError{e: err}
	}

	return s.pairsFromResults(rs)
}

// FilterAuthorizedProjects passes the pairs of all action/resources into OPA,
// lets it take care of the filtering,
// and returns the projects associated with the resulting (sub)list.
func (s *State) FilterAuthorizedProjects(
	ctx context.Context, subjects engine.Subjects) ([]string, error) {

	opaInput := map[string]interface{}{
		"subjects": subjects,
	}

	// NB: V2.1 only, so s.v2p1Store used here
	rs, err := s.evalQuery(ctx, s.queries[filteredProjectsV2Query], opaInput, s.v2p1Store)
	if err != nil {
		return nil, &EvaluationError{e: err}
	}

	return s.projectsFromPartialResults(rs)
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
			return nil, &UnexpectedResultExpressionError{exps: r.Expressions}
		}
		m, ok := r.Expressions[0].Value.(map[string]interface{})
		if !ok {
			return nil, &UnexpectedResultExpressionError{exps: r.Expressions}
		}
		res, ok := m["resource"].(string)
		if !ok {
			return nil, &UnexpectedResultExpressionError{exps: r.Expressions}
		}
		act, ok := m["action"].(string)
		if !ok {
			return nil, &UnexpectedResultExpressionError{exps: r.Expressions}
		}
		pairs[i] = engine.Pair{Resource: engine.Resource(res), Action: engine.Action(act)}
	}

	return pairs, nil
}

func (s *State) projectsFromPartialResults(rs rego.ResultSet) ([]string, error) {
	if len(rs) != 1 {
		return nil, &UnexpectedResultSetError{set: rs}
	}
	r := rs[0]
	if len(r.Expressions) != 1 {
		return nil, &UnexpectedResultExpressionError{exps: r.Expressions}
	}
	projects, err := s.stringArrayFromResults(r.Expressions)
	if err != nil {
		return nil, &UnexpectedResultExpressionError{exps: r.Expressions}
	}
	return projects, nil
}

func (s *State) stringArrayFromResults(exps []*rego.ExpressionValue) ([]string, error) {
	rawArray, ok := exps[0].Value.([]interface{})
	if !ok {
		return nil, &UnexpectedResultExpressionError{exps: exps}
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

func (s *State) projectsFromPreparedEvalQuery(rs rego.ResultSet) ([]string, error) {
	projectsFound := make(map[string]bool, len(rs))
	result := make([]string, 0, len(rs))
	for i := range rs {
		var ok bool
		proj, ok := rs[i].Bindings["project"].(string)
		if !ok {
			return nil, &UnexpectedResultExpressionError{exps: rs[i].Expressions}
		}
		if !projectsFound[proj] {
			result = append(result, proj)
			projectsFound[proj] = true
		}
	}
	return result, nil
}

// SetPolicies replaces OPA's data with a new set of policies and roles
// and resets the partial evaluation cache for v2.1
func (s *State) SetPolicies(
	ctx context.Context, policyMap map[string]interface{},
	roleMap map[string]interface{}) error {
	s.v2p1Store = inmem.NewFromObject(map[string]interface{}{
		"policies": policyMap,
		"roles":    roleMap,
	})

	return s.makeAuthorizedProjectPreparedQuery(ctx)
}

// UnexpectedResultExpressionError is returned when one of the result sets
// expressions can't be made sense of
type UnexpectedResultExpressionError struct {
	exps []*rego.ExpressionValue
}

func (e *UnexpectedResultExpressionError) Error() string {
	return fmt.Sprintf("unexpected result expressions: %v", e.exps)
}

// UnexpectedResultSetError is returned when the result set of an OPA query
// can't be made sense of
type UnexpectedResultSetError struct {
	set rego.ResultSet
}

func (e *UnexpectedResultSetError) Error() string {
	return fmt.Sprintf("unexpected result set: %v", e.set)
}

// EvaluationError is returned when a query evaluation returns an error.
type EvaluationError struct {
	e error
}

func (e *EvaluationError) Error() string {
	return fmt.Sprintf("error in query evaluation: %s", e.e.Error())
}
