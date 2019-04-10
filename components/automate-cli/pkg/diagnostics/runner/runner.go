package runner

import (
	"fmt"
	"runtime"

	"go.uber.org/multierr"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
)

type verificationTestContext struct {
	diagnostics.TestContext
	componentName string
	reporter      Reporter
	hasFailures   bool
}

func toVerifiableContext(tstContext diagnostics.TestContext, componentName string, reporter Reporter) *verificationTestContext {
	return &verificationTestContext{
		TestContext:   tstContext,
		componentName: componentName,
		reporter:      reporter,
	}
}

func (vctx *verificationTestContext) Errorf(format string, args ...interface{}) {
	vctx.hasFailures = true
	vctx.reporter.ReportVerifyFailure(vctx.componentName, fmt.Sprintf(format, args...))
}

func (vctx *verificationTestContext) FailNow() {
	vctx.hasFailures = true
	runtime.Goexit()
}

func (vctx *verificationTestContext) HasFailures() bool {
	return vctx.hasFailures
}

func runVerifyInGoRoutine(vctx diagnostics.VerificationTestContext, diagnostic diagnostics.Diagnostic) {
	// We run each test in a goroutine so that we can use the testify framework. By running in a goroutine,
	// calling the functions in the require package will continue exit correctly when they call FailNow on
	// the test context
	doneChan := make(chan struct{})
	go func() {
		defer close(doneChan)
		diagnostic.Verify(vctx)
	}()

	<-doneChan
}

// Runner is responsible for running diagnostics
type Runner struct {
	registeredDiagnostics []diagnostics.Diagnostic
	tstContext            diagnostics.TestContext
	reporter              Reporter
	runGeneratePhase      bool
	runVerifyPhase        bool
	runCleanupPhase       bool
	matchingTags          []diagnostics.TagFilter
}

// Opt are functional optionals for Runner
type Opt func(*Runner)

// WithDiagnostics sets the diagnostics the runner will use. If not set,
// it will use all registered ones
func WithDiagnostics(ds ...diagnostics.Diagnostic) Opt {
	return func(runner *Runner) {
		if runner.registeredDiagnostics == nil {
			runner.registeredDiagnostics = []diagnostics.Diagnostic{}
		}
		runner.registeredDiagnostics = append(runner.registeredDiagnostics, ds...)
	}
}

// WithTestContext sets the test context to use. The default one will not
// be able to talk to deployment service
func WithTestContext(tstContext diagnostics.TestContext) Opt {
	return func(runner *Runner) {
		runner.tstContext = tstContext
	}
}

// WithSkipGeneratePhase will skip the generate phase if set to true.
// Defaults to false
func WithSkipGeneratePhase(skip bool) Opt {
	return func(runner *Runner) {
		runner.runGeneratePhase = !skip
	}
}

// WithSkipVerifyPhase will skip the verify phase if set to true.
// Defaults to false
func WithSkipVerifyPhase(skip bool) Opt {
	return func(runner *Runner) {
		runner.runVerifyPhase = !skip
	}
}

// WithSkipCleanupPhase will skip the cleanup phase if set to true.
// Defaults to false
func WithSkipCleanupPhase(skip bool) Opt {
	return func(runner *Runner) {
		runner.runCleanupPhase = !skip
	}
}

func WithMatchingTags(tagFilters []diagnostics.TagFilter) Opt {
	return func(runner *Runner) {
		runner.matchingTags = tagFilters
	}
}

// New returns a new Runner
func New(opts ...Opt) *Runner {
	runner := &Runner{
		tstContext:       diagnostics.NewTestContext(nil),
		reporter:         NewDefaultReporter(),
		runGeneratePhase: true,
		runVerifyPhase:   true,
		runCleanupPhase:  true,
	}

	for _, o := range opts {
		o(runner)
	}

	if runner.registeredDiagnostics == nil {
		runner.registeredDiagnostics = diagnostics.RegisteredDiagnostics
	}

	runner.filterDiagnostics()

	return runner
}

func (r *Runner) filterDiagnostics() {
	if r.matchingTags == nil || len(r.matchingTags) == 0 {
		return
	}

	filtered := []diagnostics.Diagnostic{}

	for _, diag := range r.registeredDiagnostics {
		if diag.MatchesFilters(r.matchingTags) {
			filtered = append(filtered, diag)
		}
	}

	r.registeredDiagnostics = filtered
}

// Run runs the diagnostics
func (r *Runner) Run() error {
	errs := []error{}
	failed := false
	if r.runGeneratePhase {
		for _, diagnostic := range r.registeredDiagnostics {
			if diagnostic.Generate == nil {
				continue
			}
			r.reporter.ReportGenerateStart(diagnostic.Name)
			err := diagnostic.Generate(r.tstContext)

			if err != nil {
				r.reporter.ReportGenerateErrored(diagnostic.Name, fmt.Sprintf("%+v", err))
				failed = true
				errs = append(errs, err)
			} else {
				r.reporter.ReportGenerateSuccess(diagnostic.Name)
			}
		}
	}

	if !failed {
		if r.runVerifyPhase {
			for _, diagnostic := range r.registeredDiagnostics {
				if diagnostic.Verify == nil {
					continue
				}
				vctx := toVerifiableContext(r.tstContext, diagnostic.Name, r.reporter)
				r.reporter.ReportVerifyStart(diagnostic.Name)
				runVerifyInGoRoutine(vctx, diagnostic)
				r.reporter.ReportVerifyFinish(diagnostic.Name)

				if vctx.HasFailures() {
					errs = append(errs, errors.Errorf("Verify %s failed", diagnostic.Name))
				}
			}
		}
	}

	if r.runCleanupPhase {
		for _, diagnostic := range r.registeredDiagnostics {
			if diagnostic.Cleanup == nil {
				continue
			}
			r.reporter.ReportCleanupStart(diagnostic.Name)
			err := diagnostic.Cleanup(r.tstContext)
			if err != nil {
				r.reporter.ReportCleanupErrored(diagnostic.Name, fmt.Sprintf("%+v", err))
				errs = append(errs, err)
			} else {
				r.reporter.ReportCleanupSuccess(diagnostic.Name)
			}
		}
	}

	return multierr.Combine(errs...)
}
