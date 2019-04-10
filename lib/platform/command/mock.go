package command

import (
	"fmt"
	"io"
	"runtime"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/chef/automate/lib/stringutils"
)

var _ Executor = &MockExecutor{}

type expectationStore map[string][]*Expectation

// A MockExecutor is an Executor that can be used to mock calls to
// external commands during the tests.
type MockExecutor struct {
	expectations        expectationStore
	testing             *testing.T
	passthroughExecutor Executor
}

// An Expectation represents one or more allowed function calls
// against the MockExecutor.
type Expectation struct {
	method       string
	expected     ExpectedCommand
	allowedCalls int64
	callCount    int64
	retVals      []interface{}
	from         string
}

// An ExpectedCommand represents a command that we expect the
// MockExecutor will be used to run. Calls to the MockExecutor are
// checked against expectations which include the ExpectedCommand. An
// empty ExpectedCommand can be used to match any command.
type ExpectedCommand struct {
	Cmd              string
	Args             []string
	Env              []string
	PipeToFilename   string
	PipeFromFilename string
	Timeout          time.Duration
	Stdout           io.Writer
}

// Return sets the return value for an expectation
func (e *Expectation) Return(vals ...interface{}) *Expectation {
	e.retVals = vals
	return e
}

// Once sets the expected call count on an expectation. If a matching
// command is called more than once, the test will fail.
func (e *Expectation) Once() *Expectation {
	e.allowedCalls = 1
	return e
}

// Times sets the expected call count on an expectation. If a matching
// command is called more than the given number of times, the test
// will fail.  If the command is called under the expected number of
// times, AssertAllCalled() will fail.
func (e *Expectation) Times(i int64) *Expectation {
	e.allowedCalls = i
	return e
}

type MockExecutorOption func(m *MockExecutor)

// Passthrough allows us to both make assertions about what commands
// are being called while still executing them using the provided
// executor.
func Passthrough(e Executor) MockExecutorOption {
	return func(m *MockExecutor) {
		m.passthroughExecutor = e
	}
}

// NewMockExecutor returns a MockExecutor. A MockExecutor will fail
// the test if any unexpected function calls are received. Expected
// function calls can be added with the Expect function. At the end of
// the tests, expectations can be checked with the CheckAllCalled()
// function.
func NewMockExecutor(t *testing.T, opts ...MockExecutorOption) *MockExecutor {
	m := &MockExecutor{
		testing: t,
	}

	for _, o := range opts {
		o(m)
	}

	return m
}

// AssertAllCalled will fail the tests if any expected commands have
// not been called during the test.
func (m *MockExecutor) AssertAllCalled() {
	notCalled := make([]*Expectation, 0, 2)
	for _, exps := range m.expectations {
		for _, e := range exps {
			if e.callCount == 0 || e.callCount < e.allowedCalls {
				notCalled = append(notCalled, e)
			}
		}
	}

	if len(notCalled) > 0 {
		msg := strings.Builder{}
		msg.WriteString("The following methods were expected but not called:\n")
		for _, e := range notCalled {
			msg.WriteString(fmt.Sprintf("\n%s\n", prettyExpectation(e)))
		}
		m.testing.Fatal(msg.String())
	}
}

func expectedCommandFromCmd(c *cmd) ExpectedCommand {
	return ExpectedCommand{
		Args:             c.args,
		Env:              c.env,
		Timeout:          c.timeout,
		PipeToFilename:   c.pipeToFilename,
		PipeFromFilename: c.pipeFromFilename,
		Stdout:           c.stdout,
	}
}

func (m *MockExecutor) findMatchingExpectation(cmd string, expectedCmd ExpectedCommand) *Expectation {
	exps, ok := m.expectations[cmd]
	if !ok {
		return nil
	}
	for _, e := range exps {
		// Skip if we've used up the expected calls on this function
		if e.allowedCalls != 0 && e.callCount >= e.allowedCalls {
			continue
		}

		if exactMatch(e.expected.Args, expectedCmd.Args) &&
			contains(e.expected.Env, expectedCmd.Env) &&
			e.expected.Cmd == expectedCmd.Cmd &&
			e.expected.Timeout == expectedCmd.Timeout &&
			e.expected.PipeToFilename == expectedCmd.PipeToFilename &&
			e.expected.PipeFromFilename == expectedCmd.PipeFromFilename &&
			e.expected.Stdout == expectedCmd.Stdout {
			return e
		}

		// Special case to allow mocking without any argument assertions
		if len(e.expected.Args) == 0 && len(e.expected.Env) == 0 {
			return e
		}
	}
	return nil
}

// contains returns true if all elements of b are contained in a
func contains(a, b []string) bool {
	for _, bb := range b {
		if !stringutils.SliceContains(a, bb) {
			return false
		}
	}
	return true
}

func exactMatch(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}

	for i, aa := range a {
		if b[i] != aa {
			return false
		}
	}
	return true
}

// Expect adds an expectation to the MockExecutor, returning it for
// further modification. If future calls to the MockExecutor match the
// expectation, any results (see Returns) are returned to the user.
func (m *MockExecutor) Expect(funcName string, e ExpectedCommand) *Expectation {
	newExpectation := &Expectation{
		method:   funcName,
		expected: e,
	}

	pcs := make([]uintptr, 1)
	runtime.Callers(2, pcs)
	frames := runtime.CallersFrames(pcs)
	frame, _ := frames.Next()
	newExpectation.from = fmt.Sprintf("%s:%d", frame.File, frame.Line)

	if m.expectations == nil {
		m.expectations = make(expectationStore)
	}
	existingExpectations, ok := m.expectations[funcName]
	if !ok {
		m.expectations[funcName] = []*Expectation{newExpectation}
	} else {
		m.expectations[funcName] = append(existingExpectations, newExpectation)
	}
	return newExpectation
}

func prettyExpectation(e *Expectation) string {
	var allowedCalls string

	if e.allowedCalls == 0 {
		allowedCalls = "any"
	} else {
		allowedCalls = strconv.FormatInt(e.allowedCalls, 10)
	}
	cmdString := prettyCommand(e.method, e.expected)

	return fmt.Sprintf("%s    Returning: %v\n    Allowed Calls: %s\n    Actual Calls: %d\n    Defined at: %s\n",
		cmdString, e.retVals, allowedCalls, e.callCount, e.from)
}

func prettyCommand(method string, e ExpectedCommand) string {
	var inputFile string
	var outputFile string
	var timeout string

	if e.PipeFromFilename != "" {
		inputFile = fmt.Sprintf("    Input File: %s\n", e.PipeFromFilename)
	}

	if e.PipeToFilename != "" {
		outputFile = fmt.Sprintf("    Output File: %s\n", e.PipeToFilename)
	}

	if e.Timeout != 0 {
		timeout = fmt.Sprintf("    Timeout: %s\n", e.Timeout)
	}

	return fmt.Sprintf("    Method: %s\n    Command: %s\n    Arguments: %v\n    Environ: %v\n%s%s%s",
		method, e.Cmd, e.Args, e.Env, inputFile, outputFile, timeout)
}

func (m *MockExecutor) called(funcName string, command string, opts []Opt) *Expectation {
	c := &cmd{}
	err := c.processOpts(opts)
	if err != nil {
		m.testing.Fatalf("Failed to apply process options: %s", err.Error())
	}
	expectedCmd := expectedCommandFromCmd(c)
	expectedCmd.Cmd = command
	exp := m.findMatchingExpectation(funcName, expectedCmd)
	if exp == nil {
		msg := new(strings.Builder)
		msg.WriteString("\nNo matching expectation for the following command:\n")
		msg.WriteString(prettyCommand(funcName, expectedCmd))
		msg.WriteString("\nWe have the following expectations:\n")
		for _, exps := range m.expectations {
			for _, e := range exps {
				msg.WriteString("\n")
				msg.WriteString(prettyExpectation(e))
			}
		}

		msg.WriteString("\nStacktrace:\n")
		pcs := make([]uintptr, 10)
		runtime.Callers(1, pcs)
		frames := runtime.CallersFrames(pcs)
		frame, ok := frames.Next()
		for ok {
			if frame.File != "" {
				msg.WriteString(fmt.Sprintf("    %s:%d\n", frame.File, frame.Line))
			}
			frame, ok = frames.Next()
		}
		panic(msg.String())
	}

	exp.callCount++
	return exp
}

//
// Executor implementation
//

// Start is a mock of the Executor.Start function
func (m *MockExecutor) Start(cmd string, opts ...Opt) (WaitFunc, error) {
	exp := m.called("Start", cmd, opts)
	if m.passthroughExecutor != nil {
		return m.passthroughExecutor.Start(cmd, opts...)
	}

	// we don't let you mock the wait object yet
	waitRet := WaitFunc(func() error { return nil })
	if len(exp.retVals) > 0 {
		e, ok := exp.retVals[0].(error)
		if !ok {
			return waitRet, nil
		}
		return waitRet, e
	}

	return waitRet, nil
}

// Run is a mock of the Executor.Run function
func (m *MockExecutor) Run(cmd string, opts ...Opt) error {
	exp := m.called("Run", cmd, opts)
	if m.passthroughExecutor != nil {
		return m.passthroughExecutor.Run(cmd, opts...)
	}

	if len(exp.retVals) > 0 {
		e, ok := exp.retVals[0].(error)
		if !ok {
			return nil
		}
		return e
	}

	return nil
}

// CombinedOutput is a mock of the Executor.CombinedOutput function
func (m *MockExecutor) CombinedOutput(cmd string, opts ...Opt) (string, error) {
	exp := m.called("CombinedOutput", cmd, opts)
	if m.passthroughExecutor != nil {
		return m.passthroughExecutor.CombinedOutput(cmd, opts...)
	}

	if len(exp.retVals) > 0 && len(exp.retVals) != 2 {
		m.testing.Fatalf("Wrong number of mocked return arguments for CombinedOutput (expected 2, have %d)", len(exp.retVals))
	}

	if len(exp.retVals) == 2 {
		e, ok := exp.retVals[1].(error)
		if !ok {
			return exp.retVals[0].(string), nil
		}

		return exp.retVals[0].(string), e
	}

	return "", nil
}

// Output is a mock of the Executor.Output function
func (m *MockExecutor) Output(cmd string, opts ...Opt) (string, error) {
	exp := m.called("Output", cmd, opts)
	if m.passthroughExecutor != nil {
		return m.passthroughExecutor.Output(cmd, opts...)
	}

	if len(exp.retVals) > 0 && len(exp.retVals) != 2 {
		m.testing.Fatalf("Wrong number of mocked return arguments for Output (expected 2, have %d)", len(exp.retVals))
	}

	if len(exp.retVals) == 2 {
		e, ok := exp.retVals[1].(error)
		if !ok {
			return exp.retVals[0].(string), nil
		}

		return exp.retVals[0].(string), e
	}

	return "", nil
}
