// Copyright © 2018 Chef Software

// Package command is a wrapper for os/exec that provides a mock-able
// interface to make testing easier.
package command

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"os/user"
	"strconv"
	"syscall"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/platform/sys"
)

type WaitFunc func() error

// Executor is an interface for running commands on the underlying
// system. The functions available are the same API exposed by
// exec.Cmd object, but modified to prefer string return, functional
// option, and hopefully more consistent error behavior.
type Executor interface {
	Run(string, ...Opt) error
	Start(string, ...Opt) (WaitFunc, error)
	Output(string, ...Opt) (string, error)
	CombinedOutput(string, ...Opt) (string, error)
}

type cmd struct {
	eCmd        *exec.Cmd
	args        []string
	ctx         context.Context
	cancel      context.CancelFunc
	stdin       io.Reader
	stdout      io.Writer
	stderr      io.Writer
	env         []string
	sysProcAttr *syscall.SysProcAttr

	pipeToFilename   string
	pipeFromFilename string

	pipeToFile   *os.File
	pipeFromFile *os.File

	// Convenient for logging, but the ctx above is what actually
	// controls the timeout.
	timeout time.Duration

	startTime time.Time
}

// Opt is an option that can be applied to a command run by the
// Executor.
type Opt func(c *cmd) error

// Args returns a command.Opt that sets the arguments for the command to be
// run. Note, this should NOT include the name of the command itself.
func Args(args ...string) Opt {
	return func(c *cmd) error {
		c.args = args
		return nil
	}
}

// Context returns a command.Opt that sets the context for the command
// execution. If the context is expires or is cancels during the
// command execution, the command will be killed and the command will
// return an error. Cannot be used with command.Timeout.
func Context(ctx context.Context) Opt {
	return func(c *cmd) error {
		if c.ctx != nil {
			return errors.New("cannot use Context option with context already set")
		}
		c.ctx = ctx
		return nil
	}
}

// Timeout returns a command.Opt that sets a timeout for the command
// execution. If the timeout expires, the command will be killed and
// the command will return an error. Cannot be used with
// command.Context.
func Timeout(t time.Duration) Opt {
	return func(c *cmd) error {
		if c.ctx != nil {
			return errors.New("cannot use Timeout option with context already set")
		}

		// 0 duration timeout means no timeout.
		if t == 0 {
			return nil
		}

		ctx, cancel := context.WithTimeout(context.Background(), t)
		c.timeout = t
		c.ctx = ctx
		c.cancel = cancel
		return nil
	}
}

// Stdin returns a command.Opt that sets the standard input of the
// command to a file descriptor fed by the given io.Reader.  Cannot be
// used with command.PipeFrom.
func Stdin(i io.Reader) Opt {
	return func(c *cmd) error {
		if i == nil {
			return nil
		}
		if c.stdin != nil {
			return errors.New("standard input for command is already set")
		}
		if c.pipeFromFilename != "" {
			return errors.New("cannot specify both standard input reader and pipe from file")
		}
		c.stdin = i
		return nil
	}
}

// Stdout returns a command.Opt that sets the standard output of the
// command to a file descriptor connected to the given io.Writer.
// Only supported by the Run function.
func Stdout(i io.Writer) Opt {
	return func(c *cmd) error {
		if i == nil {
			return nil
		}
		if c.stdout != nil {
			return errors.New("standard output for command is already set")
		}
		c.stdout = i
		return nil
	}
}

// Stderr returns a command.Opt that sets the standard error of the
// command to a file descriptor connected to the given io.Writer.
// Only supported by the Run function.
func Stderr(i io.Writer) Opt {
	return func(c *cmd) error {
		if i == nil {
			return nil
		}
		if c.stderr != nil {
			return errors.New("standard error for command is already set")
		}
		c.stderr = i
		return nil
	}
}

// PipeFrom returns a command.Opt that sets the standard input of the
// command to the given file.  Cannot be used with command.Stdin.
func PipeFrom(filename string) Opt {
	return func(c *cmd) error {
		if c.stdin != nil {
			return errors.New("cannot specify both standard input reader and pipe from file")
		}
		c.pipeFromFilename = filename
		return nil
	}
}

// PipeTo returns a command.Opt that sets the standard output of the
// command to the given file. Can only be used with the Run()
// function.
func PipeTo(filename string) Opt {
	return func(c *cmd) error {
		c.pipeToFilename = filename
		return nil
	}
}

// Envvar returns a command.Opt that sets the given environment
// variable in the commands environment. Environment variables are
// added to the environment of the current process.
func Envvar(key string, value string) Opt {
	return func(c *cmd) error {
		if len(c.env) == 0 {
			c.env = make([]string, 0, 8)
		}
		c.env = append(c.env, fmt.Sprintf("%s=%s", key, value))
		return nil
	}
}

// AsUser returns a command.Opt that sets the user a command should be
// run as.
func AsUser(username string) Opt {
	return func(c *cmd) error {
		u, err := user.Lookup(username)
		if err != nil {
			return errors.Wrap(err, "user lookup")
		}

		uid, err := strconv.Atoi(u.Uid)
		if err != nil {
			return errors.Wrap(err, "converting uid to integer")
		}
		gid, err := strconv.Atoi(u.Gid)
		if err != nil {
			return errors.Wrap(err, "converting gid to integer")
		}

		c.sysProcAttr = sys.SysProcAttrWithCred(uint32(uid), uint32(gid))
		return nil
	}
}

type execExecutor struct{}

var DefaultExecutor Executor = &execExecutor{}

// NewExecExecutor returns an Executor backed by os.exec
func NewExecExecutor() Executor {
	return &execExecutor{}
}

// Run runs the given command, returning an error if it fails. This is
// a convenience method for when an Executor isn't needed (often because it is
// unlikely to be injected in test code)
func Run(cmd string, options ...Opt) error {
	return DefaultExecutor.Run(cmd, options...)
}

// Start starts the given command, returning an error if it fails. This is
// a convenience method for when an Executor isn't needed (often because it is
// unlikely to be injected in test code). The underlying files are closed once
// WaitFunc is called
func Start(cmd string, options ...Opt) (WaitFunc, error) {
	return DefaultExecutor.Start(cmd, options...)
}

// Output runs the given command, returning the standard output. An
// error if the command fails. This is a convenience method for when an
// Executor isn't needed (often because it is unlikely to be injected
// in test code)
func Output(cmd string, options ...Opt) (string, error) {
	return DefaultExecutor.Output(cmd, options...)
}

// CombinedOutput runs the given command, returning the standard output and
// standard error from the command. An error is returned if the
// command fails. This is a convenience method for when an Executor
// isn't needed (often because it is unlikely to be injected in test
func CombinedOutput(cmd string, options ...Opt) (string, error) {
	return DefaultExecutor.CombinedOutput(cmd, options...)
}

// StderrFromError returns the standard error output from the command
// that returned an error, if available.
func StderrFromError(e error) string {
	if ee, ok := e.(*exec.ExitError); ok {
		if ee.Stderr != nil {
			return string(ee.Stderr)
		}
	}
	return "(no stderr available)"
}

// Run runs the given command, returning an error if it fails. In the
// case of possible, the standard error output is returned in an
// exec.ExitError, if available.
func (e *execExecutor) Start(cmd string, options ...Opt) (WaitFunc, error) {
	c, err := e.makeCmd(cmd, options)
	if err != nil {
		return nil, errors.New("failed to construct command from given options")
	}
	defer c.cleanup()

	err = c.setupFiles()
	if err != nil {
		return nil, errors.Wrap(err, "failed to setup input/output files")
	}

	c.startTime = time.Now()
	b := new(bytes.Buffer)
	if c.stderr == nil {
		c.eCmd.Stderr = b
	}

	waitFunc := func() error {
		err := c.eCmd.Wait()
		if err != nil {
			// Populate Stderr on returned err. Cmd.Output() does
			// this automatically, but Cmd.Run() doesn't.
			if ee, ok := err.(*exec.ExitError); ok {
				ee.Stderr = b.Bytes()
			}
		}
		c.logCmdEnd(err)
		return err
	}

	c.logCmdStart()
	return waitFunc, c.eCmd.Start()
}

// Run runs the given command, returning an error if it fails. In the
// case of possible, the standard error output is returned in an
// exec.ExitError, if available.
func (e *execExecutor) Run(cmd string, options ...Opt) error {
	c, err := e.makeCmd(cmd, options)
	if err != nil {
		return errors.New("failed to construct command from given options")
	}
	defer c.cleanup()

	err = c.setupFiles()
	if err != nil {
		return errors.Wrap(err, "failed to setup input/output files")
	}

	c.logCmdStart()
	c.startTime = time.Now()
	b := new(bytes.Buffer)
	if c.stderr == nil {
		c.eCmd.Stderr = b
	}

	err = c.eCmd.Run()
	if err != nil {
		// Populate Stderr on returned err. Cmd.Output() does
		// this automatically, but Cmd.Run() doesn't.
		if ee, ok := err.(*exec.ExitError); ok {
			ee.Stderr = b.Bytes()
		}
	}
	c.logCmdEnd(err)
	return err
}

// Output runs the given command, returning the standard
// output. Cannot be used with options that redirect standard
// output. In the case of possible, the standard error output is
// returned in an exec.ExitError, if available. The returned output
// should be safe to read, but in the case of an error, reading the
// stderr from the returned error is preferred.
func (e *execExecutor) Output(cmd string, options ...Opt) (string, error) {
	c, err := e.makeCmd(cmd, options)
	if err != nil {
		return "", errors.New("failed to construct command from given options")
	}
	defer c.cleanup()

	if c.pipeToFilename != "" {
		return "", errors.New("PipeTo can only be used with Run")
	}

	err = c.setupFiles()
	if err != nil {
		return "", errors.Wrap(err, "failed to setup input/output files")
	}
	c.logCmdStart()
	c.startTime = time.Now()
	output, err := c.eCmd.Output()
	if output == nil {
		output = []byte{}
	}
	c.logCmdEnd(err)
	return string(output), err
}

// CombinedOutput runs the given command, returning the standard
// output and standard error. Cannot be used with options that
// redirect standard output. The returned output should be safe to
// read, but may not contain output from the command in the case of
// some errors.
func (e *execExecutor) CombinedOutput(cmd string, options ...Opt) (string, error) {
	c, err := e.makeCmd(cmd, options)
	defer c.cleanup()

	if err != nil {
		return "", errors.New("failed to construct command from given options")
	}

	if c.pipeToFilename != "" {
		return "", errors.New("PipeTo can only be used with Run")
	}

	err = c.setupFiles()
	if err != nil {
		return "", errors.Wrap(err, "failed to setup input/output files")
	}

	c.logCmdStart()
	c.startTime = time.Now()
	output, err := c.eCmd.CombinedOutput()
	if output == nil {
		output = []byte{}
	}
	c.logCmdEnd(err)
	return string(output), err
}

func (e *execExecutor) makeCmd(command string, options []Opt) (*cmd, error) {
	ret := &cmd{}
	err := ret.processOpts(options)
	if err != nil {
		return ret, err
	}

	if ret.ctx != nil {
		ret.eCmd = exec.CommandContext(ret.ctx, command, ret.args...)
	} else {
		ret.eCmd = exec.Command(command, ret.args...)
	}

	err = ret.applyOpts()
	return ret, err
}

func (c *cmd) setupFiles() error {
	if c.pipeToFilename != "" {
		f, err := os.OpenFile(c.pipeToFilename, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0600)
		if err != nil {
			return err
		}
		c.eCmd.Stdout = f
		c.pipeToFile = f
	}

	if c.pipeFromFilename != "" {
		f, err := os.Open(c.pipeFromFilename)
		if err != nil {
			return err
		}
		c.eCmd.Stdin = f
		c.pipeFromFile = f
	}
	return nil
}

func (c *cmd) cleanup() {
	if c.cancel != nil {
		c.cancel()
	}

	if c.pipeFromFile != nil {
		if err := c.pipeFromFile.Close(); err != nil {
			logrus.WithError(err).Warn("Close failed on PipeFromFile")
		}
	}

	if c.pipeToFile != nil {
		if err := c.pipeToFile.Sync(); err != nil {
			logrus.WithError(err).Warn("Sync failed on PipeToFile")
		}
		if err := c.pipeToFile.Close(); err != nil {
			logrus.WithError(err).Warn("Close failed on PipeToFile")
		}
	}
}

func (c *cmd) processOpts(opts []Opt) error {
	for _, o := range opts {
		err := o(c)
		if err != nil {
			return err
		}
	}
	return nil
}

func (c *cmd) applyOpts() error {
	if c.stdin != nil {
		c.eCmd.Stdin = c.stdin
	}

	if c.stdout != nil {
		c.eCmd.Stdout = c.stdout
	}

	if c.stderr != nil {
		c.eCmd.Stderr = c.stderr
	}

	if c.sysProcAttr != nil {
		c.eCmd.SysProcAttr = c.sysProcAttr
	}

	if len(c.env) > 0 {
		c.eCmd.Env = append(os.Environ(), c.env...)
	}
	return nil
}

func (c *cmd) logCmdStart() {
	fields := logrus.Fields{"cmd": c.eCmd.Args, "env": c.env}
	if c.timeout != 0 {
		fields["timeout"] = c.timeout
	}

	if c.pipeToFilename != "" {
		fields["output_file"] = c.pipeToFilename
	}

	if c.pipeFromFilename != "" {
		fields["input_file"] = c.pipeFromFilename
	}
	logrus.WithFields(fields).Debug("running external command")
}

func (c *cmd) logCmdEnd(err error) {
	fields := logrus.Fields{"cmd": c.eCmd.Args, "time_ms": time.Since(c.startTime).Seconds() * 1000}
	if err != nil {
		fields["stderr"] = StderrFromError(err)
		logrus.WithFields(fields).Debug("external command failed")
	} else {
		logrus.WithFields(fields).Debug("external command successful")
	}
}
