package status

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"strings"

	"github.com/sirupsen/logrus"
)

// CmdResult represents the result of an invocation of the chef-automate CLI. It
// translates errors into standard error codes and allows us to serialize
// command results to JSON.
type CmdResult struct {
	// The command that was run with args
	Command string `json:"command"`
	// Either Ok ("OK") or Failure ("ERROR") indicating whether the operation
	// succeeded or not.
	Status string `json:"status"`
	// Integer error code uniquely identifying the error. Also used as the exit
	// code for the command (will be 0 when Status is Ok).
	ErrorCode int `json:"error_code"`
	// Human readable summary of the error.
	ErrorDescription string `json:"error_description"`
	// Description of the inner error that caused the failure. Useful for
	// debugging; may not be a user-friendly description.
	ErrorCause string `json:"error_cause"`
	// The call stack trace.
	ErrorStackTrace string `json:"error_stack_trace"`
	// Optional recovery instructions
	ErrorRecovery string `json:"error_recovery"`
	// The error type
	ErrorType string `json:"error_type"`

	Result interface{} `json:"result,omitempty"`
}

// GlobalResult is used to report command results in structured format (JSON) in
// support of managed service integrations.
var GlobalResult interface{}

// NewCmdResult takes an error and returns a new instance of CmdResult.
func NewCmdResult(err error) *CmdResult {
	cr := &CmdResult{
		Command: strings.Join(os.Args, " "),
	}

	if err == nil {
		cr.Status = Ok
		cr.ErrorCode = 0
		if GlobalResult != nil {
			cr.Result = GlobalResult
		}
		return cr
	}

	cr.Status = Failure
	cr.ErrorCode = ExitCode(err)
	cr.ErrorDescription = Description(err)
	cr.ErrorCause = Cause(err).Error()
	cr.ErrorStackTrace = fmt.Sprintf("Stack trace: %+v", StackTrace(err))
	cr.ErrorRecovery = Recovery(err)
	cr.ErrorType = ErrorType(err)
	return cr
}

// WriteFile serializes the CmdResult to JSON and writes the result to path.
// Errors encountered are logged and ignored.
func (cr *CmdResult) WriteFile(path string) {
	data, err := cr.toJSON()
	if err != nil {
		return
	}

	err = ioutil.WriteFile(path, data, 0640)
	if err != nil {
		msg := fmt.Sprintf("unable to write output to %s", path)
		logrus.WithError(err).Error(msg)
	}
}

func (cr *CmdResult) toJSON() ([]byte, error) {
	data, err := json.Marshal(cr)
	if err != nil {
		logrus.WithError(err).Error("unable to convert result to JSON")
		return nil, err
	}
	return data, nil
}
