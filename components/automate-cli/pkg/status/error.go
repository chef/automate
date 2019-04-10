// Package status handles chef-automate CLI errors
//
// It's built on and intended to be used in conjunction with the annotated error
// types provided in the the github.com/pkg/errors library. We build on top of
// the extended error types and annotate with our own status information.
//
// The library can be used to initialize new errors:
//
//		err := status.New(status.MustBeRootError, "You must be root to run this command")
//
// Or to wrap an existing err:
//
//		_, err := ioutil.ReadAll("myfile")
//		if err != nil {
//			return status.Wrap(err, FileAccessError, "Unable to read myfile")
//		}
//
// Each error type is a public constant that reflects the integer representation
// of the error.
//
// To access the extended information in the error you can use status.Error
// interface:
//
//		if err, ok := err.(status.Error); ok {
//			fmt.Println(err.ExitCode())
//			fmt.Println(err.Description())
//
//			for _, f := range err.StackTrace() {
//				fmt.Printf("%+s:%d", f)
//			}
//		}
//
package status

import (
	"fmt"
	"io"

	"github.com/pkg/errors"
)

// Each invocation of the chef-automate CLI results in either a Ok or Failure
// result.
const (
	Ok      = "OK"
	Failure = "ERROR"
)

// When an invocation of the chef-automate CLI results in a Failure it exits with
// one of the following codes that identifies the error types.
const (
	IAMResetV1DatabaseError           = 116
	IAMUpgradeV2DatabaseError         = 115
	APIUnreachableError               = 114
	UnknownError                      = 113
	UpdateExecError                   = 112
	APIError                          = 100
	DeploymentServiceUnreachableError = 99
	DeploymentServiceCallError        = 98
	UnhealthyStatusError              = 97
	InvalidCommandArgsError           = 96
	PreflightError                    = 95
	DeployError                       = 94
	ConfigError                       = 93
	MustBeRootError                   = 92
	DiagnosticsError                  = 91
	FileAccessError                   = 90
	LicenseError                      = 89
	MarshalError                      = 88
	UpgradeError                      = 87
	BackupError                       = 86
	UninstallError                    = 85
	DownloadError                     = 84
	AirgapCreateInstallBundleError    = 83
	AirgapUnpackInstallBundleError    = 82
	ServiceStartError                 = 81
	ServiceUnloadError                = 80
	BackupRestoreError                = 79
	TimedOutError                     = 78
	PackageInstallError               = 77
	GatherLogsError                   = 76
	HabAPIError                       = 75
	HabCommandError                   = 74
	ProfileError                      = 73
	TraceError                        = 72
	CommandExecutionError             = 71
	DatabaseError                     = 70
	SnapshotChecksumMismatchError     = 69
	HabUserAccessError                = 68
)

// ErrorMetadata is a map keyed by an error code that returns a slice of strings
// with metadata for the error. The metadata is used both to translate between
// an error code and its type and a description, but also as a way to generate
// documentation.
var ErrorMetadata = map[int][]string{
	IAMResetV1DatabaseError:           {"116", "UnknownError", "Failed to reset IAM state to v1"},
	IAMUpgradeV2DatabaseError:         {"115", "UnknownError", "Failed to upgrade IAM to v2"},
	APIUnreachableError:               {"114", "APIUnreachableError", "Could not connect to Automate API"},
	UnknownError:                      {"113", "UnknownError", "An unknown issue occurred during execution"},
	UpdateExecError:                   {"112", "UpdateExecError", "An issue occurred when trying to run an auto-updated CLI executable"},
	APIError:                          {"100", "APIError", "An API error occurred during execution"},
	DeploymentServiceUnreachableError: {"99", "DeploymentServiceUnreachableError", "Unable to make a request to the deployment-service"},
	DeploymentServiceCallError:        {"98", "DeploymentServiceCallError", "A request to the deployment-service failed"},
	UnhealthyStatusError:              {"97", "UnhealthyStatusError", "System status is unhealthy"},
	InvalidCommandArgsError:           {"96", "InvalidCommandArgsError", "The arguments provided are invalid"},
	PreflightError:                    {"95", "PreflightError", "One or more preflight checks failed"},
	DeployError:                       {"94", "DeployError", "Unable to install, configure and start the service"},
	ConfigError:                       {"93", "ConfigError", "The configuration is invalid"},
	MustBeRootError:                   {"92", "MustBeRootError", "The command must be run as the root user"},
	DiagnosticsError:                  {"91", "DiagnosticsError", "One or more diagnostics checks failed"},
	FileAccessError:                   {"90", "FileAccessError", "Unable to access the file or directory"},
	LicenseError:                      {"89", "LicenseError", "The license is invalid, expired or incomplete"},
	MarshalError:                      {"88", "MarshalError", "Unable to convert or deconvert a textual representation of an internal object"},
	UpgradeError:                      {"87", "UpgradeError", "An issue occurred during the upgrade"},
	BackupError:                       {"86", "BackupError", "An issue occurred when creating or restoring a backup"},
	UninstallError:                    {"85", "UninstallError", "An issue occurred when attempting to uninstall Chef Automate"},
	DownloadError:                     {"84", "DownloadError", "An issue occurred when attempting to perform a file download"},
	AirgapCreateInstallBundleError:    {"83", "AirgapCreateInstallBundleError", "An issue occurred when attempting to create the airgap install bundle"},
	AirgapUnpackInstallBundleError:    {"82", "AirgapUnpackInstallBundleError", "An issue occurred when attempting to unpack the airgap install bundle"},
	ServiceStartError:                 {"81", "ServiceStartError", "Unable to start the habitat service"},
	ServiceUnloadError:                {"80", "ServiceUnloadError", "Unable to unload the habitat service"},
	BackupRestoreError:                {"79", "BackupRestoreError", "Unable to restore backup"},
	TimedOutError:                     {"78", "TimedOutError", "Timed out waiting for the operation to complete"},
	PackageInstallError:               {"77", "PackageInstallError", "Unable to install the habitat package"},
	GatherLogsError:                   {"76", "GatherLogsError", "Unable to complete log gathering"},
	HabAPIError:                       {"75", "HabAPIError", "An issue occurred when attempting to query the Habitat API"},
	HabCommandError:                   {"74", "HabCommandError", "An issue occurred when running a hab command"},
	ProfileError:                      {"73", "ProfileError", "An issue occurred when attempting to profile the request"},
	TraceError:                        {"72", "TraceError", "An issue occurred when attempting to trace the request"},
	CommandExecutionError:             {"71", "CommandExecutionError", "An issue occurred when running an executable command"},
	DatabaseError:                     {"70", "DatabaseError", "An issue occurred with the database"},
	SnapshotChecksumMismatchError:     {"69", "SnapshotChecksumMismatchError", "A file in the snapshot did not have the expected checksum"},
	HabUserAccessError:                {"68", "HabUserAccessError", "Unable to access file or directory with the hab user"},
}

// New takes an exit code and message and returns a new status.Error.
func New(exitCode int, message string) error {
	return &withStatus{
		error:      errors.New(message),
		exitStatus: newExitStatus(exitCode),
	}
}

// Errorf takes an exit code, format string and format args and returns a new
// status.Error.
func Errorf(exitCode int, fmtStr string, args ...interface{}) error {
	return New(exitCode, fmt.Sprintf(fmtStr, args...))
}

// Wrap takes an error, exit code and message and returns a status.Error that
// wraps the origin error.
func Wrap(err error, exitCode int, message string) error {
	// We aren't supposed to get nil err in this function but it can happen.
	// Rather than panic on a nil pointer some time later, we fix up the nil
	// error case to ensure we can get a useful message out to the end user
	if err == nil {
		err = errors.New(message)
	} else {
		err = errors.Wrap(err, message)
	}

	return &withStatus{
		error:      err,
		exitStatus: newExitStatus(exitCode),
	}
}

// Wrapf takes an error, exit code, format string and format args and returns a
// status.Error that wraps the origin error with a formatted message.
func Wrapf(err error, exitCode int, fmtStr string, args ...interface{}) error {
	return Wrap(err, exitCode, fmt.Sprintf(fmtStr, args...))
}

// Annotate takes an error and exit code and restores a status.Error that wraps
// and annotates the error with the help text associated with the error code.
// This is useful for adding exit code information but a wrap message isn't
// required.
func Annotate(err error, exitCode int) error {
	return &withStatus{
		// error:      errors.Wrap(err, ""),
		error:      err,
		exitStatus: newExitStatus(exitCode),
	}
}

// WithRecovery sets optional recovery instructions on an error
func WithRecovery(err error, rec string) error {
	recErr, ok := err.(Error)
	if ok {
		recErr.SetRecovery(rec)
		return recErr
	}

	return errors.Wrap(err, rec)
}

// Error is an interface that extends the errors interface to include an exit
// code and description.
type Error interface {
	error
	stackTracer
	causer
	exitCoder
	recoverer
}

// ExitCode follows the error cause chain until it finds the oldest status
// error and returns its error code.
func ExitCode(err error) int {
	if err == nil {
		return 0
	}

	coder := oldestExitCoder(err)
	if coder != nil {
		return coder.ExitCode()
	}

	// Make sure we return an UnknownError if we don't find a status error in
	// in the chain.
	return UnknownError
}

// Cause returns the first error in the causal chain
func Cause(err error) error {
	if err == nil {
		return nil
	}

	return errors.Cause(err)
}

// Recovery returns the first error in the causal chain that includes recovery
// information
func Recovery(err error) string {
	if err == nil {
		return ""
	}

	recovery := ""
	for {
		recErr, ok := err.(recoverer)
		if ok {
			if rec := recErr.Recovery(); rec != "" {
				// Only overwrite recovery instructions if they aren't blank
				recovery = rec
			}
		}

		cause, ok := err.(causer)
		if !ok {
			break
		}
		err = cause.Cause()
	}

	return recovery
}

func ErrorType(err error) string {
	if err == nil {
		return ""
	}

	coder := oldestExitCoder(err)
	if coder != nil {
		return coder.Type()
	}

	return "UnknownError"
}

// Description take an error and returns a formatted description of the error
// causal chain.
func Description(err error) string {
	if err == nil {
		return ""
	}

	coder := oldestExitCoder(err)
	if coder != nil {
		return fmt.Sprintf("%s: %s", coder.Description(), err.Error())
	}

	return err.Error()
}

// StackTrace follows the error cause chain until it finds the oldest traceable
// error and returns its stack trace.
func StackTrace(err error) errors.StackTrace {
	var oldestTrace stackTracer

	if err == nil {
		return nil
	}

	for {
		trace, ok := err.(stackTracer)
		if ok {
			oldestTrace = trace
		}

		cause, ok := err.(causer)
		if !ok {
			break
		}
		err = cause.Cause()
	}

	if oldestTrace != nil {
		return oldestTrace.StackTrace()
	}

	return nil
}

// IsStatusError returns true if the error is an error with a status
func IsStatusError(err error) bool {
	if err != nil {
		_, ok := err.(Error)
		return ok
	}
	return false
}

// stackTracer is an interface for an error that implements a stack trace.
type stackTracer interface {
	StackTrace() errors.StackTrace
}

// causer is an interface for an error that implements a cause.
type causer interface {
	Cause() error
}

// exitCode is an interface for an error that implements an exit code
type exitCoder interface {
	Type() string        // the error type as a string
	String() string      // type and description
	ExitCode() int       // the error code
	Description() string // a description why the error is used
}

type recoverer interface {
	Recovery() string   // get recovery instructions
	SetRecovery(string) // set recovery instructions
}

// withStatus is a composite wrapper error that includes an exitStatus
type withStatus struct {
	error
	*exitStatus
}

var _ Error = (*withStatus)(nil)

// Cause returns the wrapped errors cause
func (s *withStatus) Cause() error {
	return s.error
}

// StackTrace returns the wrapped errors stack trace
func (s *withStatus) StackTrace() errors.StackTrace {
	// If the original error has a stack trace then return it
	//	err, ok := errors.Cause(s.error).(stackTracer)
	//	if ok {
	//		return err.StackTrace()
	//	}
	//
	//	// Otherwise try and return our wrapped error's stack trace
	//	err, ok = s.error.(stackTracer)
	//	if ok {
	//		return err.StackTrace()
	//	}
	return StackTrace(s.error)
}

// Format partially implements the fmt.Formatter interface so that status
// Errors can be formatted correctly.
func (s *withStatus) Format(state fmt.State, verb rune) {
	msg := fmt.Sprintf("%d: %s", s.code, s.description)
	switch verb {
	case 'v':
		if state.Flag('+') {
			if s.recovery != "" {
				msg = fmt.Sprintf("%s. \n%s", msg, s.recovery)
			}

			_, _ = io.WriteString(state, msg)
			if fCause, ok := s.error.(fmt.Formatter); ok {
				fCause.Format(state, verb)
			}
			return
		}
		fallthrough
	case 's':
		if s.recovery != "" {
			msg = fmt.Sprintf("%s. %s", msg, s.recovery)
		}
		_, _ = io.WriteString(state, msg)
	case 'q':
		if s.recovery != "" {
			msg = fmt.Sprintf("%s. %s", msg, s.recovery)
		}

		_, _ = fmt.Fprintf(state, "%q", msg)
	}
}

type exitStatus struct {
	code        int
	typeName    string
	description string
	recovery    string
}

func newExitStatus(code int) *exitStatus {
	i, found := ErrorMetadata[code]
	if !found || len(i) != 3 {
		return &exitStatus{
			code:        UnknownError,
			typeName:    "UnknownError",
			description: "Unable to determine error cause",
		}
	}

	return &exitStatus{
		code:        code,
		typeName:    i[1],
		description: i[2],
	}
}

// ExitCode is the is the errors exit code
func (s *exitStatus) ExitCode() int {
	return s.code
}

// Description is a description of the error
func (s *exitStatus) Description() string {
	return s.description
}

// String returns the exit status type name
func (s *exitStatus) String() string {
	return fmt.Sprintf("%s: %s", s.typeName, s.description)
}

// Type returns the exit status type name
func (s *exitStatus) Type() string {
	return s.typeName
}

// Recovery returns the recovery instructions
func (s *exitStatus) Recovery() string {
	return s.recovery
}

// SetRecovery sets the recovery instructions
func (s *exitStatus) SetRecovery(rec string) {
	s.recovery = rec
}

// Find the oldest error in the causal chain that fullfills the exitCoder interface
func oldestExitCoder(err error) exitCoder {
	if err == nil {
		return nil
	}

	if !IsStatusError(err) {
		return nil
	}

	var oldest error
	for {
		_, ok := err.(exitCoder)
		if ok {
			oldest = err
		}

		cause, ok := err.(causer)
		if !ok {
			break
		}
		err = cause.Cause()
	}

	coder, ok := oldest.(exitCoder)
	if !ok {
		return nil
	}

	return coder
}
