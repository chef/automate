package inspec

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/sirupsen/logrus"
)

// BackendCache used for configuring inspec exec command, passed in via config flag
var BackendCache bool

// TmpDir is used for setting the location of the /tmp dir to be used by inspec for caching
var TmpDir string

// The timeout used for tasks that just evaluate a profile but do not execute it.
const defaultTimeout = 2 * time.Minute

const binName = "inspec"
const shimBinName = "inspec_runner"

// Set to `true` to emit inspec configuration and environment
// variables via debug logs. Leave to false for release.
const logSensitiveData = false

func inspecShimEnv() map[string]string {
	return map[string]string{
		"HOME":   TmpDir,
		"TMPDIR": TmpDir,
	}
}

func isTimeoutSane(timeout time.Duration, max time.Duration) error {
	if timeout < 1*time.Second {
		return errors.New("Timeout for InSpec CLI should never be less than 1 second. It is probably misconfigured.")
	}
	if timeout > max {
		return errors.New(fmt.Sprintf("Timeouts of more than %s should not be set for the inspec cli.", max))
	}
	return nil
}

// Scan a target node with all specified profiles
func Scan(paths []string, target *TargetConfig, timeout time.Duration, env map[string]string) ([]byte, []byte, *Error) {
	if err := isTimeoutSane(timeout, 12*time.Hour); err != nil {
		return nil, nil, NewInspecError(INVALID_PARAM, err.Error())
	}

	target.Reporter = make(map[string]Reporter)
	// Using "json-automate" as it provides better inherited profiles report
	target.Reporter["json-automate"] = Reporter{}
	target.BackendCache = BackendCache

	env["HOME"] = os.Getenv("HOME")

	args := append([]string{binName, "exec"}, paths...)
	stdOut, stdErr, err := run(args, target, timeout, env)

	stdOutErr := ""
	if len(stdOut) == 0 {
		stdOutErr = "Empty STDOUT, we have a problem..."
	} else {
		// ensuring that stdout starts with json character {
		if stdOut[0] != '{' {
			// If non-json text ends up in STDOUT, we are in trouble. Showing only the first 80
			// characters should be enough to capture that and avoid reports being dumped in the logs
			if len(stdOut) >= 80 {
				stdOutErr = fmt.Sprintf("Expected InSpec STDOUT to start with '{' but found %q. First 80 chars of STDOUT: %s\n", stdOut[0], stdOut[0:80])
			} else {
				stdOutErr = fmt.Sprintf("Expected InSpec STDOUT to start with '{' but found %q. STDOUT: %s\n", stdOut[0], stdOut)
			}
		}
		// ensuring that stdout ends with json character }
		if stdOut[len(stdOut)-1] != '}' {
			if len(stdOut) >= 80 {
				stdOutErr += fmt.Sprintf("Expected InSpec STDOUT to end with '}' but found %q. Last 80 chars of STDOUT: %s\n", stdOut[len(stdOut)-1], stdOut[len(stdOut)-80:])
			} else {
				stdOutErr += fmt.Sprintf("Expected InSpec STDOUT to end with '}' but found %q. STDOUT: %s\n", stdOut[len(stdOut)-1], stdOut)
			}
		}
	}

	if stdOutErr != "" {
		scanErr := getInspecError(string(stdErr), err, target, timeout)
		scanErr.Message = stdOutErr + "\n" + scanErr.Message
		return nil, nil, scanErr
	}

	return stdOut, stdErr, nil
}

// Detect retrieves a target's operating system and returns
// connection errors if there were any.
func Detect(target *TargetConfig, timeout time.Duration, env map[string]string) (*OSInfo, *Error) {
	if err := isTimeoutSane(timeout, 30*time.Minute); err != nil {
		return nil, NewInspecError(CONN_TIMEOUT, err.Error())
	}

	env["HOME"] = os.Getenv("HOME")

	args := []string{binName, "detect", "--format", "json"}
	out, serr, err := run(args, target, timeout, env)

	if err != nil {
		// all connection errors etc go here
		return nil, getInspecError(string(serr), err, target, timeout)
	}

	var res OSInfo
	err = json.Unmarshal(findJsonLine(out), &res)
	if err != nil {

		return nil, NewInspecError(RESPONSE_ERROR,
			"The target "+target.Hostname+" failed the detection commands.")
	}
	if res.OSName == "" {
		return nil, NewInspecError(RESPONSE_ERROR,
			"The target "+target.Hostname+" failed the detection commands.")
	}
	return &res, nil
}

func run(args []string, conf *TargetConfig, timeout time.Duration, env map[string]string) ([]byte, []byte, error) {
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	var cmd *exec.Cmd
	if conf == nil {
		cmd = exec.CommandContext(ctx, args[0], args[1:]...)
	} else {
		jsonConf, err := json.Marshal(conf)
		if err != nil {
			return nil, nil, err
		}

		args = append(args, "--json-config=-") // read config from stdin
		cmd = exec.CommandContext(ctx, args[0], args[1:]...)
		cmd.Stdin = bytes.NewBuffer(jsonConf)
		if logSensitiveData {
			logrus.Debugf("Using inspec configuration: %s", string(jsonConf))
		}
	}

	cmd.Env = []string{"PATH=" + os.Getenv("PATH")}
	if TmpDir != "" {
		if _, ok := env["TMPDIR"]; !ok {
			cmd.Env = append(cmd.Env, "TMPDIR="+TmpDir)
		}
	}

	for k, v := range env {
		cmd.Env = append(cmd.Env, k+"="+v)
	}

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	logCtx := logrus.WithFields(logrus.Fields{
		"exe":  args[0],
		"args": args[1:],
	})
	if logSensitiveData {
		logCtx = logCtx.WithField("env", cmd.Env)
	}

	logCtx.Debug("Running Inspec")
	err := cmd.Run()
	return stdout.Bytes(), stderr.Bytes(), err
}

func Check(profilePath string) (CheckResult, error) {
	var res CheckResult

	args := []string{shimBinName, "check", profilePath, "--format", "json"}
	logrus.Debugf("Run: inspec %v", args)
	stdout, stderr, err := run(args, nil, defaultTimeout, inspecShimEnv())

	if err != nil {
		e := fmt.Sprintf("%s\n%s", err.Error(), stderr)
		return res, errors.New("Check InSpec check failed for " + profilePath + " with message: " + e)
	}

	jsonContent := findJsonLine(stdout)
	err = json.Unmarshal(jsonContent, &res)
	if err != nil {
		return res, fmt.Errorf("Failed to unmarshal json:\n%s\nWith message: %s\nstdout: %s\nstderr: %s", jsonContent, err.Error(), stdout, stderr)
	}

	if len(res.Errors) > 0 {
		errs := make([]string, len(res.Errors))
		for i, err := range res.Errors {
			errs[i] = err.Msg
		}
		return res, errors.New(strings.Join(errs, "\n"))
	}

	logrus.Debugf("Successfully checked inspec profile in %s", profilePath)
	return res, nil
}

func Json(profilePath string) ([]byte, error) {
	args := []string{shimBinName, "json", profilePath}
	logrus.Debugf("Run: inspec %v", args)
	stdout, stderr, err := run(args, nil, defaultTimeout, inspecShimEnv())
	logrus.Debugf("Run: %s %s %v", stdout, stderr, err)
	if err != nil {
		e := fmt.Sprintf("%s\n%s", err.Error(), stderr)
		return nil, errors.New("Could not gather profile json for " + profilePath + " caused by: " + e)
	}
	return stdout, nil
}

// Archives a directory to a TAR.GZ
func Archive(profilePath string, outputPath string) error {
	args := []string{shimBinName, "archive", profilePath, "-o", outputPath, "--overwrite"}
	logrus.Debugf("Run: inspec %v", args)
	_, stderr, err := run(args, nil, defaultTimeout, inspecShimEnv())

	if err != nil {
		e := fmt.Sprintf("%s\n%s", err.Error(), stderr)
		return errors.New("InSpec archive failed for " + profilePath + " with message: " + e)
	}

	logrus.Debugf("Successfully archived %s to %s", profilePath, outputPath)
	return nil
}

func getInspecError(serr string, err error, target *TargetConfig, timeout time.Duration) *Error {
	connErr := "Errno::ECONNREFUSED"
	if strings.Index(serr, connErr) >= 0 {
		return NewInspecError(CONN_REFUSED,
			"Failed to connect to "+target.Hostname+", connection refused.")
	}
	noTTYErr := "Errno::ENOTTY"
	if strings.Index(serr, noTTYErr) >= 0 {
		return NewInspecError(AUTH_FAILED,
			"Authentication failed for "+target.Hostname)
	}
	hostDownErr := "Errno::EHOSTDOWN"
	if strings.Index(serr, hostDownErr) >= 0 {
		return NewInspecError(UNREACHABLE_HOST,
			"Failed to connect to "+target.Hostname+", host is unreachable.")
	}
	authErr := "Net::SSH::AuthenticationFailed"
	if strings.Index(serr, authErr) >= 0 {
		return NewInspecError(AUTH_FAILED,
			"Authentication failed for "+target.Hostname)
	}
	sudoErr := "Sudo requires a password"
	if strings.Index(serr, sudoErr) >= 0 {
		return NewInspecError(SUDO_PW_REQUIRED,
			"Failed to run commands on "+target.Hostname+
				": The node is configured to use sudo, but sudo requires a password to run commands.")
	}
	pwErr := "Wrong sudo password"
	if strings.Index(serr, pwErr) >= 0 {
		return NewInspecError(WRONG_SUDO_PW,
			"Failed to run commands on "+target.Hostname+
				": Sudo password is incorrect.")
	}
	nosudoErr := "Can't find sudo command"
	if strings.Index(serr, nosudoErr) >= 0 {
		return NewInspecError(NO_SUDO,
			"Failed to run commands on "+target.Hostname+
				": Cannot use sudo, please deactivate it or configure sudo for this user.")
	}
	notInSudoersErr := "is not in the sudoers file"
	if strings.Index(serr, notInSudoersErr) >= 0 {
		return NewInspecError(NO_SUDO,
			"Failed to run commands on "+target.Hostname+
				": User is not in sudoers file.")
	}
	if err.Error() == "execute timeout" {
		return NewInspecError(CONN_TIMEOUT,
			"Failed to connect to "+target.Hostname+
				", connection timed out after "+timeout.String())
	}

	return NewInspecError(UNKNOWN_ERROR,
		"Unknown inspec error for "+target.Hostname+": "+err.Error()+"\n\nSTDERR: "+serr)
}

func findJsonLine(in []byte) []byte {
	rawJson := ""
	for _, line := range strings.Split(string(in), "\n") {
		if strings.HasPrefix(line, "{") {
			rawJson = line
			break
		}
	}
	return []byte(rawJson)
}
