package inspec

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	yaml "gopkg.in/yaml.v2"
)

// BackendCache used for configuring inspec exec command, passed in via config flag
var BackendCache bool

// ResultMessageLimit used for configuring inspec exec command, passed in via config flag
var ResultMessageLimit int

// TmpDir is used for setting the location of the /tmp dir to be used by inspec for caching
var TmpDir string

var FIREJAIL string

// The timeout used for tasks that just evaluate a profile but do not execute it.
const defaultTimeout = 2 * time.Minute

const binName = "inspec"
const shimBinName = "inspec_runner"
const startInspec = "===start inspec==="
const endInspec = "===end inspec==="

// Set to `true` to emit inspec configuration and environment
// variables via debug logs. Leave to false for release.
const logSensitiveData = false

func inspecShimEnv() map[string]string {
	return map[string]string{
		"HOME":         TmpDir,
		"TMPDIR":       TmpDir,
		"CHEF_LICENSE": "accept-no-persist",
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

func writeInputsToYmlFile(inputs map[string]string, filename string) error {
	content, err := yaml.Marshal(inputs)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(filename, content, 0644)
	if err != nil {
		return err
	}
	return nil
}

// Scan a target node with all specified profiles
func Scan(paths []string, target *TargetConfig, timeout time.Duration, env map[string]string, inputs map[string]string) ([]byte, []byte, *Error) {
	if err := isTimeoutSane(timeout, 12*time.Hour); err != nil {
		return nil, nil, NewInspecError(INVALID_PARAM, err.Error())
	}

	target.Reporter = make(map[string]Reporter)
	// Using "json-automate" as it provides better inherited profiles report
	target.Reporter["json-automate"] = Reporter{}
	target.BackendCache = BackendCache
	target.ResultIncludeBacktrace = false
	target.ResultMessageLimit = ResultMessageLimit

	env["HOME"] = os.Getenv("HOME")

	args := append([]string{binName, "exec"}, paths...)

	// write the inputs to a file to be passed to inspec during command execution
	if len(inputs) > 0 {
		filename := "/tmp/inputs.yml"
		err := writeInputsToYmlFile(inputs, filename)
		if err != nil {
			errString := fmt.Sprintf("unable to write inputs to file for scan job %s : %s", target.Hostname, err.Error())
			return nil, nil, NewInspecError(INVALID_PARAM, errString)
		}
		args = append(args, fmt.Sprintf("--input-file %s", filename))
	}

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
				stdOutErr += fmt.Sprintf("Expected InSpec STDOUT to end with '}' but found %q. Last 80 chars of STDOUT: `%s`\n", stdOut[len(stdOut)-1], stdOut[len(stdOut)-80:])
			} else {
				stdOutErr += fmt.Sprintf("Expected InSpec STDOUT to end with '}' but found %q. STDOUT: `%s`\n", stdOut[len(stdOut)-1], stdOut)
			}
		}
	}

	if stdOutErr != "" {
		scanErr := getInspecError("", string(stdErr), err, target, timeout)
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
		return nil, getInspecError(string(out), string(serr), err, target, timeout)
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

func sanitizeEnv(env map[string]string) map[string]string {
	outEnv := make(map[string]string, len(env))
	for k, v := range env {
		switch k {
		case "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN", "AZURE_CLIENT_SECRET", "AZURE_SUBSCRIPTION_ID":
			outEnv[k] = "REDACTED"
		default:
			outEnv[k] = v
		}
	}
	return outEnv
}

func run(args []string, conf *TargetConfig, timeout time.Duration, env map[string]string) ([]byte, []byte, error) {

	if https_proxy := os.Getenv("https_proxy"); https_proxy != "" {
		env["https_proxy"] = https_proxy
	}
	if http_proxy := os.Getenv("http_proxy"); http_proxy != "" {
		env["http_proxy"] = http_proxy
	}
	if no_proxy := os.Getenv("no_proxy"); no_proxy != "" {
		env["no_proxy"] = no_proxy
	}

	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	which, _ := exec.LookPath(args[0]) // nolint: errcheck
	logrus.WithFields(logrus.Fields{
		"path":  os.Getenv("PATH"),
		"args":  args,
		"env":   sanitizeEnv(env),
		"which": which,
	}).Info("Running inspec cli command")
	var cmd *exec.Cmd
	if conf == nil {
		cmd = exec.CommandContext(ctx, args[0], args[1:]...)
	} else {
		jsonConf, err := json.Marshal(conf)
		if err != nil {
			return nil, nil, err
		}

		args = append(args, "--config=-") // read config from stdin
		cmd = exec.CommandContext(ctx, args[0], args[1:]...)
		cmd.Stdin = bytes.NewBuffer(jsonConf)
		if logSensitiveData {
			logrus.Infof("Using inspec configuration: %s", string(jsonConf))
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

	logCtx.Info("Running Inspec")
	err := cmd.Run()
	return stdout.Bytes(), stderr.Bytes(), err
}

func Check(profilePath string, firejailprofilePath string) (CheckResult, error) {
	var res CheckResult
	tmpDirPath := fmt.Sprintf("/tmp/inspec-upload-%v", makeTimestamp())
	tmpDirFile, args, err := getFirejailArgsaAndOutputFile(false, firejailprofilePath, profilePath, tmpDirPath)
	if err != nil {
		return res, err
	}

	args = append(args, []string{binName, "check", tmpDirFile, "--format", "json"}...)

	logrus.Infof("Run: inspec %v", args)
	stdout, stderr, err := run(args, nil, defaultTimeout, inspecShimEnv())
	//Removing the file before checking the command
	os.RemoveAll(tmpDirPath)
	if err != nil {
		e := fmt.Sprintf("%s\n%s", err.Error(), stderr)
		return res, errors.New("Check InSpec check failed for " + profilePath + " with message: " + e)
	}

	logrus.Info("check command giving the output", string(stdout))

	jsonContent := findJsonLine([]byte(stdout))
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

	logrus.Infof("Successfully checked inspec profile in %s", profilePath)
	return res, nil
}

func Json(profilePath string, firejailprofilePath string) ([]byte, error) {
	var output string
	tmpDirPath := fmt.Sprintf("/tmp/inspec-upload-%v", makeTimestamp())

	tmpDirFile, args, err := getFirejailArgsaAndOutputFile(false, firejailprofilePath, profilePath, tmpDirPath)
	if err != nil {
		return nil, err
	}

	//echoStatement := fmt.Sprintf(";echo '%s'", endInspec)
	args = append(args, []string{binName, "json", tmpDirFile}...)
	logrus.Infof("Run: inspec %v", args)
	stdout, stderr, err := run(args, nil, defaultTimeout, inspecShimEnv())

	logrus.Infof("Run output from json: %s %s %v", stdout, stderr, err)

	logrus.Infof("Run: %s %s %v", output, stderr, err)
	if err != nil {
		e := fmt.Sprintf("%s\n%s", err.Error(), stderr)
		return nil, errors.New("Could not gather profile json for " + profilePath + " caused by: " + e)
	}
	os.RemoveAll(tmpDirPath)

	return []byte(output), nil
}

// Archives a directory to a TAR.GZ
func Archive(profilePath string, outputPath string, firejailprofilePath string) error {
	tmpDirPath := fmt.Sprintf("/tmp/inspec-upload-%v", makeTimestamp())
	tmpDirProfilePath, args, err := getFirejailArgsaAndOutputFile(true, firejailprofilePath, profilePath, tmpDirPath)
	if err != nil {
		return err
	}
	_, outputFileName := filepath.Split(outputPath)
	outputFilePath := tmpDirPath + "/" + outputFileName

	args = append(args, []string{binName, "archive", tmpDirProfilePath, "-o", outputFilePath, "--overwrite"}...)

	logrus.Infof("Run: inspec %v", args)
	_, stderr, err := run(args, nil, defaultTimeout, inspecShimEnv())

	if err != nil {
		e := fmt.Sprintf("%s\n%s", err.Error(), stderr)
		return errors.New("InSpec archive failed for " + tmpDirProfilePath + " with message: " + e)
	}

	err = fileutils.CopyFile(outputFilePath, outputPath)
	if err != nil {
		return errors.Wrapf(err, "Unable to copy archived file for output file", outputFileName)
	}
	err = os.RemoveAll(tmpDirPath)
	if err != nil {
		logrus.Errorf("Unable to delete tmp direcotory created %v", err)

	}
	logrus.Infof("Successfully archived %s to %s", profilePath, outputPath)
	return nil
}

func getInspecError(stdOut string, stdErr string, err error, target *TargetConfig, timeout time.Duration) *Error {
	for _, serr := range []string{stdOut, stdErr} {
		connErr := "Errno::ECONNREFUSED"
		if strings.Index(serr, connErr) >= 0 {
			return NewInspecError(CONN_REFUSED,
				"Failed to connect to "+target.Hostname+", connection refused."+"\n\n"+connErr)
		}
		noTTYErr := "Errno::ENOTTY"
		if strings.Index(serr, noTTYErr) >= 0 {
			return NewInspecError(AUTH_FAILED,
				"Authentication failed for "+target.Hostname+"\n\n"+noTTYErr)
		}
		hostDownErr := "Errno::EHOSTDOWN"
		if strings.Index(serr, hostDownErr) >= 0 {
			return NewInspecError(UNREACHABLE_HOST,
				"Failed to connect to "+target.Hostname+", host is unreachable."+"\n\n"+hostDownErr)
		}
		connTimeoutErr := "Net::SSH::ConnectionTimeout"
		if strings.Index(serr, connTimeoutErr) >= 0 {
			return NewInspecError(CONN_TIMEOUT,
				"Failed to connect to "+target.Hostname+", connection timeout."+"\n\n"+connTimeoutErr)
		}
		authErr := "Net::SSH::AuthenticationFailed"
		if strings.Index(serr, authErr) >= 0 {
			return NewInspecError(AUTH_FAILED,
				"Authentication failed for "+target.Hostname+"\n\n"+authErr)
		}
		winRmAuthErr := "WinRM::WinRMAuthorizationError"
		if strings.Index(serr, winRmAuthErr) >= 0 {
			return NewInspecError(AUTH_FAILED,
				"Authentication failed for "+target.Hostname+"\n\n"+winRmAuthErr)
		}
		sudoErr := "Sudo requires a password"
		if strings.Index(serr, sudoErr) >= 0 {
			return NewInspecError(SUDO_PW_REQUIRED,
				"Failed to run commands on "+target.Hostname+
					": The node is configured to use sudo, but sudo requires a password to run commands."+"\n\n"+sudoErr)
		}
		pwErr := "Wrong sudo password"
		if strings.Index(serr, pwErr) >= 0 {
			return NewInspecError(WRONG_SUDO_PW,
				"Failed to run commands on "+target.Hostname+
					": Sudo password is incorrect."+"\n\n"+pwErr)
		}
		nosudoErr := "Can't find sudo command"
		if strings.Index(serr, nosudoErr) >= 0 {
			return NewInspecError(NO_SUDO,
				"Failed to run commands on "+target.Hostname+
					": Cannot use sudo, please deactivate it or configure sudo for this user."+"\n\n"+nosudoErr)
		}
		notInSudoersErr := "is not in the sudoers file"
		if strings.Index(serr, notInSudoersErr) >= 0 {
			return NewInspecError(NO_SUDO,
				"Failed to run commands on "+target.Hostname+
					": User is not in sudoers file."+"\n\n"+notInSudoersErr)
		}
	}

	if err == nil {
		err = errors.New("unknown error")
	}

	if err.Error() == "execute timeout" {
		return NewInspecError(CONN_TIMEOUT,
			"Failed to connect to "+target.Hostname+
				", connection timed out after "+timeout.String())
	}

	errStr := "STDERR: " + stdErr
	if stdOut != "" {
		errStr = "\n\nSTDOUT: " + stdOut
	}
	return NewInspecError(UNKNOWN_ERROR,
		"Unknown inspec error for "+target.Hostname+": "+err.Error()+"\n\n"+errStr)
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

func getFirejailArgsaAndOutputFile(isArchive bool, firejailprofilePath string, profilePath string, tmpDirPath string) (string, []string, error) {

	firjailBin := os.Getenv("FIREJAIL")
	firejailFlag := "--quiet"
	firejailProfile := fmt.Sprintf("--profile=%s", firejailprofilePath)
	//echoStatement := fmt.Sprintf("echo '%s' ;", startInspec)

	firejailArgs := []string{firjailBin, firejailProfile, firejailFlag}

	fileName := filepath.Base(profilePath)
	fileCreated := tmpDirPath + "/" + fileName

	if isArchive {
		tempDirProfile := tmpDirPath + "/" + fileName
		err := prerequisiteForArchive(tempDirProfile, profilePath)
		if err != nil {
			logrus.Errorf("Unable to move files %v", err)
			return "", nil, nil
		}
		return tempDirProfile, firejailArgs, nil
	}

	err := prerequisiteForCommands(tmpDirPath, profilePath, fileName)
	if err != nil {
		logrus.Errorf("Unable to move files %v", err)
		return "", nil, nil
	}
	return fileCreated, firejailArgs, nil
}

func prerequisiteForArchive(tmpDir string, file string) error {
	err := os.MkdirAll(tmpDir, 0777)
	if err != nil {
		return errors.Wrapf(err, "Unable to make tmp directory")
	}

	err = fileutils.CopyDir(file, tmpDir, fileutils.Overwrite())
	if err != nil {
		return errors.Wrapf(err, "Unable to copy files in tmp directory")
	}
	return nil

}

func prerequisiteForCommands(tmpDir string, filepath string, fileName string) error {

	err := os.MkdirAll(tmpDir, 0777)
	if err != nil {
		return errors.Wrapf(err, "Unable to make tmp directory")
	}
	tmpDir = tmpDir + "/" + fileName
	err = fileutils.CopyFile(filepath, tmpDir, fileutils.Overwrite())
	if err != nil {
		return err
	}

	return nil
}

func makeTimestamp() int64 {
	return time.Now().UnixNano()
}

// func getTrimmedOutputFromFirejail(output []byte) string {
// 	var trimmedOutput string
// 	outputStr := string(output)
// 	startIndex := strings.Index(outputStr, startInspec)
// 	endIndex := strings.Index(outputStr, endInspec)
// 	if startIndex != -1 {
// 		trimmedOutput = outputStr[startIndex+len(startInspec)+1 : endIndex-1]

// 	}

// 	return trimmedOutput
// }
