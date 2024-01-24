package inspec

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strings"
	"time"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	yaml "gopkg.in/yaml.v3"
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
const json_command = "json"
const check_command = "check"
const archive_command = "archive"
const exec_command = "exec"

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

func errorStringValues() []string {
	return []string{
		"Permission denied",
		"Could not resolve host",
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
func Scan(paths []string, target *TargetConfig, timeout time.Duration, env map[string]string, inputs map[string]string, fireJailExecProfilePath string) ([]byte, []byte, *Error) {
	if err := isTimeoutSane(timeout, 12*time.Hour); err != nil {
		return nil, nil, NewInspecError(INVALID_PARAM, err.Error())
	}

	target.Reporter = make(map[string]Reporter)
	// Using "json-automate" as it provides better inherited profiles report
	target.Reporter["json-automate"] = Reporter{}
	target.BackendCache = BackendCache
	target.ResultIncludeBacktrace = false
	target.ResultMessageLimit = ResultMessageLimit

	//env["HOME"] = os.Getenv("HOME")

	//args := append([]string{binName, "exec"}, paths...)

	// write the inputs to a file to be passed to inspec during command execution
	tmpDirPath := fmt.Sprintf("/tmp/inspec-upload-%v", makeTimestamp())
	args, err := getFirejailArgsaAndOutputFileForExec(fireJailExecProfilePath, paths, tmpDirPath)
	if err != nil {
		return nil, nil, NewInspecError(INVALID_PARAM, err.Error())
	}

	tmpKeys := copyKeyFilesIntoTmpDirectory(tmpDirPath, target.KeyFiles)
	newTarget := target
	newTarget.KeyFiles = tmpKeys
	stdoutFile, erroutFile, shellFile := shellscriptAndResponse(exec_command, tmpDirPath)
	commandArgs := args
	commandArgs = append(commandArgs, []string{"/bin/sh", shellFile}...)
	commandArgs = append(commandArgs, paths...)
	if len(inputs) > 0 {
		filename := path.Join(tmpDirPath, "inputs.yml")
		err := writeInputsToYmlFile(inputs, filename)
		if err != nil {
			errString := fmt.Sprintf("unable to write inputs to file for scan job %s : %s", target.Hostname, err.Error())
			return nil, nil, NewInspecError(UNKNOWN_ERROR, errString)
		}
		args = append(args, fmt.Sprintf("--input-file %s", filename))
	}
	//Changing the home directory to tmp directory created
	env["HOME"] = tmpDirPath

	logrus.Debugf("Run: inspec %v", args)
	_, _, err = run(commandArgs, newTarget, timeout, env)
	stdOut := readFile(stdoutFile)
	stdErr := readFile(erroutFile)
	os.RemoveAll(tmpDirPath)
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

func Check(profilePath string, firejailprofilePath string) (CheckResult, error) {
	var res CheckResult
	tmpDirPath := fmt.Sprintf("/tmp/inspec-upload-%v", makeTimestamp())
	tmpDirFile, args, err := getFirejailArgsaAndOutputFile(false, firejailprofilePath, profilePath, tmpDirPath)
	if err != nil {
		return res, err
	}

	stdoutFile, erroutFile, shellFile := shellscriptAndResponse(check_command, tmpDirPath)

	args = append(args, []string{"/bin/sh", shellFile, tmpDirFile, stdoutFile, erroutFile}...)

	logrus.Infof("Run: inspec %v", args)
	env := map[string]string{
		"HOME":         tmpDirPath,
		"TMPDIR":       tmpDirPath,
		"CHEF_LICENSE": "accept-no-persist",
	}
	_, _, err = run(args, nil, defaultTimeout, env)

	errorContent := readFile(erroutFile)

	successContent := readFile(stdoutFile)

	os.RemoveAll(tmpDirPath)
	if err != nil {
		return res, errors.New("Check InSpec check failed for " + profilePath + " with message: " + err.Error() + string(errorContent))
	}

	if checkForError(errorContent, successContent) {
		return res, errors.New("InSpec check failed for " + profilePath + " with message: " + string(errorContent))
	}

	jsonContent := findJsonLine([]byte(successContent))
	err = json.Unmarshal(jsonContent, &res)
	if err != nil {
		return res, fmt.Errorf("Failed to unmarshal json:\n%s\nWith message: %s\nstdout: %s\nstderr: %s", jsonContent, err.Error(), successContent, errorContent)
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

	tmpDirPath := fmt.Sprintf("/tmp/inspec-upload-%v", makeTimestamp())

	tmpDirFile, args, err := getFirejailArgsaAndOutputFile(false, firejailprofilePath, profilePath, tmpDirPath)
	if err != nil {
		return nil, err
	}

	stdoutFile, erroutFile, shellFile := shellscriptAndResponse(json_command, tmpDirPath)
	args = append(args, []string{"/bin/sh", shellFile, tmpDirFile, stdoutFile, erroutFile}...)
	//Changing the home directory to tmp directory created
	env := map[string]string{
		"HOME":         tmpDirPath,
		"TMPDIR":       tmpDirPath,
		"CHEF_LICENSE": "accept-no-persist",
	}
	_, _, err = run(args, nil, defaultTimeout, env)
	errorContent := readFile(erroutFile)

	successContent := readFile(stdoutFile)

	os.RemoveAll(tmpDirPath)
	if err != nil {
		e := fmt.Sprintf("%s\n%s", err.Error(), errorContent)
		return nil, errors.New("Could not gather profile json for " + profilePath + " caused by: " + e)
	}

	if checkForError(errorContent, successContent) {
		return nil, errors.New("InSpec json failed for " + profilePath + " with message: " + string(errorContent) + "/n" + string(successContent))
	}

	return []byte(successContent), nil
}

// Archives a directory to a TAR.GZ
// 1. Creates a tmp directory
// 2. Copy the uploaded file tmp directory
// 3. Create firejail command sand box env
// 4. Create script to add archive command
func Archive(profilePath string, outputPath string, firejailprofilePath string) error {
	//Creating a tmp directory for intermittent profile
	tmpDirPath := fmt.Sprintf("/tmp/inspec-upload-%v", makeTimestamp())

	tmpDirProfilePath, args, err := getFirejailArgsaAndOutputFile(true, firejailprofilePath, profilePath, tmpDirPath)
	if err != nil {
		return err
	}

	_, outputFileName := filepath.Split(outputPath)
	outputFilePath := tmpDirPath + "/" + outputFileName

	stdoutFile, erroutFile, shellFile := shellscriptAndResponse(archive_command, tmpDirPath)

	//args = append(args, []string{binName, "archive", tmpDirProfilePath, "-o", outputFilePath, "--overwrite"}...)
	args = append(args, []string{"/bin/sh", shellFile, tmpDirProfilePath, outputFilePath, stdoutFile, erroutFile}...)

	env := map[string]string{
		"HOME":         tmpDirPath,
		"TMPDIR":       tmpDirPath,
		"CHEF_LICENSE": "accept-no-persist",
	}

	logrus.Debugf("Run: inspec %v", args)
	_, _, err = run(args, nil, defaultTimeout, env)

	errorContent := readFile(erroutFile)
	successContent := readFile(stdoutFile)

	if err != nil {
		e := fmt.Sprintf("%s\n%s", err.Error(), errorContent)
		os.RemoveAll(tmpDirPath)
		return errors.New("InSpec archive failed for " + tmpDirProfilePath + " with message: " + e)
	}
	if checkForError(errorContent, successContent) {
		os.RemoveAll(tmpDirPath)
		return errors.New("InSpec archive failed for " + tmpDirProfilePath + " with message: " + string(errorContent) + "/n" + string(successContent))
	}

	err = fileutils.CopyFile(outputFilePath, outputPath)
	if err != nil {
		return errors.Wrapf(err, "Unable to copy archived file for output file %s", outputFileName)
	}
	os.RemoveAll(tmpDirPath)
	logrus.Debugf("Successfully archived %s to %s", profilePath, outputPath)
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

func getFirejailArgsaAndOutputFileForExec(firejailprofilePath string, profilePaths []string, tmpDirPath string) ([]string, error) {

	err := os.MkdirAll(tmpDirPath, 0777)
	if err != nil {
		return nil, errors.Wrapf(err, "Unable to make tmp directory")
	}
	firjailBin := os.Getenv("FIREJAIL")
	firejailFlag := "--quiet"
	firejailProfile := fmt.Sprintf("--profile=%s", firejailprofilePath)

	firejailArgs := []string{firjailBin, firejailProfile, firejailFlag}

	return firejailArgs, nil
}

func prerequisiteForExec(tmpDir string, profilePath string) error {
	err := os.MkdirAll(tmpDir, 0777)
	if err != nil {
		return errors.Wrapf(err, "Unable to make tmp directory")
	}

	err = fileutils.CopyDir(profilePath, tmpDir, fileutils.Overwrite())
	if err != nil {
		return errors.Wrapf(err, "Unable to copy files in tmp directory")
	}

	return nil
}

func getFirejailArgsaAndOutputFile(isArchive bool, firejailprofilePath string, profilePath string, tmpDirPath string) (string, []string, error) {

	firjailBin := os.Getenv("FIREJAIL")
	firejailFlag := "--quiet"
	firejailProfile := fmt.Sprintf("--profile=%s", firejailprofilePath)

	firejailArgs := []string{firjailBin, firejailProfile, firejailFlag}

	fileName := filepath.Base(profilePath)
	fileCreated := path.Join(tmpDirPath, fileName)

	if isArchive {
		tempDirProfile := tmpDirPath + "/" + fileName
		err := prerequisiteForArchive(tempDirProfile, profilePath)
		if err != nil {
			logrus.Errorf("Unable to move files %v", err)
			return "", nil, err
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

func shellscriptAndResponse(command string, tmpDirPath string) (string, string, string) {

	stdoutFile := tmpDirPath + "/success_json"
	erroutFile := tmpDirPath + "/error_json"
	shellFile := fmt.Sprintf("%s/%s_script.sh", tmpDirPath, command)
	contentForShellFile := createShellFileContent(command, stdoutFile, erroutFile)
	err := createFileAndAddContent(shellFile, contentForShellFile)
	if err != nil {
		logrus.Errorf("Unable to create shell script for path %s with error %v", shellFile, err)
	}

	return stdoutFile, erroutFile, shellFile

}

func createShellFileContent(command string, stdout string, stderr string) string {
	if command == json_command {
		return fmt.Sprintf(`inspec %s $1 >$2 2>$3`, command)
	}
	if command == check_command {
		return `inspec check $1 --format json>$2 2>$3`
	}
	if command == archive_command {
		return `inspec archive $1 -o $2 --overwrite >$3 2>$4`
	}
	if command == exec_command {
		return fmt.Sprintf(`inspec exec $@ > %s 2>%s`, stdout, stderr)
	}

	return ""
}

func createFileAndAddContent(fileName string, content string) error {
	f, err := os.OpenFile(fileName, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		return err
	}
	if _, err := f.Write([]byte(content)); err != nil {
		return err
	}
	if err := f.Close(); err != nil {
		return err
	}

	return nil
}

func readFile(fileName string) []byte {
	dat, err := os.ReadFile(fileName)
	if err != nil {
		logrus.Errorf("Unable to read the contents of the file %v", err)
		return nil
	}

	return dat

}

func copyKeyFilesIntoTmpDirectory(tmpDirPath string, keyfiles []string) []string {
	var outputkeys []string
	for _, keyFile := range keyfiles {
		key := filepath.Base(keyFile)
		outputKey := fmt.Sprintf("%s/%s", tmpDirPath, key)
		fileutils.CopyFile(keyFile, outputKey, fileutils.Overwrite())
		outputkeys = append(outputkeys, outputKey)

	}

	return outputkeys
}

func checkForError(stdErr []byte, stdOut []byte) bool {

	if stdErr != nil && isErrorInOutput(stdErr, errorStringValues()) {
		return true
	}

	if stdOut != nil && isErrorInOutput(stdOut, errorStringValues()) {
		return true
	}

	return false

}

func isErrorInOutput(fileContent []byte, value []string) bool {

	for _, val := range value {
		if strings.Contains(string(fileContent), val) {
			return true
		}
	}

	return false

}
