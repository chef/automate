package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	mc "github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
	"github.com/chef/automate/lib/version"
	"github.com/hpcloud/tail"
)

var logFileSize int64 = 0

func executeAutomateClusterCtlCommand(command string, args []string, helpDocs string) error {
	if len(command) < 1 {
		return errors.New("Invalid or empty command")
	}
	writer.Printf("%s command execution started \n\n\n", command)
	args = append([]string{command}, args...)
	c := exec.Command("automate-cluster-ctl", args...)
	c.Dir = "/hab/a2_deploy_workspace"
	c.Stdin = os.Stdin
	var out bytes.Buffer
	var stderr bytes.Buffer
	c.Stdout = io.MultiWriter(os.Stdout, &out)
	c.Stderr = io.MultiWriter(os.Stderr, &stderr)
	err := c.Run()
	if err != nil {
		writer.Printf(stderr.String())
		return status.Wrap(err, status.CommandExecutionError, helpDocs)
	} else {
		writer.Printf("No error in executing commands")
	}
	outStr, errStr := string(out.Bytes()), string(stderr.Bytes())
	if len(outStr) > 0 {
		writer.Printf("\nout:\n%s", outStr)
	}
	if len(errStr) > 0 {
		writer.Printf("\nerr:\n%s\n", errStr)
	}
	writer.Printf("%s command execution done, exiting\n", command)
	return err
}

func executeAutomateClusterCtlCommandAsync(command string, args []string, helpDocs string) error {
	var logFilePath = filepath.Join(AUTOMATE_HA_RUN_LOG_DIR, "/a2ha-run.log")
	if len(command) < 1 {
		return errors.New("invalid or empty command")
	}
	if _, err := os.Stat(AUTOMATE_HA_RUN_LOG_DIR); !errors.Is(err, nil) {
		err = os.Mkdir(AUTOMATE_HA_RUN_LOG_DIR, os.ModeDir)
		if err != nil {
			panic(err)
		}
	}
	writer.Printf("%s command execution started \n\n\n", command)
	args = append([]string{command}, args...)
	c := exec.Command("automate-cluster-ctl", args...)
	c.Dir = AUTOMATE_HA_WORKSPACE_DIR
	c.Stdin = os.Stdin
	var out bytes.Buffer
	var stderr bytes.Buffer
	outfile, err := os.Create(logFilePath)
	if err != nil {
		panic(err)
	}
	defer outfile.Close()
	c.Stdout = outfile
	c.Stderr = outfile
	err = c.Start()
	if err != nil {
		writer.Printf(stderr.String())
		return status.Wrap(err, status.CommandExecutionError, helpDocs)
	}
	outStr, errStr := string(out.Bytes()), string(stderr.Bytes())
	if len(outStr) > 0 {
		writer.Printf("\nout:\n%s", outStr)
	}
	if len(errStr) > 0 {
		writer.Printf("\nerr:\n%s\n", errStr)
	}
	writer.Printf("%s command execution inprogress with process id : %d, + \n storing log in %s \n", command, c.Process.Pid, logFilePath)
	isExited := checkIfProcessExited(c.Process)
	if isExited {
		writer.Println("Exited")
	} else {
		writer.Println("Not Exited")
	}
	if !isExited {
		go showSpinnerForLongWait(logFilePath, c.Process)
	}
	tailFile(logFilePath, c.Process)
	return err
}

func checkIfProcessExited(process *os.Process) bool {
	err := process.Signal(syscall.Signal(0))
	if err == nil {
		return false
	}
	if err != nil && err.Error() == "os: process already finished" {
		return true
	}
	errno, ok := err.(syscall.Errno)
	if !ok {
		fmt.Println("Exited")
		return true
	}
	switch errno {
	case syscall.ESRCH:
		fmt.Println("Exit ESRCH")
		return true
	case syscall.EPERM:
		fmt.Println("NOT Exit EPERM")
		return false
	}
	return true
}

func killTailProcess(t *tail.Tail, secondsToStop int) {
	writer.Print("Tail exit command triggered")
	time.Sleep(time.Duration(secondsToStop) * time.Millisecond)
	writer.Println("Process Exited!")
	t.Stop()
}
func tailFile(logFilePath string, process *os.Process) {
	var exitFlag bool = false
	time.Sleep(1 * time.Second)
	t, err := tail.TailFile(logFilePath, tail.Config{Follow: true, MustExist: true})
	if err != nil {
		writer.Printf(err.Error())
		return
	}
	if checkIfProcessExited(process) {
		go killTailProcess(t, 1000)
		exitFlag = true
	} else {
		writer.Println("Not killed yet")
	}
	/* for {
		//fmt.Println(state.Exited())
		go killTailProcess(t, 120)
		line, open := <-t.Lines
		if !open {
			break
		}
		writer.Println(line.Text)
	} */
	for line := range t.Lines {
		if checkIfProcessExited(process) && !exitFlag {
			go killTailProcess(t, 1000)
			exitFlag = true
		}
		writer.Println(logMessageFilter(line.Text))
	}
}

func showSpinnerForLongWait(logFilePath string, process *os.Process) {
	for {
		fmt.Println("is exited", checkIfProcessExited(process))
		time.Sleep(1 * time.Minute)
		if getFileSize(logFilePath) == logFileSize {
			writer.StartSpinner()
		} else {
			writer.StopSpinner()
		}
	}
}

func bootstrapEnv(dm deployManager) error {
	if !deployCmdFlags.acceptMLSA {
		agree, err := writer.Confirm(promptMLSA)
		if err != nil {
			return status.Wrap(err, status.InvalidCommandArgsError, errMLSA)
		}

		if !agree {
			return status.New(status.InvalidCommandArgsError, errMLSA)
		}
	}
	conf := new(dc.AutomateConfig)
	if err := mergeFlagOverrides(conf); err != nil {
		return status.Wrap(
			err,
			status.ConfigError,
			"Merging command flag overrides into Chef Automate config failed",
		)
	}
	manifestProvider := manifest.NewLocalHartManifestProvider(
		mc.NewDefaultClient(conf.Deployment.GetV1().GetSvc().GetManifestDirectory().GetValue()),
		conf.Deployment.GetV1().GetSvc().GetHartifactsPath().GetValue(),
		conf.Deployment.GetV1().GetSvc().GetOverrideOrigin().GetValue())
	offlineMode := deployCmdFlags.airgap != ""
	err := client.DeployHA(writer, conf, manifestProvider, version.BuildTime, offlineMode)
	if err != nil && !status.IsStatusError(err) {
		return status.Annotate(err, status.DeployError)
	}
	err = dm.generateConfig()
	if err != nil {
		return status.Annotate(err, status.DeployError)
	}
	return nil
}

func isA2HARBFileExist() bool {
	if checkIfFileExist(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "a2ha.rb")) {
		return true
	}
	return false
}

func checkIfFileExist(path string) bool {
	if _, err := os.Stat(path); errors.Is(err, nil) {
		return true
	}
	return false
}

func getFileSize(path string) int64 {
	if checkIfFileExist(path) {
		info, _ := os.Stat(path)
		return info.Size()
	}
	return 0
}

func executeSecretsInitCommand(secretsKeyFilePath string) error {
	if !checkIfFileExist(secretsKeyFilePath) {
		writer.Printf("doing secrets init  \n")
		return executeSecretsCommand([]string{"init"})
	}
	return nil
}

func executeShellCommand(command string, args []string) error {
	writer.Printf("%s command execution started \n\n\n", command)
	c := exec.Command(command, args...)
	c.Stdin = os.Stdin
	var out bytes.Buffer
	var stderr bytes.Buffer
	c.Stdout = io.MultiWriter(os.Stdout, &out)
	c.Stderr = io.MultiWriter(os.Stderr, &stderr)
	err := c.Run()
	if err != nil {
		writer.Printf(stderr.String())
		return status.Wrap(err, status.CommandExecutionError, "")
	} else {
		writer.Printf("No error in executing commands")
	}
	outStr, errStr := string(out.Bytes()), string(stderr.Bytes())
	if len(outStr) > 0 {
		writer.Printf("\nout:\n%s", outStr)
	}
	if len(errStr) > 0 {
		writer.Printf("\nerr:\n%s\n", errStr)
	}
	writer.Printf("%s command execution done, exiting\n", command)
	return err
}

func logMessageFilter(logLine string) string {
	writer.Println("filtering log messages")
	logMessageKeys, logMessageMap := getLogMessageGrammer()
	for _, k := range logMessageKeys {
		if strings.Contains(logLine, k) {
			logLine = strings.ReplaceAll(logLine, k, logMessageMap[k])
		}
	}
	return logLine
}
func getLogMessageGrammer() ([]string, map[string]string) {
	m := make(map[string]string)
	m["automate-cluster-ctl"] = "chef-automate"

	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, k)
	}
	return keys, m
}
