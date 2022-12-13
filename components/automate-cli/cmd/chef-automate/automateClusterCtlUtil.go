package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"regexp"
	"strings"
	"text/template"
	"time"

	dc "github.com/chef/automate/api/config/deployment"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	mc "github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/parser"
	"github.com/chef/automate/lib/version"
	"github.com/hpcloud/tail"
	"github.com/sirupsen/logrus"
)

func executeAutomateClusterCtlCommand(command string, args []string, helpDocs string) error {
	if len(command) < 1 {
		return errors.New("Invalid or empty command")
	}
	//writer.Printf("%s command execution started \n\n\n", command)
	writer.StartSpinner()
	args = append([]string{command}, args...)
	c := exec.Command("automate-cluster-ctl", args...)
	c.Dir = "/hab/a2_deploy_workspace"
	c.Stdin = os.Stdin
	var out bytes.Buffer
	var stderr bytes.Buffer
	c.Stdout = io.MultiWriter(&out)
	c.Stderr = io.MultiWriter(&stderr)
	err := c.Run()
	if err != nil {
		writer.Printf(stderr.String())
		return status.Wrap(err, status.CommandExecutionError, helpDocs)
	}
	outStr, errStr := string(out.Bytes()), string(stderr.Bytes())
	if len(outStr) > 0 {
		writer.Printf("\n%s\n", outStr)
	}
	if len(errStr) > 0 {
		writer.Printf("\n%s\n", errStr)
	}
	//writer.Printf("%s command execution done, exiting\n", command)
	writer.StopSpinner()
	return err
}

func executeAutomateClusterCtlCommandAsync(command string, args []string, helpDocs string) error {
	var logFilePath = filepath.Join(AUTOMATE_HA_RUN_LOG_DIR, "/a2ha-run.log")
	if len(command) < 1 {
		return errors.New("Invalid or empty command")
	}
	if _, err := os.Stat(AUTOMATE_HA_RUN_LOG_DIR); !errors.Is(err, nil) {
		err = os.Mkdir(AUTOMATE_HA_RUN_LOG_DIR, os.ModeDir)
		if err != nil {
			panic(err)
		}
	}
	//writer.Printf("%s command execution started \n\n\n", command)
	args = append([]string{command}, args...)
	c := exec.Command("automate-cluster-ctl", args...)
	c.Dir = AUTOMATE_HA_WORKSPACE_DIR
	c.Stdin = os.Stdin
	outfile, err := os.Create(logFilePath)
	if err != nil {
		panic(err)
	}
	defer outfile.Close()
	c.Stdout = outfile
	c.Stderr = outfile
	err = c.Start()
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, helpDocs)
	}
	writer.Printf("%s command execution inprogress with process id : %d, + \n storing log in %s \n", command, c.Process.Pid, logFilePath)
	executed := make(chan struct{})
	go tailFile(logFilePath, executed)
	_, err = c.Process.Wait()
	if err != nil {
		return err
	}
	time.Sleep(5 * time.Second)
	close(executed)
	return err
}

func tailFile(logFilePath string, executed chan struct{}) {
	time.Sleep(1 * time.Second)
	t, err := tail.TailFile(logFilePath, tail.Config{Follow: true, MustExist: true})
	if err != nil {
		writer.Printf(err.Error())
		return
	}
	var spinning bool = false
	for {
		select {
		case <-executed:
			writer.Println("Exiting as execution process completed")
			_ = t.Stop()
			return
		case line := <-t.Lines:
			if spinning {
				writer.StopSpinner()
				spinning = false
			}
			writer.Println(line.Text)
		default:
			if !spinning {
				writer.StartSpinner()
				spinning = true
			}
		}

	}
}
func doBootstrapEnv(airgapBundlePath string, saas bool) error {
	conf := new(dc.AutomateConfig)
	if err := mergeFlagOverrides(conf); err != nil {
		return status.Wrap(
			err,
			status.ConfigError,
			"Merging command flag overrides into Chef Automate config failed",
		)
	}

	offlineMode := airgapBundlePath != ""
	manifestPath := ""
	var airgapMetadata airgap.UnpackMetadata
	if offlineMode {
		writer.Title("Installing artifact")
		metadata, err := airgap.Unpack(airgapBundlePath)
		if err != nil {
			return status.Annotate(err, status.AirgapUnpackInstallBundleError)
		}
		airgapMetadata = metadata
		manifestPath = api.AirgapManifestPath

		// We need to set the path for the hab binary so that the deployer does not
		// try to go to the internet to get it
		pathEnv := os.Getenv("PATH")

		err = os.Setenv("PATH", fmt.Sprintf("%s:%s", path.Dir(metadata.HabBinPath), pathEnv))
		if err != nil {
			return err
		}
	} else {
		manifestPath = conf.Deployment.GetV1().GetSvc().GetManifestDirectory().GetValue()
	}

	manifestProvider := manifest.NewLocalHartManifestProvider(
		mc.NewDefaultClient(manifestPath),
		conf.Deployment.GetV1().GetSvc().GetHartifactsPath().GetValue(),
		conf.Deployment.GetV1().GetSvc().GetOverrideOrigin().GetValue())

	err := client.DeployHA(writer, conf, manifestProvider, version.BuildTime, offlineMode, saas)
	if err != nil && !status.IsStatusError(err) {
		return status.Annotate(err, status.DeployError)
	}
	if offlineMode {
		err := moveFrontendBackendAirgapToTransferDir(airgapMetadata, airgapBundlePath)
		if err != nil {
			return status.Annotate(err, status.DeployError)
		}
	}
	return nil
}

func bootstrapEnv(dm deployManager, airgapBundlePath string, saas bool) error {
	if !deployCmdFlags.acceptMLSA {
		agree, err := writer.Confirm(promptMLSA)
		if err != nil {
			return status.Wrap(err, status.InvalidCommandArgsError, errMLSA)
		}

		if !agree {
			return status.New(status.InvalidCommandArgsError, errMLSA)
		}
	}
	err := doBootstrapEnv(airgapBundlePath, saas)
	if err != nil {
		return err
	}
	err = dm.generateConfig()
	if err != nil {
		return status.Annotate(err, status.DeployError)
	}
	return nil
}

/*
In airgap deployement of A2HA we need to have two AIB files in /hab/a2_deploy_workspace/terraform/transfer_files/
first airgap file will automate generated airgap bundle,
second file will be backend airgap bundle which is same as automate(frontend) airgap bundle but without header.
also we need to keep MD5 hash files both frontend and backend bundles.
we also need to have following files in /hab/a2_deploy_workspace/terraform dir
1. a2ha_aib_fe.auto.tfvars --> it will contain info of frontend airgap bundle
2. a2ha_aib_be.auto.tfvars --> it will contain info of backend airgap bundle
1. a2ha_manifest.auto.tfvars --> it will contain info of required packages to deploy a2ha.
*/

func getVersion(airgapBundle string) (string, error) {
	_, manifestBytes, err := airgap.GetMetadata(airgapBundle)
	if err != nil {
		return "", status.Annotate(err, status.AirgapUnpackInstallBundleError)
	}
	manifest, err := parser.ManifestFromBytes(manifestBytes)
	if err != nil {
		return "", status.Annotate(err, status.AirgapUnpackInstallBundleError)
	}
	return manifest.Build, nil
}

func getBytesFromTempalte(templateName string, templateContent string, placeholders map[string]interface{}) []byte {
	t := template.Must(template.New(templateName).Parse(templateContent))
	buf := &bytes.Buffer{}
	if err := t.Execute(buf, placeholders); err != nil {
		panic(err)
	}
	return buf.Bytes()
}
func getBldrSupportedPkgName(h string) string {
	packageName := extractPackageNameFromHartifactPath(h)
	ver, rel := extarctVersionAndRelease(packageName)
	origin := extractOrigin(packageName)
	pkg := extractPackageName(packageName)
	return origin + "/" + pkg + "/" + ver + "/" + rel
}

func extractPackageNameFromHartifactPath(path string) string {
	path = strings.ReplaceAll(path, "/hab/cache/artifacts/", "")
	return path
}

func copyFileContents(src, dst string) (err error) {
	in, err := os.Open(src)
	if err != nil {
		return
	}
	defer in.Close()
	out, err := os.Create(dst)
	if err != nil {
		return
	}
	defer func() {
		cerr := out.Close()
		if err == nil {
			err = cerr
		}
	}()
	if _, err = io.Copy(out, in); err != nil {
		return
	}
	err = out.Sync()
	return
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

func executeSecretsInitCommand(secretsKeyFilePath string) error {
	if !checkIfFileExist(secretsKeyFilePath) {
		writer.Printf("doing secrets init  \n")
		return executeSecretsCommand([]string{"init"})
	}
	return nil
}

func executeShellCommand(command string, args []string, workingDir string) error {
	writer.Printf("\n%s command execution started \n\n\n", command)
	c := exec.Command(command, args...)
	c.Stdin = os.Stdin
	if len(workingDir) > 0 {
		c.Dir = workingDir
	}
	c.Stdout = io.MultiWriter(os.Stdout)
	c.Stderr = io.MultiWriter(os.Stderr)
	err := c.Run()
	writer.Printf("%s command execution done, exiting\n", command)
	return err
}

func executeShellCommandAndGet(command string, args []string, workingDir string) error {
	writer.Printf("\n%s command execution started \n\n\n", command)
	c := exec.Command(command, args...)
	c.Stdin = os.Stdin
	if len(workingDir) > 0 {
		c.Dir = workingDir
	}
	c.Stdout = io.MultiWriter(os.Stdout)
	c.Stderr = io.MultiWriter(os.Stderr)
	err := c.Run()
	writer.Printf("%s command execution done, exiting\n", command)
	return err
}

func extarctVersionAndRelease(filename string) (string, string) {
	r := regexp.MustCompile(RELEASE_AND_VERSION_PATTERN)
	match := r.FindStringSubmatch(filename)
	if match == nil {
		logrus.Debugf("failed to parse version of hart %s", filename)
	}
	return match[1], match[2]
}

func extractOrigin(filename string) string {
	r := regexp.MustCompile(ORIGIN_PATTERN)
	match := r.FindStringSubmatch(filename)
	if match == nil {
		logrus.Debugf("failed to parse origin from of hart %s", filename)
	}
	return match[0]
}

func extractPackageName(filename string) string {
	r := regexp.MustCompile(PACKAGE_NAME_PATTERN)
	match := r.FindStringSubmatch(filename)
	if match == nil {
		logrus.Debugf("failed to parse package name of hart %s", filename)
	}
	return match[0][1 : len(match[0])-(len(match[0])-(strings.LastIndexAny(match[0], "-")))]
}

func grepFromFile(searchEle string, filepath string) (string, error) {
	command := "grep \"" + searchEle + "\" " + filepath + " | awk '{print $2}'"
	out, err := exec.Command("/bin/sh", "-c", command).Output()
	if err != nil {
		writer.Fail(err.Error())
		return "", nil
	}
	return strings.TrimSpace(string(out)), nil
}

func isManagedServicesOn() bool {
	isManagedService, err := grepFromFile("setup_managed_services", "/hab/a2_deploy_workspace/a2ha.rb")
	if err != nil {
		return false
	}
	if isManagedService != "" && isManagedService == "true" {
		return true
	}
	return false
}
