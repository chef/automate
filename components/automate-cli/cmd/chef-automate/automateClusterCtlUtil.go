package main

import (
	"bufio"
	"bytes"
	"crypto/md5"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"strings"
	"time"

	dc "github.com/chef/automate/api/config/deployment"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	mc "github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
	"github.com/chef/automate/lib/version"
	"github.com/hpcloud/tail"
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

	offlineMode := deployCmdFlags.airgap != ""
	manifestPath := ""
	if offlineMode {
		writer.Title("Installing artifact")
		metadata, err := airgap.Unpack(deployCmdFlags.airgap)
		if err != nil {
			return status.Annotate(err, status.AirgapUnpackInstallBundleError)
		}
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

	err := client.DeployHA(writer, conf, manifestProvider, version.BuildTime, offlineMode)
	if err != nil && !status.IsStatusError(err) {
		return status.Annotate(err, status.DeployError)
	}
	err = moveAirgapToTransferDir()
	if err != nil {
		return status.Annotate(err, status.DeployError)
	}
	err = dm.generateConfig()
	if err != nil {
		return status.Annotate(err, status.DeployError)
	}
	return nil
}

func moveAirgapToTransferDir() error {
	if len(deployCmdFlags.airgap) > 0 {
		var bundleName string = filepath.Base(deployCmdFlags.airgap)
		if strings.Contains(bundleName, "automate") {
			bundleName = strings.ReplaceAll(bundleName, "automate", "frontend")
		}
		err := copyFileContents(deployCmdFlags.airgap, (AIRGAP_HA_TRANS_DIR_PATH + bundleName))
		if err != nil {
			return err
		}
		//generating md5 sum for frontend bundle
		err = generateChecksumFile(AIRGAP_HA_TRANS_DIR_PATH+bundleName, AIRGAP_HA_TRANS_DIR_PATH+bundleName+".md5")
		if err != nil {
			return err
		}
		//generating frontend auto tfvars
		var frontendAutotfvarsTemplate string = `
frontend_aib_dest_file = "/var/tmp/` + bundleName + `"
frontend_aib_local_file = "` + bundleName + `"
		`
		err = ioutil.WriteFile(AUTOMATE_HA_TERRAFORM_DIR+"a2ha_aib_fe.auto.tfvars", []byte(frontendAutotfvarsTemplate), 0755)
		if err != nil {
			return err
		}
		//generating backend bundle
		backendBundleFile := AIRGAP_HA_TRANS_DIR_PATH + (strings.ReplaceAll(bundleName, "frontend", "backend"))
		err = generateBackendAIB(deployCmdFlags.airgap, backendBundleFile)
		if err != nil {
			return err
		}
		err = generateChecksumFile(backendBundleFile, backendBundleFile+".md5")
		if err != nil {
			return err
		}
		//generating backend auto tfvars
		var backendAutotfvarsTemplate string = `
backend_aib_dest_file = "/var/tmp/` + filepath.Base(backendBundleFile) + `"
backend_aib_local_file = "` + filepath.Base(backendBundleFile) + `"
`
		err = ioutil.WriteFile(AUTOMATE_HA_TERRAFORM_DIR+"a2ha_aib_be.auto.tfvars", []byte(backendAutotfvarsTemplate), 0755)
		if err != nil {
			return err
		}
		//generate manifest auto tfvars
		ioutil.WriteFile(AUTOMATE_HA_TERRAFORM_DIR+"a2ha_manifest.auto.tfvars", []byte(manifestTfVars), 0755)
		if err != nil {
			return err
		}
	}
	return nil
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

func generateBackendAIB(filePath string, destPath string) error {
	installBundleFile, err := os.Open(filePath)
	if err != nil {
		return errors.New(err.Error() + " Failed to open install bundle file " + filePath)
	}
	defer installBundleFile.Close() // nolint: errcheck

	bufReader := bufio.NewReader(installBundleFile)
	// Read the header:
	// AIB-1\n\n
	s, err := bufReader.ReadString('\n')
	if err != nil {
		return errors.New(err.Error() + " Could not read artifact file")
	}
	if s != "AIB-1\n" {
		return errors.New("Malformed install bundle file")
	}

	s, err = bufReader.ReadString('\n')
	if err != nil {
		return err
	}

	if s != "\n" {
		return errors.New("Malformed install bundle file")
	}

	fo, err := os.Create(destPath)
	if err != nil {
		panic(err)
	}
	defer func() {
		if err := fo.Close(); err != nil {
			panic(err)
		}
	}()

	bufWriter := bufio.NewWriter(fo)
	buf := make([]byte, 1024)
	for {
		n, err := bufReader.Read(buf)
		if err != nil && err != io.EOF {
			panic(err)
		}
		if n == 0 {
			break
		}
		if _, err := bufWriter.Write(buf[:n]); err != nil {
			panic(err)
		}
	}

	if err = bufWriter.Flush(); err != nil {
		panic(err)
	}

	return nil
}

func generateChecksumFile(sourceFileName string, checksumFileName string) error {
	sfile, err := os.Open(sourceFileName)
	if err != nil {
		return err
	}
	defer sfile.Close()
	hash := md5.New()
	if _, err := io.Copy(hash, sfile); err != nil {
		return err
	}
	sum := hash.Sum(nil)
	hashedFileContent := hex.EncodeToString(sum) + "  " + filepath.Base(sfile.Name())
	err = ioutil.WriteFile(checksumFileName, []byte(hashedFileContent), 0644)
	if err != nil {
		return err
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

func executeSecretsInitCommand(secretsKeyFilePath string) error {
	if !checkIfFileExist(secretsKeyFilePath) {
		writer.Printf("doing secrets init  \n")
		return executeSecretsCommand([]string{"init"})
	}
	return nil
}

func executeShellCommand(command string, args []string, workingDir string) error {
	writer.Printf("%s command execution started \n\n\n", command)
	c := exec.Command(command, args...)
	c.Stdin = os.Stdin
	var out bytes.Buffer
	var stderr bytes.Buffer
	if len(workingDir) > 0 {
		c.Dir = workingDir
	}
	c.Stdout = io.MultiWriter(&out)
	c.Stderr = io.MultiWriter(&stderr)
	err := c.Run()
	if err != nil {
		writer.Printf(stderr.String())
		return status.Wrap(err, status.CommandExecutionError, "")
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
