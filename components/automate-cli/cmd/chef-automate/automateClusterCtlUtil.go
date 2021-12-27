package main

import (
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
	// moving airgap to transfer files
	writer.Println("moving to transfer files")
	if len(deployCmdFlags.airgap) > 0 {
		var bundleName string = filepath.Base(deployCmdFlags.airgap)
		writer.Println(bundleName)
		if strings.Contains(bundleName, "automate") {
			writer.Println("found automate")
			bundleName = strings.ReplaceAll(bundleName, "automate", "frontend")
		}
		writer.Println("#$#$#$#$#$#$ Replaced automate ######")
		writer.Println(bundleName)

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
		ioutil.WriteFile(AUTOMATE_HA_TERRAFORM_DIR+"a2ha_aib_fe.auto.tfvars", []byte(frontendAutotfvarsTemplate), 0755)

		//generating backend bundle
		backendBundleFile := AIRGAP_HA_TRANS_DIR_PATH + (strings.ReplaceAll(bundleName, "frontend", "backend"))
		cargs := []string{"-c", "+8", bundleName, ">", "temp_tar_file_name", "&&", "cat", "temp_tar_file_name", ">", backendBundleFile}
		executeShellCommand("tail", cargs, AIRGAP_HA_TRANS_DIR_PATH)
		/* err = removeBytesFromFileAndWriteToFile(deployCmdFlags.airgap,
		backendBundleFile,
		8)
		if err != nil {
			return err
		} */

		err = generateChecksumFile(backendBundleFile, backendBundleFile+".md5")
		if err != nil {
			return err
		}
		//generating backend auto tfvars
		var backendAutotfvarsTemplate string = `
backend_aib_dest_file = "/var/tmp/` + filepath.Base(backendBundleFile) + `"
backend_aib_local_file = "` + filepath.Base(backendBundleFile) + `"
`
		ioutil.WriteFile(AUTOMATE_HA_TERRAFORM_DIR+"a2ha_aib_be.auto.tfvars", []byte(backendAutotfvarsTemplate), 0755)

		//generate manifest auto tfvars
		var manifestAutotfvarsTemplate string = `
pgleaderchk_pkg_ident = "            chef/automate-ha-pgleaderchk/0.1.0/20211122152431"
postgresql_pkg_ident = "            chef/automate-ha-postgresql/11.2/20211118151626"
proxy_pkg_ident = "            chef/automate-ha-haproxy/2.2.14/20211118151626"
journalbeat_pkg_ident = "            chef/automate-ha-journalbeat/6.8.6/20211118151544"
metricbeat_pkg_ident = "            chef/automate-ha-metricbeat/6.8.6/20211118151544"
kibana_pkg_ident = "            chef/automate-ha-kibana/6.8.6/20211118151625"
elasticsearch_pkg_ident = "            chef/automate-ha-elasticsearch/6.8.6/20211118151544"
elasticsidecar_pkg_ident = "            chef/automate-ha-elasticsidecar/0.1.0/20211118151618"
curator_pkg_ident = "            chef/automate-ha-curator/0.1.0/20211118151626"
		`

		ioutil.WriteFile(AUTOMATE_HA_TERRAFORM_DIR+"a2ha_manifest.auto.tfvars", []byte(manifestAutotfvarsTemplate), 0755)

	}
	return nil
	// #### moved airgap to transfer file.
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

func removeBytesFromFileAndWriteToFile(filePath string, destPath string, skipBytes int64) error {
	f, err := os.Open(filePath) // open file from argument
	if err != nil {
		return err
	}
	defer func() {
		if err := f.Close(); err != nil {
			writer.Print(err.Error())
			return
		}
	}()

	fo, err := os.Create(destPath)
	if err != nil {
		return err
	}
	// close fo on exit and check for its returned error
	defer func() {
		if err := fo.Close(); err != nil {
			writer.Print(err.Error())
			return
		}
	}()

	_, err = f.Seek(skipBytes, io.SeekStart) // skipping first bytes
	if err != nil {
		return err
	}

	buffer := make([]byte, 1024) // allocating buffer to read
	for {
		n, err := f.Read(buffer)
		if err != nil && err != io.EOF {
			panic(err)
		}
		if n == 0 {
			break
		}

		// write a chunk
		if _, err := fo.Write(buffer[:n]); err != nil {
			panic(err)
		}
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
