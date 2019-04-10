package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/golang/protobuf/proto"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
)

func init() {
	RootCmd.AddCommand(setupCmd())
	RootCmd.AddCommand(deployCmd())
}

func setupCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "setup-supervisor",
		Short: "Install hab-sup and setup systemd unit",
		Run:   runSetup,
	}
}

func deployCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "deploy-service",
		Short: "Configure and start deployment-service",
		Run:   runDeploy,
	}
}

// This is the same path that systemd will use for our service at startup
var standardSystemPath = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

func runSetup(cmd *cobra.Command, args []string) {
	writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
	target := target.NewLocalTarget(airgap.AirgapInUse())
	conf, manifest := getConfigAndManifest(args, writer)
	setupLogging()
	setupEnvironment()

	err := target.SetupSupervisor(conf, manifest, writer)
	if err != nil {
		writer.FailWrap(err, "setup failed")
		os.Exit(1)
	}
}

func runDeploy(cmd *cobra.Command, args []string) {
	writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
	conf, manifest := getConfigAndManifest(args, writer)
	setupLogging()
	setupEnvironment()

	target := target.NewLocalTarget(airgap.AirgapInUse())
	err := target.DeployDeploymentService(conf, manifest, writer)
	if err != nil {
		writer.FailWrap(err, "setup failed")
		os.Exit(1)
	}
}

func setupEnvironment() {
	path := os.Getenv("PATH")
	os.Setenv("PATH", fmt.Sprintf("%s:%s", path, standardSystemPath))
	logrus.Debugf("PATH=%s", os.Getenv("PATH"))
}

func setupLogging() {
	level := os.Getenv("CHEF_AUTOMATE_LOG_LEVEL")
	if level != "" {
		l, err := logrus.ParseLevel(level)
		if err != nil {
			logrus.Warnf("Unable to parse log level %s", level)
			return
		}
		logrus.SetLevel(l)
	}
}

func getConfigAndManifest(args []string, writer cli.FormatWriter) (*dc.ConfigRequest, manifest.ReleaseManifest) {
	configFile := args[0]
	configData, err := ioutil.ReadFile(configFile)
	if err != nil {
		writer.FailWrap(err, "reading configuration data")
		os.Exit(1)
	}

	conf := &dc.ConfigRequest{}
	err = proto.Unmarshal(configData, conf)
	if err != nil {
		writer.FailWrap(err, "parsing configuration data")
		os.Exit(1)
	}

	manifestFile := args[1]
	manifestData, err := ioutil.ReadFile(manifestFile)
	if err != nil {
		writer.FailWrap(err, "reading manifest data")
		os.Exit(1)
	}

	manifest := &manifest.A2{}
	err = json.Unmarshal(manifestData, manifest)
	if err != nil {
		writer.FailWrap(err, "parsing manifest data")
		os.Exit(1)
	}

	return conf, manifest
}
