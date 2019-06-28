// Copyright © 2018 Chef Software

package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptrace"
	"os"
	"reflect"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	dc "github.com/chef/automate/api/config/deployment"
	w "github.com/chef/automate/api/config/shared/wrappers"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/adminmgmt"
	"github.com/chef/automate/components/automate-cli/pkg/client/apiclient"
	"github.com/chef/automate/components/automate-cli/pkg/dev/hab"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"
	"github.com/chef/automate/components/automate-deployment/pkg/bind"
	"github.com/chef/automate/components/automate-deployment/pkg/bootstrap"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/constants"
	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	mc "github.com/chef/automate/components/automate-deployment/pkg/manifest/client"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/components/automate-grpc/protoc-gen-a2-config/api/a2conf"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/platform/pg"
	"github.com/chef/automate/lib/stringutils"
)

// Since cobra's pattern for handling flag variables favors globals,
// we encapsulate flags used by this subcommand in a struct to avoid
// conflicts with other flags from other subcommands living in package
// main.
var devCmdFlags = struct {
	configFilePath   string
	docsPath         string
	docsFormat       string
	hartifactsPath   string
	overrideOrigin   string
	withDeps         bool
	manifestDir      string
	enableChefServer bool
	enableWorkflow   bool

	// service-deps option
	reverse bool
}{}

func init() {
	devCmd.AddCommand(configureDeploymentCmd())
	devCmd.AddCommand(newBootstrapCmd())
	devCmd.AddCommand(doDeployCmd)
	devCmd.AddCommand(genDocsCmd())
	devCmd.AddCommand(newDeploySomeCmd())
	devCmd.AddCommand(removeSomeCmd)
	devCmd.AddCommand(startConvergeCmd)
	devCmd.AddCommand(stopConvergeCmd)
	devCmd.AddCommand(newDumpA1PGCmd())
	devCmd.AddCommand(newExtractA1UserData())
	devCmd.AddCommand(newRunA1BackupCmd())
	devCmd.AddCommand(newReindexEs2Cmd())
	devCmd.AddCommand(psqlCmd)
	devCmd.AddCommand(newDepsCmd())
	devCmd.AddCommand(defaultCfg)
	devCmd.AddCommand(grpCurlCmd)
	devCmd.AddCommand(newGetURLCmd())
	devCmd.AddCommand(newDevPortCmd())
	devCmd.AddCommand(newVerifyPackagesCmd())
	devCmd.AddCommand(newEnablePrometheusCmd())
	devCmd.AddCommand(newDisablePrometheusCmd())
	devCmd.AddCommand(newCreateIAMDevUsersCmd())
	RootCmd.AddCommand(devCmd)
}

var devCmd = &cobra.Command{
	Use:    "dev COMMAND",
	Short:  "Utilities for Chef Automate development",
	Hidden: true,
}

func newBootstrapCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "bootstrap",
		Short: "Install hab and the deployment service on localhost.",
		RunE:  runBootstrapCmd,
	}

	cmd.Flags().StringVarP(&devCmdFlags.configFilePath,
		"config", "c", "config.toml", "Config file path")
	cmd.Flags().StringVar(&devCmdFlags.hartifactsPath,
		"hartifacts", "results", "Path containing locally built hart packages to use instead of depot packages")
	cmd.Flags().StringVar(&devCmdFlags.overrideOrigin,
		"override-origin", os.Getenv("HAB_ORIGIN"), "Origin from which to install local hart packages")
	cmd.Flags().StringVar(&devCmdFlags.manifestDir,
		"manifest-dir", "", "Path for local manifest files")

	return cmd
}

func runBootstrapCmd(cmd *cobra.Command, args []string) error {
	conf, err := getDevDeploymentConfig()
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Loading dev configuration failed")
	}
	tgt := target.NewLocalTarget(false)

	writer.Println("Fetching release manifest")

	manifestProvider := manifest.NewLocalHartManifestProvider(
		mc.NewDefaultClient(devCmdFlags.manifestDir),
		devCmdFlags.hartifactsPath,
		devCmdFlags.overrideOrigin)
	m, err := manifestProvider.GetCurrentManifest(context.Background(), "dev")
	if err != nil {
		return status.Wrap(err, status.DeployError, "Fetching the manifest failed")
	}

	mergedCfg, err := dc.MergeWithDefaults(conf)
	if err != nil {
		return status.Wrap(err, status.DeployError, "Merging config with defaults failed")
	}
	mergedCfg.SetGlobalConfig()
	client.SetProxyEnvironment(conf.Deployment)

	b := bootstrap.NewCompatBootstrapper(tgt)
	err = bootstrap.FullBootstrap(context.TODO(), b, m, mergedCfg.GetDeployment(), "", writer)
	if err != nil {
		return status.Annotate(err, status.DeployError)
	}

	return nil
}

func configureDeploymentCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:  "configure-deployment",
		RunE: runConfigureDeploymentCmd,
	}
	cmd.Flags().StringVarP(&devCmdFlags.configFilePath,
		"config", "c", "config.toml", "Config file path")
	cmd.Flags().StringVar(&devCmdFlags.hartifactsPath,
		"hartifacts", "results", "Path containing locally built hart packages to use instead of depot packages")
	cmd.Flags().StringVar(&devCmdFlags.overrideOrigin,
		"override-origin", os.Getenv("HAB_ORIGIN"), "Origin from which to install local hart packages")

	cmd.Flags().StringVar(&devCmdFlags.manifestDir,
		"manifest-dir", "", "Directory containing A2 manifest files")

	return cmd
}

func runConfigureDeploymentCmd(cmd *cobra.Command, args []string) error {
	conf, err := getDevDeploymentConfig()
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Loading dev configuration failed")
	}

	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	ctx := context.Background()
	dr := &api.ConfigureDeploymentRequest{Config: conf}
	resp, err := connection.ConfigureDeployment(ctx, dr)
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to configure the deployment failed",
		)
	}

	writer.Title("Configured deployment " + resp.DeploymentId.GetId())
	return nil
}

func getDevDeploymentConfig() (*dc.AutomateConfig, error) {
	return dc.LoadUserOverrideConfigFile(
		devCmdFlags.configFilePath,
		dc.WithManifestDir(devCmdFlags.manifestDir),
		dc.WithHartifacts(devCmdFlags.hartifactsPath),
		dc.WithOrigin(devCmdFlags.overrideOrigin),
	)
}

var doDeployCmd = &cobra.Command{
	Use:   "deployinate",
	RunE:  runDoDeployCmd,
	Short: "Deploys all services",
}

func runDoDeployCmd(cmd *cobra.Command, args []string) error {
	//// DoDeploy runs the core deployment and skips the bootstrap steps
	//func (cliApp *App) DoDeploy() error {
	writer.Title("Starting execution of deployment")
	connection, err := client.Connection(client.DefaultDeployTimeout)
	if err != nil {
		return err
	}

	ctx := context.Background()
	resp, err := connection.Deploy(ctx, &api.DeployRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeployError,
			"Request to deploy Chef Automate failed",
		)
	}
	deploymentID := &api.DeploymentID{}
	taskID := resp.TaskId
	h := &client.CLIEventWriter{Writer: writer}
	err = connection.StreamDeployEvents(taskID, deploymentID, h)
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to stream deployment events failed",
		)
	}

	return nil
}

func newDeploySomeCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "deploy-some <svc1> ...",
		RunE:  runDeploySomeCmd,
		Short: "Deploys the specified services.",
		Long:  "Deploys the specified services. Specify services as origin/name.",
	}
	cmd.Flags().BoolVar(&devCmdFlags.withDeps,
		"with-deps", false, "Whether to deploy required dependencies as well")
	return cmd
}

func runDeploySomeCmd(cmd *cobra.Command, args []string) error {
	var servicesToDeploy []string

	if devCmdFlags.withDeps {
		serviceIDs, err := services.ServicesInCollection("automate-full")
		if err != nil {
			return status.Wrap(err, status.DeployError, "Failed to load list of services")
		}

		sortedDeps, err := bind.TopoSort(args, serviceIDs, services.AllBinds)
		if err != nil {
			return status.Wrap(err, status.DeployError, "Dependency resolution for given services failed")
		}

		servicesToDeploy = make([]string, len(sortedDeps))
		for i, s := range sortedDeps {
			servicesToDeploy[i] = habpkg.Ident(&s)
		}
	} else {
		servicesToDeploy = args
	}
	msg := fmt.Sprintf("Starting deployment of %d services", len(servicesToDeploy))
	writer.Title(msg)
	connection, err := client.Connection(client.DefaultDeployTimeout)
	if err != nil {
		return err
	}

	ctx := context.Background()
	req := &api.DeployRequest{
		Services: servicesToDeploy,
	}
	resp, err := connection.DeploySome(ctx, req)
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to deploy some Chef Automate services failed",
		)
	}

	deploymentID := &api.DeploymentID{}
	h := &client.CLIEventWriter{Writer: writer}
	err = connection.StreamDeployEvents(resp.TaskId, deploymentID, h)
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to stream deployment events failed",
		)
	}

	return nil
}

func newDepsCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "service-deps SERVICE...",
		RunE:  runDepsCmd,
		Short: "Print the  dependencies of the given service",
	}

	cmd.Flags().BoolVar(&devCmdFlags.reverse,
		"reverse", false, "Print the reverse dependency list.")

	return cmd
}

func runDepsCmd(cmd *cobra.Command, args []string) error {
	serviceIDs, err := services.ServicesInCollection("automate-full")
	if err != nil {
		return status.Wrap(err, status.DeployError, "Failed to load list of services")
	}

	var sortedDeps []habpkg.HabPkg
	if devCmdFlags.reverse {
		sortedDeps, err = bind.RDeps(args, serviceIDs, services.AllBinds)
		if err != nil {
			return status.Wrap(err, status.DeployError, "Dependency resolution for given services failed")
		}
	} else {
		sortedDeps, err = bind.TopoSort(args, serviceIDs, services.AllBinds)
		if err != nil {
			return status.Wrap(err, status.DeployError, "Dependency resolution for given services failed")
		}
	}

	for _, s := range sortedDeps {
		writer.Println(habpkg.Ident(&s))
	}
	return nil
}

var removeSomeCmd = &cobra.Command{
	Use:   "remove-some [svc1 svc2 svcN...]",
	RunE:  runRemoveSomeCmd,
	Short: "Removes the specified services.",
	Long:  "Removes the specified services. Specify services as origin/name.",
}

func runRemoveSomeCmd(cmd *cobra.Command, svcNames []string) error {
	//// RemoveSome removes the provided slice of services.
	//// Elements of svcNames should be in the form origin/name.
	//func (cliApp *App) RemoveSome(svcNames []string) error {
	msg := fmt.Sprintf("Starting removal of %d services", len(svcNames))
	writer.Title(msg)
	// Use the same timeout for deploy and remove
	connection, err := client.Connection(client.DefaultDeployTimeout)
	if err != nil {
		return err
	}

	ctx := context.Background()
	req := &api.RemoveRequest{
		Services: svcNames,
	}

	_, err = connection.RemoveSome(ctx, req)
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to remove some Chef Automate services failed",
		)
	}

	writer.Title("Services removed.")
	return nil
}

var stopConvergeCmd = &cobra.Command{
	Use:   "stop-converge",
	RunE:  runStopConvergeCmd,
	Short: "Stops the periodic converge loop.",
	Long:  "Stops the periodic converge loop.  Does nothing if the converge loop is already stopped.",
}

func runStopConvergeCmd(cmd *cobra.Command, _ []string) error {
	//// StopConverge stops the periodic converge loop.  Dev only for now
	//func (cliApp *App) StopConverge() error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	_, err = connection.StopConverge(context.Background(), &api.StopConvergeRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to stop the deployment-service converge loop failed",
		)
	}

	writer.Title("Stopped converge loop.")
	return nil
}

var startConvergeCmd = &cobra.Command{
	Use:   "start-converge",
	RunE:  runStartConvergeCmd,
	Short: "Starts the periodic converge loop.",
	Long:  "Starts the periodic converge loop.  Does nothing if the converge loop is already running.",
}

func runStartConvergeCmd(cmd *cobra.Command, _ []string) error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return err
	}

	_, err = connection.StartConverge(context.Background(), &api.StartConvergeRequest{})
	if err != nil {
		return status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request to start the deployment-service converge loop failed",
		)
	}

	writer.Title("Started converge loop.")
	return nil
}

func genDocsCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "generate-docs",
		RunE:  runGenDocsCmd,
		Short: "Generate docs.",
		Long:  "Generate docs.",
		Annotations: map[string]string{
			NoCheckVersionAnnotation: NoCheckVersionAnnotation,
			NoRequireRootAnnotation:  NoRequireRootAnnotation,
		},
	}

	cmd.Flags().StringVar(&devCmdFlags.docsPath,
		"docs-dir", "./", "Directory where generated documentation will be put")
	cmd.Flags().StringVar(&devCmdFlags.docsFormat,
		"format", "yaml", `Generated documentation format. Only "yaml" is supported for now`)
	return cmd
}

func runGenDocsCmd(cmd *cobra.Command, _ []string) error {
	switch devCmdFlags.docsFormat {
	case "yaml":
		if err := docs.GenYamlTree(RootCmd, devCmdFlags.docsPath); err != nil {
			return status.Wrap(err, status.ConfigError, "Documentation generation failed")
		}
	default:
		return status.New(status.InvalidCommandArgsError, `"yaml" is the only acceptable format.`)
	}
	return nil
}

func newDumpA1PGCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "a1-dump-pg",
		RunE:  runDumpA1PG,
		Short: "Dump an A1 postgres database using the same method the actual upgrade does",
	}
	cmd.PersistentFlags().StringVarP(
		&upgradeCmdFlags.a2ConfigPath,
		"config",
		"c",
		"",
		"Path to an automate-deploy.toml")
	cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.enableChefServer,
		"enable-chef-server",
		false,
		"Enable integrated Chef Server migration and deployment; only valid for all-in-one topology")
	cmd.PersistentFlags().BoolVar(
		&upgradeCmdFlags.enableWorkflow,
		"enable-workflow",
		false,
		"Enable integrated Workflow Server migration and deployment; only valid for all-in-one topology")
	cmd.PersistentFlags().IntVar(
		&upgradeCmdFlags.pgDumpSeconds,
		"postgres-dump-wait-seconds",
		300,
		"Optional max wait for Chef Automate v1 PostgreSQL dump (default = 300 seconds)")

	return cmd
}

func newExtractA1UserData() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "a1-extract-user-data",
		RunE:  runExtractA1UserData,
		Short: "Extract A1 User Data",
	}
	cmd.PersistentFlags().StringVarP(
		&upgradeCmdFlags.a2ConfigPath,
		"config",
		"c",
		"",
		"Path to an automate-deploy.toml")
	cmd.PersistentFlags().IntVar(
		&upgradeCmdFlags.pgDumpSeconds,
		"postgres-dump-wait-seconds",
		300,
		"Optional max wait for Chef Automate v1 PostgreSQL dump (default = 300 seconds)")

	return cmd
}

func runDumpA1PG(cmd *cobra.Command, _ []string) error {
	upgrade, err := newLocalUpgrade()
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Configuring database dump operation failed")
	}

	if err = client.A1PgDump(writer, upgrade); err != nil {
		return status.Wrap(err, status.UpgradeError, "Database dump failed")
	}

	return nil
}

func runExtractA1UserData(cmd *cobra.Command, _ []string) error {
	upgrade, err := newLocalUpgrade()
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Configuring database user data extraction failed")
	}

	if err = client.A1ExtractUserData(writer, upgrade); err != nil {
		return status.Wrap(err, status.UpgradeError, "Database user data extraction failed")
	}

	if err = client.A1ExtractUserRolesData(writer, upgrade); err != nil {
		return status.Wrap(err, status.UpgradeError, "Database user roles data extraction failed")
	}

	return nil
}

func newRunA1BackupCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "a1-create-backup",
		RunE:  runA1Backup,
		Short: "Run automate-ctl create-backup and display progress",
	}
	return cmd
}

func runA1Backup(cmd *cobra.Command, backupNameList []string) error {
	u, err := a1upgrade.NewA1Upgrade(
		a1upgrade.WithDeliveryRunning("/etc/delivery/delivery-running.json"),
		a1upgrade.WithDeliverySecrets("/etc/delivery/delivery-secrets.json"),
	)
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Configuring A1 backup failed")
	}

	if err = client.DevA1Backup(writer, u); err != nil {
		return err
	}

	return nil
}

func newReindexEs2Cmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "a1-reindex-es2",
		RunE:  runReindexEs2,
		Short: "Run es2 reindex process against localhost:9200",
	}
	return cmd
}

func runReindexEs2(cmd *cobra.Command, args []string) error {
	r, err := a1upgrade.NewReindexer(writer, "http://localhost:9200")
	if err != nil {
		return status.Wrap(err, status.ConfigError, "Configuring A1 Elasticsearch re-indexer failed")
	}

	if err = r.RunReindex(); err != nil {
		return status.Wrap(err, status.UpgradeError, "A1 Elasticsearch re-index failed")
	}

	return nil
}

var psqlCmd = &cobra.Command{
	Use:   "psql",
	RunE:  runPsql,
	Short: "Run psql as the PostgreSQL superuser",
	Args:  cobra.ArbitraryArgs,
}

func runPsql(cmd *cobra.Command, args []string) error {
	connInfo := pg.A2ConnInfo{
		Host:  "localhost",
		Port:  5432,
		User:  "automate",
		Certs: pg.A2SuperuserCerts,
	}

	pager := os.Getenv("PAGER")
	if pager == "" {
		pager = "less"
	}

	psqlArgs := append(pg.PSQLCmd[1:], args...)
	psqlOpts := append(connInfo.PsqlCmdOptions(),
		command.Envvar("PAGER", pager),
		command.Args(psqlArgs...),
		command.Stderr(os.Stderr),
		command.Stdout(os.Stdout),
		command.Stdin(os.Stdin))
	return command.Run(pg.PSQLCmd[0], psqlOpts...)
}

var defaultCfg = &cobra.Command{
	Use:   "default-config",
	RunE:  runDefaultConfig,
	Short: "Display the default config",
}

func runDefaultConfig(cmd *cobra.Command, args []string) error {
	cfgToml, err := dc.DefaultAutomateConfig().MarshalToTOML()
	if err != nil {
		return status.Wrap(
			err,
			status.MarshalError,
			"Marshaling configuration to TOML failed",
		)
	}

	writer.Println(string(cfgToml))

	return nil
}

func newGetURLCmd() *cobra.Command {
	c := &cobra.Command{
		Use:   "get-url URL",
		RunE:  runGetURL,
		Short: "Fetch the given URL using Go.",
		Args:  cobra.ExactArgs(1),
		Long:  "Fetches the given URL using Go. Useful for debugging CDN issues that can't be reproduced with curl",
		Annotations: map[string]string{
			NoRequireRootAnnotation: NoRequireRootAnnotation,
		},
	}
	return c
}

func runGetURL(cmd *cobra.Command, args []string) error {
	req, err := http.NewRequest("GET", args[0], nil)
	if err != nil {
		return errors.Wrap(err, "creating request")
	}

	trace := &httptrace.ClientTrace{
		DNSDone: func(d httptrace.DNSDoneInfo) {
			addrStrs := make([]string, len(d.Addrs))
			for i, addr := range d.Addrs {
				addrStrs[i] = addr.String()
			}

			if d.Err != nil {
				// It wasn't clear to me in the docs
				// whether d.Err != nil is always
				// fatal, so we still log the
				// addresses here.
				logrus.Debugf("HTTP TRACE: %q resolved to %v (with error: %s)",
					req.URL.Host, addrStrs, d.Err.Error())
				return
			}
			logrus.Debugf("HTTP TRACE: %q resolved to %v", req.URL.Host, addrStrs)
		},
		GotConn: func(c httptrace.GotConnInfo) {
			logrus.Debugf("HTTP TRACE: connected to %q (reused: %t) (was idle: %t)",
				c.Conn.RemoteAddr(), c.Reused, c.WasIdle)
		},
	}

	req = req.WithContext(httptrace.WithClientTrace(req.Context(), trace))
	c := http.Client{}
	response, err := c.Do(req)
	if err != nil {
		return errors.Wrap(err, "sending request")
	}
	defer func() {
		_ = response.Body.Close()
	}()

	bodyBytes, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return errors.Wrap(err, "reading response body")
	}

	for k, v := range response.Header {
		logrus.Debugf("HTTP RESPONSE HEADER %s: %s", k, strings.Join(v, ", "))
	}
	fmt.Println(string(bodyBytes))
	return nil
}

var grpCurlCmd = &cobra.Command{
	Use:   "grpcurl SERVICE_NAME",
	RunE:  runGRPCurl,
	Short: "Run grpcurl with connection information for SERVICE_NAME",
	Args:  cobra.MinimumNArgs(1),
	Long: `Run grpcurl with connection information for SERVICE_NAME.

To pass options to grpcurl, use --

    chef-automate dev grpcurl SERVICE_NAME -- GRPCURL_OPTIONS
`,
}

var grpcOptsWithArgs = []string{
	"-H",
	"-authority",
	"-cacert",
	"-cert",
	"-connect-timeout",
	"-d",
	"-import-path",
	"-keepalive-time",
	"-key",
	"-max-time",
	"-proto",
	"-protoset",
	"-reflect-header",
	"-rpc-header",
	"-servername",
}

func runGRPCurl(cmd *cobra.Command, args []string) error {
	ctx, cancel := context.WithTimeout(context.Background(), time.Second*30)
	defer cancel()

	svcName := args[0]

	grpcPkg := habpkg.New("core", "grpcurl")
	habcmd := target.NewHabCmd(command.DefaultExecutor, false)
	installed, err := habcmd.IsInstalled(ctx, &grpcPkg)
	if err != nil {
		return status.Wrap(
			err,
			status.HabCommandError,
			"Failed to determine if grpcurl is available",
		)
	}
	if !installed {
		return status.New(status.UnknownError, "core/grpcurl is not installed")
	}

	c := habapi.New("http://localhost:9631")
	svcs, err := c.ListServices(ctx)
	if err != nil {
		return status.Wrap(
			err,
			status.HabAPIError,
			"Failed to list services from habitat",
		)
	}
	port, found := habapi.PortForService(svcs, svcName)
	if !found {
		return status.Errorf(status.HabAPIError, "Could not find port for %q", svcName)
	}

	// grpcurl has very finicky option parsing
	grpcOpts := []string{}
	grpcArgs := []string{}
	for i := 1; i < len(args); i++ {
		if strings.HasPrefix(args[i], "-") {
			grpcOpts = append(grpcOpts, args[i])
			if stringutils.SliceContains(grpcOptsWithArgs, args[i]) {
				grpcOpts = append(grpcOpts, args[i+1])
				i++
			}
		} else {
			grpcArgs = append(grpcArgs, args[i])
		}
	}

	cmdArgs := append([]string{
		"pkg", "exec", "core/grpcurl", "grpcurl",
		"-servername", svcName,
		"-cacert", constants.RootCertPath,
		"-key", constants.KeyPath,
		"-cert", constants.CertPath,
	}, grpcOpts...)
	// TODO(multinode) 2018-11-02: Get host
	cmdArgs = append(cmdArgs, fmt.Sprintf("localhost:%d", port))
	cmdArgs = append(cmdArgs, grpcArgs...)

	return command.Run("hab",
		command.Envvar("HAB_LICENSE", "accept-no-persist"),
		command.Args(cmdArgs...),
		command.Stderr(os.Stderr),
		command.Stdout(os.Stdout),
		command.Stdin(os.Stdin))
}

func newDevPortCmd() *cobra.Command {
	list := &cobra.Command{
		Use:   "list",
		Short: "List used default ports",
		RunE: func(cmd *cobra.Command, args []string) error {
			cfg := dc.AutomateConfig{}
			a2ServiceConfigType := reflect.TypeOf((*a2conf.A2ServiceConfig)(nil)).Elem()
			a := reflect.TypeOf(cfg)
			writer.Bodyf("%-30s%-10s\t%-10s\t%-10s", "Service Name", "Port Name", "Default", "Protocol")
			for i := 0; i < a.NumField(); i++ {
				f := a.Field(i)
				if f.Type.Implements(a2ServiceConfigType) {
					v := (reflect.New(f.Type.Elem()).Interface()).(a2conf.A2ServiceConfig)
					//writer.Titlef("%s [%s]", v.ServiceName(), f.Name)
					ports := v.ListPorts()
					for _, p := range ports {
						writer.Bodyf("%-30s%-10s\t%-10d\t%-10s", v.ServiceName(), p.Name, p.Default, p.Protocol)
					}
				}
			}
			return nil
		},
	}

	portMapFunc := func() (map[uint16]string, error) {
		portMap := make(map[uint16]string)
		cfg := dc.AutomateConfig{}
		a2ServiceConfigType := reflect.TypeOf((*a2conf.A2ServiceConfig)(nil)).Elem()
		a := reflect.TypeOf(cfg)
		for i := 0; i < a.NumField(); i++ {
			f := a.Field(i)
			if f.Type.Implements(a2ServiceConfigType) {
				v := (reflect.New(f.Type.Elem()).Interface()).(a2conf.A2ServiceConfig)
				ports := v.ListPorts()
				for _, p := range ports {
					if n := portMap[p.Default]; n != "" {
						return nil, errors.Errorf("Default port %d is being used by %s and %s/%s", p.Default, n, v.ServiceName(), p.Name)
					}
					portMap[p.Default] = fmt.Sprintf("%s/%s", v.ServiceName(), p.Name)
				}
			}
		}
		return portMap, nil
	}
	unique := &cobra.Command{
		Use:   "unique",
		Short: "Asserts ports are unique",
		RunE: func(cmd *cobra.Command, args []string) error {
			_, err := portMapFunc()
			if err != nil {
				return err
			}
			return nil
		},
	}

	free := &cobra.Command{
		Use:   "free",
		Short: "List free ports",
		RunE: func(cmd *cobra.Command, args []string) error {
			portMap, err := portMapFunc()
			if err != nil {
				return err
			}

			c := 0
			for i := uint16(10100); i < 10200; i++ {
				if portMap[i] == "" {
					writer.Printf("%-6d", i)
					c++
				}

				if c%10 == 0 {
					writer.Printf("\n")
				}
			}
			if c%10 != 0 {
				writer.Printf("\n")
			}
			return nil
		},
	}

	cmd := &cobra.Command{
		Use: "ports",
	}

	cmd.AddCommand(list)
	cmd.AddCommand(unique)
	cmd.AddCommand(free)

	return cmd
}

func newVerifyPackagesCmd() *cobra.Command {
	c := &cobra.Command{
		Use: "verify-packages",
		RunE: func(*cobra.Command, []string) error {
			return hab.VerifyHabPackages()
		},
		Short: "Verify installed hab packages",
		Args:  cobra.NoArgs,
		Annotations: map[string]string{
			NoRequireRootAnnotation: NoRequireRootAnnotation,
		},
	}
	return c
}

func newEnablePrometheusCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "enable-prometheus",
		RunE:  runEnablePrometheusCmd,
		Short: "Configure automate to run the automate-prometheus metrics gathering service",
		Args:  cobra.NoArgs,
		Annotations: map[string]string{
			NoRequireRootAnnotation: NoRequireRootAnnotation,
		},
	}
}

func newDisablePrometheusCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "disable-prometheus",
		RunE:  runDisablePrometheusCmd,
		Short: "Configure automate to no longer run the automate-prometheus metrics gathering service",
		Args:  cobra.NoArgs,
		Annotations: map[string]string{
			NoRequireRootAnnotation: NoRequireRootAnnotation,
		},
	}
}

func newCreateIAMDevUsersCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "create-iam-dev-users",
		RunE:  runCreateIAMDevUsersCmd,
		Short: `Create IAM v2 dev users ("viewer" and "editor") idempotently`,
		Args:  cobra.NoArgs,
	}
}

// Equivalent to patching the config with the following toml:
// [deployment.v1.svc]
// enable_dev_monitoring = true
func runEnablePrometheusCmd(*cobra.Command, []string) error {
	cfg := &dc.AutomateConfig{
		Deployment: &dc.ConfigRequest{
			V1: &dc.ConfigRequest_V1{
				Svc: &dc.ConfigRequest_V1_Service{
					EnableDevMonitoring: w.Bool(true),
				},
			},
		},
	}
	if err := client.PatchAutomateConfig(configCmdFlags.timeout, cfg, writer); err != nil {
		return err
	}
	return nil
}

func runDisablePrometheusCmd(*cobra.Command, []string) error {
	cfg := &dc.AutomateConfig{
		Deployment: &dc.ConfigRequest{
			V1: &dc.ConfigRequest_V1{
				Svc: &dc.ConfigRequest_V1_Service{
					EnableDevMonitoring: w.Bool(false),
				},
			},
		},
	}
	if err := client.PatchAutomateConfig(configCmdFlags.timeout, cfg, writer); err != nil {
		return err
	}
	return nil
}

func runCreateIAMDevUsersCmd(*cobra.Command, []string) error {
	ctx := context.TODO()
	apiClient, err := apiclient.OpenConnection(ctx)
	if err != nil {
		return err
	}
	for username, data := range map[string]struct {
		displayName, password, team string
	}{
		"viewer": {"Viewer User", "chefautomate", "viewers"},
		"editor": {"Editor User", "chefautomate", "editors"},
	} {
		userID, _, err := adminmgmt.CreateUserOrUpdatePassword(ctx,
			apiClient, username, data.displayName, data.password, false /* dry run */)
		if err != nil {
			return err
		}
		// Note: the teams SHOULD exist. But since you never know what happens in a
		// long running acceptance env, we'll better ensure them:
		teamID, _, err := adminmgmt.EnsureTeam(ctx, data.team, data.team /* description */, apiClient, false /* dry run */)
		if err != nil {
			return err
		}
		_, err = adminmgmt.AddUserToTeam(ctx, apiClient, teamID, userID, false /* dry run */)
		if err != nil {
			return err
		}
	}

	return nil
}
