// Copyright © 2017 Chef Software

// Package client contains all the business logic of the command line deployment
// tool. It knows how to talk to Habitat and the grpc client to communicate
// with the server.
package client

import (
	"context"
	"crypto/rand"
	"fmt"
	"io/ioutil"
	"math/big"
	"os"
	"os/exec"
	"os/user"
	"path"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	gstatus "google.golang.org/grpc/status"

	dc "github.com/chef/automate/api/config/deployment"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"
	"github.com/chef/automate/components/automate-deployment/pkg/bootstrap"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/preflight"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/platform/pg"
	"github.com/chef/automate/lib/platform/sys"
)

// deployTimeout is used as the timeout for the deployers initial
// connection to the deployment-service. The gRPC library has built-in
// retries.  Thus, we set this to a high timeout to account for the
// fact that it may take some time for deployment service to start up.
var deployTimeout = 5 * time.Minute

const editConfigInstructions = `To change the configuration:

* Respond No to the "Are you ready to continue?" prompt,
* Edit the provided config file,
* Restart the upgrade using the edited config file:

`

const cancelUpgradeInstructionsFmt = `If you would prefer to cancel the upgrade, you will need to delete the following
file to return your Chef Automate v1 installation to a working state:

/var/opt/delivery/delivery/etc/maint.flag

Additionally, you will need to restart the stopped Chef Automate v1 processes
with the following command(s):

%s

Some components of Chef Automate v2 have already started. You can disable them
by running the following:

chef-automate stop

Please contact support@chef.io if you need further assistance.
`

const cancelUpgradeWithRestorePreamble = `
Please contact support@chef.io with the output of this command, along with the
logs of the affected services. You can capture the logs with the following
command:

chef-automate gather-logs --local-fallback

If you wish to cancel the upgrade, you will need to shut down Chef Automate v2,
disable maintenance mode for Chef Automate v1, and then restore Chef Automate
v1 from backup.

The following commands will stop Chef Automate v2 and disengage maintenance
mode on your Chef Automate v1 installation:

systemctl stop chef-automate
systemctl disable chef-automate
rm /var/opt/delivery/delivery/etc/maint.flag
`

const recoverFromFailedCredentialSave = `
To recover from a failed credential save you can reset the admin password after deployment has complete with the following command:

chef-automate iam admin-access restore PASSWORD
`

const howToRestore = `
The following command will restore your Chef Automate v1 installation's data:
%s
`

const howToRestoreFallback = `
A backup of your Chef Automate v1 data was made, but the arguments to the
automate-ctl restore-backup command cannot be determined from your settings.
Please consult the Chef Automate v1 documentation for the correct syntax.

The snapshot created by the upgrade process was named:
%s
`

const consolationPrize = `
Integrated backups were disabled for this upgrade. If you have a custom
backup/restore procedure, you may use that to recover your Chef Automate v1 data.
`

const upgradeSummary = `Ready to upgrade to Chef Automate v2. The upgrade process consists of these steps:

* Preload Chef Automate v2 binaries
* Enable maintenance mode in Chef Automate v1
* Backup your Chef Automate v1 data
* Shutdown your Chef Automate v1 installation
* Start Chef Automate v2 and run stage 1 data migrations
* Run stage 2 data migrations in the background

The stage 2 data migrations are irreversible. You will be required to reinstall
Chef Automate v1 from backup if you wish to revert the upgrade after the stage
2 migrations begin.
`

const optedInToTelemetry = `Users of this Automate deployment may elect to share anonymized usage data with
Chef Software, Inc. Chef uses this shared data to improve Automate.
Please visit https://chef.io/privacy-policy for more information about the
information Chef collects, and how that information is used.
`

const pleaseOptInToTelemetry = `Users of this Automate deployment cannot choose to share anonymized usage data
with Chef Software, Inc. Chef uses this shared data to improve Automate
Please consider allowing your users to share this data enabling telemetry.
See the Chef Automate v2 telemetry docs for details.
`

const uninstallWarning = `!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
This command will uninstall Chef Automate and any other applications deployed
with Habitat on this system. All associated data will be permanently destroyed.

If you wish to preserve your data, create a backup before uninstalling.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!`

func init() {
	if s, err := strconv.Atoi(os.Getenv("AUTOMATE_DEPLOY_TIMEOUT")); err == nil {
		deployTimeout = time.Duration(s) * time.Second
	}
}

// deployer runs a deployment by coordinating interactions of other types
type deployer struct {
	ctx                  context.Context
	client               api.DeployClientStreamer
	overrideCfg          *dc.AutomateConfig
	mergedCfg            *dc.AutomateConfig
	appliedCfg           *dc.AutomateConfig
	upgrade              *a1upgrade.A1Upgrade
	err                  error
	deploymentID         *api.DeploymentID
	target               target.Target
	writer               cli.FormatWriter
	manifestProvider     manifest.ReleaseManifestProvider
	configPath           string
	cliVersion           string
	a1BackupNameMemoized string
	uninstallOpts        UninstallOpts
	airgap               bool
	bootstrapBundlePath  string
}

type UninstallOpts struct {
	Yes              bool
	PreservePkgCache bool
}

// newDeployer initializes a deployer with usable values for pure install
func newDeployer(writer cli.FormatWriter,
	overrideCfg *dc.AutomateConfig,
	manifestProvider manifest.ReleaseManifestProvider,
	cliVersion string,
	airgap bool) deployer {
	return deployer{
		ctx:              context.Background(),
		err:              nil,
		overrideCfg:      overrideCfg,
		target:           target.NewLocalTarget(airgap),
		writer:           writer,
		manifestProvider: manifestProvider,
		cliVersion:       cliVersion,
		airgap:           airgap,
	}
}

// newUpgradeDeployer initializes a deployer with usable values for upgrades
func newUpgradeDeployer(writer cli.FormatWriter,
	upgrade *a1upgrade.A1Upgrade,
	manifestProvider manifest.ReleaseManifestProvider,
	cliVersion string,
	airgap bool) deployer {

	return deployer{
		ctx:              context.Background(),
		upgrade:          upgrade,
		err:              nil,
		target:           target.NewLocalTarget(airgap),
		writer:           writer,
		manifestProvider: manifestProvider,
		cliVersion:       cliVersion,
		airgap:           airgap,
	}
}

// newUnDeployer initializes a deployer with usable values for an uninstall
func newUnDeployer(writer cli.FormatWriter, opts UninstallOpts) deployer {
	return deployer{
		ctx:           context.Background(),
		target:        target.NewLocalTarget(true),
		err:           nil,
		writer:        writer,
		uninstallOpts: opts,
	}
}

// Preflight runs preflight checks for the pure install case
func Preflight(writer cli.FormatWriter,
	overrideCfg *dc.AutomateConfig,
	cliVersion string,
	airgap bool) error {

	d := deployer{
		ctx:         context.Background(),
		overrideCfg: overrideCfg,
		target:      target.NewLocalTarget(airgap),
		writer:      writer,
		cliVersion:  cliVersion,
		airgap:      airgap,
	}
	d.genMergedConfig()
	d.preflight(false)
	return d.err
}

// Deploy bootstraps the system and then installs Chef Automate v2
func Deploy(writer cli.FormatWriter,
	overrideConfig *dc.AutomateConfig,
	skipPreflight bool,
	manifestProvider manifest.ReleaseManifestProvider,
	cliVersion string,
	airgap bool,
	bootstrapBundlePath string) error {

	d := newDeployer(writer, overrideConfig, manifestProvider, cliVersion, airgap)
	d.bootstrapBundlePath = bootstrapBundlePath
	d.genMergedConfig()
	if !skipPreflight {
		d.preflight(false)
	}
	d.bootstrap()
	d.waitForDeploymentService()
	d.connect()
	d.configureDeployment()
	d.deployAll()
	d.saveDeploymentCreds()
	d.showTelemetryNotice()

	return d.err
}

func Destroy(writer cli.FormatWriter, opts UninstallOpts) error {
	d := newUnDeployer(writer, opts)
	d.destroy()
	return d.err
}

// A1Upgrade bootstraps the system, copies/moves a1 data to a2, and then installs a2
func A1Upgrade(writer cli.FormatWriter,
	upgrade *a1upgrade.A1Upgrade,
	acceptDefaults bool,
	manifestProvider manifest.ReleaseManifestProvider,
	cliVersion string,
	airgap bool,
) error {

	d := newUpgradeDeployer(writer, upgrade, manifestProvider, cliVersion, airgap)

	d.prepare()
	d.upgradePreflight()
	d.summarizeSteps(acceptDefaults)
	d.bootstrap()
	d.waitForDeploymentService()
	d.connect()
	d.configureDeployment()
	d.preload()
	d.DoA1MaintMode()
	d.doA1Backup()
	d.migrateEs2Indices()
	d.extractA1PostgreSQLData()
	d.extractA1UserData()
	d.extractA1UserRolesData()
	d.extractA1ChefServerCredentials()
	d.doA1Shutdown()
	d.printUpgradeHeader()
	// -- POINT OF NO RETURN (except via a1 restore) --:
	// After this point, data has been removed from a1 in ways that we could
	// theoretically undo but are probably too complicated to expect customers to
	// get right. Failures in subsequent steps should advise customers to restore
	// from backup if they want to abort.
	d.moveA1ESData()
	d.moveA1ComplianceData()
	d.writeA1ComplianceSecret()
	d.moveA1NotificationData()
	d.moveA1WorkflowGitRepos()
	d.writeA1WorkflowErlCookie()
	d.deployDataServices()
	d.restorePostgresData()
	d.startNonDataServices()
	d.showTelemetryNotice()
	d.initialMigrationComplete()

	return d.err
}

// A1PgDump runs the PGDump portion of the A1 migration process.  It
// is used to create a1migration data artifacts.
func A1PgDump(writer cli.FormatWriter, upgrade *a1upgrade.A1Upgrade) error {
	d := newUpgradeDeployer(writer, upgrade, nil, "", false)
	d.extractA1PostgreSQLData()
	return d.err
}

// A1ExtractUserData runs the A1 user data extraction portion of the A1
// migration process.
func A1ExtractUserData(writer cli.FormatWriter, upgrade *a1upgrade.A1Upgrade) error {
	d := newUpgradeDeployer(writer, upgrade, nil, "", false)
	d.extractA1UserData()
	return d.err
}

// A1ExtractUserRolesData runs the A1 user data extraction portion of the A1
// migration process.
func A1ExtractUserRolesData(writer cli.FormatWriter, upgrade *a1upgrade.A1Upgrade) error {
	d := newUpgradeDeployer(writer, upgrade, nil, "", false)
	d.extractA1UserRolesData()
	return d.err
}

func (d *deployer) prepare() {
	if d.err != nil {
		return
	}

	// set the pg superuser password before all other checks
	// some of the checks (e.g. saml check) require password
	// based authn with postgres, and this sets a known username
	// and password that can be used to execute these checks
	d.setPgSuperuserPassword()

	d.validateUpgradableA1Config()
	d.validateUpgradableChefServerConfig()
	// TODO @afiune maybe check Workflow Config here?
	d.validateUserOverrideConfig()
	d.genMergedConfig()
}

func getOrGuessFQDN() string {
	// If deployment service is available, try to get FQDN from it
	res, err := GetAutomateConfig(int64(DefaultClientTimeout.Seconds()))
	if err == nil {
		maybeFQDN := res.Config.GetGlobal().GetV1().GetFqdn().GetValue()
		// if user is uninstalling because of severe breakage, configuration may
		// not be reliable
		if maybeFQDN != "" {
			return maybeFQDN
		}
		logrus.Debug("FQDN in deployment service configuration was set to empty string (\"\")")
	} else {
		logrus.WithError(err).Debug("failed to load configuration from deployment service")
	}
	// fallback to asking the system
	return dc.LbFQDN()
}

func (d *deployer) destroy() {
	if !d.uninstallOpts.Yes {
		d.writer.LongWarningln(uninstallWarning)

		fqdn := getOrGuessFQDN()

		prepromptFmt := `The FQDN of your Chef Automate installation is "%s".
Type the FQDN in to continue.`
		d.writer.Titlef(prepromptFmt, fqdn)
		response, err := d.writer.Prompt("FQDN")
		if err != nil {
			d.err = err
			return
		}
		if response != fqdn {
			d.writer.Titlef("Response did not match \"%s\", canceling uninstall", fqdn)
			return
		}
	}

	d.writer.Title("Removing Chef Automate")

	d.writer.Body("stopping Automate services")
	err := d.target.EnsureStopped()
	if err != nil {
		d.err = err
		return
	}

	d.writer.Body("removing Automate from system startup configuration")
	err = d.target.EnsureDisabled()
	if err != nil {
		d.err = err
		return
	}

	d.writer.Body("removing Habitat user and configuration")
	err = d.target.DestroySupervisor()
	if err != nil {
		d.err = err
		return
	}

	d.writer.Body("removing Automate data stores")
	err = d.target.DestroyData()
	if err != nil {
		d.err = err
		return
	}

	if !d.uninstallOpts.PreservePkgCache {
		d.writer.Body("removing Habitat package cache")
		err = d.target.DestroyPkgCache()
		if err != nil {
			d.err = err
			return
		}
	} else {
		d.writer.Body("skipping Habitat package cache removal")
	}

	d.writer.Title("Chef Automate uninstalled")
}

func (d *deployer) validateUserOverrideConfig() {
	if d.err != nil {
		return
	}

	if err := d.getOverrideCfg().ValidateWithGlobalAndDefaults(); err != nil {
		d.err = status.Annotate(err, status.ConfigError)
	}
}

func (d *deployer) genMergedConfig() {
	if d.err != nil {
		return
	}

	// Bootstrapping hab and the deployment service may need defaults, however,
	// the defaults should not be appled until after the override config has
	// been persisted via the deployment-service. Instead, make a copy with
	// defaults and globals applied and use the copy to bootstrap the deployment
	// service, then pass the override config to the deployment service during
	// the deploy.
	mergedCfg, err := dc.MergeWithDefaults(d.getOverrideCfg())
	mergedCfg.SetGlobalConfig()
	if err != nil {
		d.err = status.Annotate(err, status.ConfigError)
		return
	}
	// We may need the proxy configuration for communicating with
	// the internet to install hab, etc.
	SetProxyEnvironment(mergedCfg.Deployment)
	d.mergedCfg = mergedCfg
}

// setPgSuperuserPassword determines if A1 machine has superuser enabled and
// attempts to enable it if it has not. This allows us to connect using to
// the postgresql instance using the http socket with the superuser name and
// password.
func (d *deployer) setPgSuperuserPassword() {
	if d.err != nil {
		return
	}

	superuserEnable := d.upgrade.DeliveryRunning.Delivery.PostgreSQL.SuperuserEnable
	pass := d.upgrade.DeliverySecrets.Postgresql.SuperuserPassword
	user := d.upgrade.DeliveryRunning.Delivery.PostgreSQL.SuperuserUsername

	if !superuserEnable {
		if err := a1upgrade.PsqlAlterUserWithPassword(user, pass); err != nil {
			err = status.Wrap(err,
				status.DatabaseError,
				"Failed to set the superuser password. If the superuser password is not set the migration cannot export the postgresql databases.",
			)
			d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		}
	}
}

// In the upgrade case, the A1Upgrade object owns the a2 config, but for a
// plain install, deployer owns it. This getter handles that special case, and
// should be used to get the a2config for all code here unless it only applies
// to one of the cases
func (d *deployer) getOverrideCfg() *dc.AutomateConfig {
	if d.upgrade == nil {
		return d.overrideCfg
	}
	return d.upgrade.A2Config
}

// PreflightCheck verifies host is ready for Setup/Bootstrap
func (d *deployer) preflight(skipSharedPortCheck bool) {
	if d.err != nil {
		return
	}

	preflightCheckOpts := preflight.DeployPreflightCheckOptions{
		SkipSharedPortCheck: skipSharedPortCheck,
		Airgap:              d.airgap,
		AutomateConfig:      d.mergedCfg,
	}

	d.writer.Title("Beginning pre-flight checks")

	hartifactsPath := d.mergedCfg.GetDeployment().GetV1().GetSvc().GetHartifactsPath().GetValue()
	overrideOrigin := d.mergedCfg.GetDeployment().GetV1().GetSvc().GetOverrideOrigin().GetValue()
	channel := d.mergedCfg.GetDeployment().GetV1().GetSvc().GetChannel().GetValue()

	if !d.airgap {
		err := CLIUpToDateWithLatestManifest(hartifactsPath, overrideOrigin, channel, d.cliVersion)
		if err != nil {
			d.err = status.Annotate(err, status.PreflightError)
			return
		}
	}

	out, err := preflight.RunDeployPreflightCheck(preflightCheckOpts)
	if err != nil {
		d.err = status.Annotate(err, status.PreflightError)
		return
	}
	d.writer.Title(out)
}

func (d *deployer) upgradePreflight() {
	if d.err != nil {
		return
	}
	if d.upgrade.SkipUpgradePreflight {
		return
	}

	d.preflight(true)
	if d.err != nil {
		return
	}

	p := a1upgrade.NewPreflightRunner(
		d.writer,
		d.upgrade.DeliveryRunning,
		d.upgrade.DeliverySecrets,
		d.upgrade.EnableChefServer,
		d.upgrade.EnableWorkflow,
	)

	if err := p.Run(); err != nil {
		d.err = status.Annotate(err, status.PreflightError)
	}
}

func (d *deployer) summarizeSteps(acceptDefaults bool) {
	if d.err != nil {
		return
	}

	d.writer.Println(upgradeSummary)
	if acceptDefaults {
		d.writer.Println("Accepting configuration and continuing...")
		return
	}

	d.writeConfigForReview()
	if d.err != nil {
		return
	}

	viewConfig, err := d.writer.Confirm("Would you like to review the Chef Automate v2 configuration?")
	if err != nil {
		d.err = status.Annotate(err, status.InvalidCommandArgsError)
		return
	}

	if viewConfig {
		d.displayConfig()
	}

	d.writer.Titlef("Your Chef Automate v2 configuration has been written to %s\n", d.configPath)
	d.writer.Print(editConfigInstructions)
	d.writer.Printf("chef-automate migrate-from-v1 --config %s\n", d.configPath)

	userReady, err := d.writer.Confirm("Are you ready to continue?")
	if err != nil {
		d.err = status.Annotate(err, status.InvalidCommandArgsError)
		return
	}
	if !userReady {
		d.err = status.New(status.UpgradeError, "User cancelled upgrade")
	}
}

func (d *deployer) writeConfigForReview() {
	n := time.Now().UTC()
	datestamp := n.Format("20060102150405")

	wd, err := os.Getwd()
	if err != nil {
		d.err = status.Wrap(err, status.FileAccessError, "Could not get current working directory to write config")
		return
	}
	emittedConfig := fmt.Sprintf("config-%s.toml", datestamp)
	d.configPath = filepath.Join(wd, emittedConfig)
	copy, err := d.getOverrideCfg().RedactedCopy()
	if err != nil {
		d.err = status.Annotate(err, status.ConfigError)
		return
	}

	t, err := copy.MarshalToTOML()
	if err != nil {
		d.err = status.Annotate(err, status.MarshalError)
		return
	}

	err = ioutil.WriteFile(d.configPath, t, 0600)
	if err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to write config")
		d.err = err
		return
	}
}

func (d *deployer) displayConfig() {
	cmd, args := d.getConfigPagerCommand()
	if cmd == "" {
		// Couldn't find a pager, just print
		copy, err := d.getOverrideCfg().RedactedCopy()
		if err != nil {
			d.err = status.Annotate(err, status.ConfigError)
			return
		}
		t, err := copy.MarshalToTOML()
		if err != nil {
			d.err = status.Annotate(err, status.MarshalError)
			return
		}
		d.writer.Println(string(t))
		return
	}

	cmdExecutor := command.NewExecExecutor()
	err := cmdExecutor.Run(cmd, command.Args(args...), command.Stdin(os.Stdin), command.Stdout(os.Stdout), command.Stderr(os.Stderr))
	if err != nil {
		d.err = status.Wrapf(err, status.CommandExecutionError, "Failed to display configuration to terminal pager")
		return
	}
}

func (d *deployer) getConfigPagerCommand() (string, []string) {
	// Use the user-provided PAGER if we can find it on the path
	userPager := os.Getenv("PAGER")
	if userPager != "" {
		userPagerPath, err := exec.LookPath(userPager)
		if err == nil {
			return userPagerPath, []string{d.configPath}
		}
		d.writer.Warnf("Unable to find $PAGER=%s in PATH. Trying 'less' instead.", userPager)
	}

	// Try our default pager (less) if it is on the path
	lessPath, err := exec.LookPath("less")
	if err != nil {
		d.writer.Warnln("Unable to find 'less' in $PATH.")
		return "", []string{}
	}

	return lessPath, []string{"-X", "-PUse SPACE to scroll and q to exit", d.configPath}
}

func (d *deployer) bootstrap() {
	if d.err != nil {
		return
	}

	d.writer.Title("Bootstrapping Chef Automate")
	d.writer.Body("Fetching Release Manifest")
	m, err := d.manifestProvider.GetCurrentManifest(d.ctx, d.mergedCfg.Deployment.V1.Svc.Channel.GetValue())
	if err != nil {
		d.err = status.Annotate(err, status.PackageInstallError)
		return
	}

	b := bootstrap.NewCompatBootstrapper(d.target)
	d.err = bootstrap.FullBootstrap(d.ctx, b, m, d.mergedCfg.Deployment, d.bootstrapBundlePath, d.writer)
}

func (d *deployer) waitForDeploymentService() {
	if d.err != nil {
		return
	}

	d.writer.Body("Waiting for deployment-service to be ready")
	// If 5 minutes isn't enough, we should fix that problem
	startTime := time.Now()
	timeout := 5 * time.Minute
	sleepTime := 10 * time.Second

	// I think we could probably use contexts for this, but we are
	// using this style of for loop everywhere else.
	for attempts := 1; ; attempts++ {
		logrus.Debugf("health check attempt %d (%.1fs elapsed) (%.1fs interval)", attempts, time.Since(startTime).Seconds(), sleepTime.Seconds())
		_, err := Connection(2 * time.Second)
		if err == nil {
			break
		}

		if time.Since(startTime) > timeout {
			d.err = status.New(status.TimedOutError, "waiting for deployment-service healthy status")
			return
		}

		time.Sleep(sleepTime)
	}
}

func (d *deployer) connect() {
	if d.err != nil {
		return
	}

	d.writer.Body("Initializing connection to deployment-service")
	client, err := Connection(deployTimeout)
	if err != nil {
		d.err = err
		return
	}
	d.client = client
}

func (d *deployer) configureDeployment() {
	if d.err != nil {
		return
	}

	d.writer.Title("Applying Deployment Configuration")

	var errResponse error
	sleepTime := 25 * time.Second
	req := &api.ConfigureDeploymentRequest{Config: d.getOverrideCfg()}

	for retries, maxRetries := 1, 12; retries <= maxRetries; retries++ {
		resp, err := d.client.ConfigureDeployment(d.ctx, req)
		if err == nil {
			d.deploymentID = resp.DeploymentId
			d.appliedCfg = resp.Config
			return
		}
		errResponse = status.Wrap(err, status.DeploymentServiceCallError, "Request to configure deployment failed")

		retry := false
		if grpcStatus, ok := gstatus.FromError(err); ok {
			// first test for grpc codes:
			// codes.Unavailable means that we can retry
			if grpcStatus.Code() == codes.Unavailable {
				retry = true
			}

			// back-compat: old versions of the server don't return a code so we
			// should check the message
			if grpcStatus.Code() == codes.Unknown {
				retry = api.IsDeploymentServicePendingMessage(grpcStatus.Message())
			}
		}

		if retry {
			d.writer.Bodyf(
				"deployment-service pending updates: retry %d/%d",
				retries, maxRetries,
			)
			Disconnect()
			time.Sleep(sleepTime)
			d.connect()
			continue
		}

		break
	}
	d.err = errResponse
}

func (d *deployer) preload() {
	if d.err != nil {
		return
	}

	d.writer.Title("Starting preload")
	resp, err := d.client.Preload(d.ctx, &api.DeployRequest{})
	if err != nil {
		d.err = status.Wrap(err, status.DeploymentServiceCallError, "Failed to start preload")
		return
	}
	h := CLIEventWriter{Writer: d.writer}
	err = d.client.StreamDeployEvents(resp.TaskId, d.deploymentID, &h)
	if err != nil {
		d.err = status.Wrap(err, status.DeploymentServiceCallError, "Failed to preload services")
		return
	}
}

func (d *deployer) DoA1MaintMode() {
	if d.err != nil {
		return
	}

	d.writer.Title("Enabling maintenance mode for Chef Automate v1 installation")
	if err := a1upgrade.EngageMaintMode(); err != nil {
		err = status.Wrap(err, status.FileAccessError, fmt.Sprintf(`Failed to create the file %s
The upgrade command must create this file in order to put Chef Automate v1 into
maintenance mode. Make sure this file can be created by the current user and
try again.
`, a1upgrade.MaintDotFlag))
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	if d.mergedCfg.GetDeployment().GetV1().GetSvc().GetEnableChefServer().GetValue() {
		err := a1upgrade.ChefServerCtlStopService("nginx")
		if err != nil {
			err = status.Wrap(err, status.UpgradeError, `Failed to enable maintenance mode on Chef Automate v1 installation. The upgrade process must stop the Chef Server nginx process to put Chef Automate v1 into maintenance mode. Please ensure the follow commands executes without error and try again:

    chef-server-ctl stop nginx
`)
			d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
			return
		}
	}

	if err := a1upgrade.VerifyMaintMode(d.mergedCfg); err != nil {
		err = status.Wrap(err, status.UpgradeError, `Failed to enable maintenance mode on Chef Automate v1 installation. The upgrade process must put your Chef Automate v1 installation into maintenance mode to ensure all in-flight data is migrated to Chef Automate v2. If you are using a custom load balancer, please ensure requests return 503 and try again.
`)
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	if err := a1upgrade.VerifyA1QueueDrained(d.upgrade.A1Config); err != nil {
		err = status.Wrap(err, status.UpgradeError, `Chef Automate v1 failed to process queued data in the allotted time. This may indicate a problem in Chef Automate v1's data collection service or the system may have higher load than expected. The following command returns the count of incoming Chef Automate v1 data records that have not yet been
processed:

PATH=/opt/delivery/embedded/bin:$PATH rabbitmqctl list_queues -p /insights name messages

When this command returns zero records, you may try the upgrade again.
`)
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}
}

func (d *deployer) a1BackupName() string {
	if d.a1BackupNameMemoized == "" {
		t := time.Now().UTC()
		n := fmt.Sprintf("a1-migration-backup-%d-%d-%d-%d-%d-%d", t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second())
		d.a1BackupNameMemoized = n
	}
	return d.a1BackupNameMemoized
}

func (d *deployer) a1RestoreInstructions() (string, error) {
	n := d.a1BackupName()
	z, err := d.zstLocatorFor(n)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("automate-ctl restore-backup %s %s", z, n), nil
}

func (d *deployer) zstLocatorFor(backupName string) (string, error) {
	backupType := d.upgrade.A1Config.DeliveryRunning.Delivery.Backup.Type
	switch backupType {
	case "s3":
		// delivery["backup"]["region"]
		region := d.upgrade.A1Config.DeliveryRunning.Delivery.Backup.S3Region
		if region == "" {
			return "", status.New(status.ConfigError, "restore argument type is \"s3\" but region is not set")
		}
		// delivery["backup"]["bucket"]
		bucket := d.upgrade.A1Config.DeliveryRunning.Delivery.Backup.S3Bucket
		if bucket == "" {
			return "", status.New(status.ConfigError, "restore argument type is \"s3\" but bucket is not set")
		}
		// e.g., us-east-1:s3_bucket:chef-automate-backup.zst
		return fmt.Sprintf("%s:%s:%s.zst", region, bucket, backupName), nil
	case "fs":
		dir := d.upgrade.A1Config.DeliveryRunning.Delivery.Backup.Location
		if dir == "" {
			return "", status.New(status.ConfigError, "restore argument type is \"fs\" but location is not set")
		}
		basename := fmt.Sprintf("%s.zst", backupName)
		return filepath.Join(dir, basename), nil
	default:
		return "", status.Errorf(status.ConfigError, "'%s' in not a valid backup type", backupType)
	}
}

func (d *deployer) doA1Backup() {
	if d.err != nil {
		return
	}

	if d.upgrade.SkipBackup {
		d.writer.Title("Skipping backup of Chef Automate v1 data due to user configuration")
		return
	}

	explanation := `Failed to backup Chef Automate v1. This may indicate that backup of your Chef Automate v1 installation is taking
longer than expected, or it may indicate a larger problem. You may investigate further by checking the Chef Automate v1
logs with the following command:

automate-ctl tail

When you can backup your Chef Automate v1 installation successfully, run the upgrade command again to continue.
`

	d.writer.Title("Backing up Chef Automate v1 installation")
	d.writer.Bodyf("Creating Chef Automate v1 backup '%s'", d.a1BackupName())
	if err := a1upgrade.AutomateCtlCreateBackup(d.a1BackupName()); err != nil {
		err = status.Wrap(err, status.BackupError, explanation)
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	esURL := d.upgrade.A1Config.DeliveryRunning.Delivery.Elasticsearch.NginxProxyURL
	repoType := d.upgrade.A1Config.DeliveryRunning.Delivery.Backup.Type

	if err := a1upgrade.WaitForEsSnapshot(d.writer, esURL, repoType, d.a1BackupName()); err != nil {
		err = status.Wrap(err, status.BackupError, explanation)
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}
}

// DevA1Backup runs just the the backup part of an upgrade. This is exposed so
// we can use it in dev CLI tooling to verify backups function correctly.
// TODO: Investigate if we should rm this function
func DevA1Backup(w cli.FormatWriter, u *a1upgrade.A1Upgrade) error {
	d := newUpgradeDeployer(w, u, nil, "", false)
	d.doA1Backup()
	if d.err != nil {
		return status.Annotate(d.err, status.BackupError)
	}
	instructions, err := d.a1RestoreInstructions()
	if err != nil {
		return status.Annotate(d.err, status.BackupError)
	}

	d.writer.Title("The backup can be restored with the command:")
	d.writer.Body(instructions)

	return nil
}

func (d *deployer) migrateEs2Indices() {
	if d.err != nil {
		return
	}

	d.writer.Title("Ensuring Elasticsearch data is compatible with Chef Automate v2")

	esURL := d.upgrade.A1Config.DeliveryRunning.Delivery.Elasticsearch.NginxProxyURL
	w := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)

	r, err := a1upgrade.NewReindexer(w, esURL)
	if err != nil {
		err = status.Wrap(err, status.UpgradeError, `Failed to connect to Chef Automate v1 Elasticsearch instance. You may investigate further by checking the Chef Automate v1 logs with the following command:

automate-ctl tail
`)
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	if err = r.RunReindex(); err != nil {
		err = status.Wrap(err, status.UpgradeError, `Failed to migrate Chef Automate v1 data for Elasticsearch 6 compatibility. If the error is transient, you can re-run the upgrade command to try again. You may investigate further by checking the Chef Automate v1 logs with the following command:

automate-ctl tail
`)
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}
}

func (d *deployer) doA1Shutdown() {
	if d.err != nil {
		return
	}

	d.writer.Title("Shutting down Chef Automate v1 installation")
	if err := a1upgrade.AutomateCtlStop(); err != nil {
		err = status.Wrap(err, status.ServiceUnloadError, `Failed to shut down Chef Automate v1. This may indicate a process is simply taking longer than expected to shut down cleanly, or it may indicate a larger problem. You may investigate further by checking the Chef Automate v1 logs with the following command:

automate-ctl tail

When the automate-ctl status command indicates that all processes are stopped run the upgrade command again to continue.
`)
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	if a1upgrade.SystemCtlIsEnabledDelivery() {
		if err := a1upgrade.SystemCtlStopDelivery(); err != nil {
			err = status.Wrap(err, status.ServiceUnloadError, `Failed to shut stop the systemd unit for Chef Automate v1. Check to make sure Chef Automate v1 is stopped and disabled`)
			d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
			return
		}

		if err := a1upgrade.SystemCtlDisableDelivery(); err != nil {
			err = status.Wrap(err, status.ServiceUnloadError, `Failed to disable the systemd unit for Chef Automate v1. Check to make sure Chef Automate v1 is stopped and disabled.`)
			d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
			return
		}
	}

	if d.mergedCfg.GetDeployment().GetV1().GetSvc().GetEnableChefServer().GetValue() {
		d.writer.Title("Shutting down Chef Server (omnibus) installation")
		if err := a1upgrade.ChefServerCtlStop(); err != nil {
			err = status.Wrap(err, status.ServiceUnloadError, `Failed to shut down Chef Server. This may indicate a process is simply taking longer than expected to shut down cleanly, or it may indicate a larger problem. You may investigate further by checking the Chef Server logs with the following command:

chef-server-ctl tail

When the chef-server-ctl status command indicates that all processes are stopped run the upgrade command again to continue.
`)
			d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
			return
		}

		if a1upgrade.SystemCtlIsEnabledChefServer() {
			if err := a1upgrade.SystemCtlStopChefServer(); err != nil {
				err = status.Wrap(err, status.ServiceUnloadError, `Failed to shut down the Chef Server. Check to make sure Chef Server is stopped and disabled.
`)
				d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
				return
			}

			if err := a1upgrade.SystemCtlDisableChefServer(); err != nil {
				err = status.Wrap(err, status.ServiceUnloadError, `Failed to disable the systemd unit for Chef Server. Check to make sure Chef Server is stopped and disabled.
`)
				d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
			}
		}
	}
}

func (d *deployer) printUpgradeHeader() {
	if d.err != nil {
		return
	}
	resp, err := d.client.ManifestVersion(d.ctx, &api.ManifestVersionRequest{})
	if err != nil {
		// NOTE(ssd) 2018-04-27: It may seem silly to fail
		// here, but this is the last step before the point of
		// no return, so if something is up with the
		// deployment-service, we definitely want to know.
		explanationFmt := `
An error occurred attempting to query Automate 2 for its current version.

This may indicate a problem with the new Automate 2 deployment
service. You can attempt the upgrade again with the following command

    %s --skip-preflight

If the problem persists, please contact support@chef.io to resolve the
issue. Include a copy of the output of this upgrade command and the
tar archive created by

    %s gather-logs --local-fallback
`
		explanation := fmt.Sprintf(explanationFmt, strings.Join(os.Args, " "), os.Args[0])
		err = status.Wrap(err, status.DeploymentServiceCallError, explanation)
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	a2Version := resp.BuildTimestamp
	a1Version, err := a1upgrade.VersionStringFromA1Manifest()
	if err != nil {
		// TODO(ssd) 2018-04-27: Should we just fail here?
		a1Version = "1"
	}

	d.writer.Titlef("Upgrading Chef Automate from %s to %s", a1Version, a2Version)
}

func (d *deployer) extractA1PostgreSQLData() {
	if d.err != nil {
		return
	}

	d.writer.Title("Exporting Chef Automate v1 PostgreSQL data")

	a1Config := d.upgrade.A1Config
	port, err := a1Config.DeliveryRunning.Delivery.PostgreSQL.Port.Int64()
	if err != nil {
		err = status.Wrap(err, status.ConfigError, "Failed parse PostgreSQL port from Automate 1 configuration\n")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	connInfo := &pg.A1ConnInfo{
		User: a1Config.DeliveryRunning.Delivery.PostgreSQL.SuperuserUsername,
		Pass: a1Config.DeliverySecrets.Postgresql.SuperuserPassword,
		Host: a1Config.DeliveryRunning.Delivery.PostgreSQL.Vip,
		Port: uint64(port),
	}

	if err := a1upgrade.BackupA1Postgres(connInfo, d.upgrade.Databases(), time.Duration(d.upgrade.PgDumpWait)*time.Second); err != nil {
		err = status.Wrap(err, status.BackupError, "Failed to backup postgres data from Chef Automate v1")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
	}
}

func (d *deployer) extractA1UserData() {
	if d.err != nil {
		return
	}

	d.writer.Title("Exporting Chef Automate v1 User Data")

	a1Config := d.upgrade.A1Config
	port, err := a1Config.DeliveryRunning.Delivery.PostgreSQL.Port.Int64()
	if err != nil {
		err = status.Wrap(err, status.ConfigError, "Failed to parse PostgreSQL port from Automate 1 configuration")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	connInfo := &pg.A1ConnInfo{
		User: a1Config.DeliveryRunning.Delivery.PostgreSQL.SuperuserUsername,
		Pass: a1Config.DeliverySecrets.Postgresql.SuperuserPassword,
		Host: a1Config.DeliveryRunning.Delivery.PostgreSQL.Vip,
		Port: uint64(port),
	}

	userDataJSON, err := a1upgrade.ExportUserData(connInfo, time.Duration(d.upgrade.PgDumpWait)*time.Second)
	if err != nil {
		err = status.Wrap(err, status.ConfigError, "Failed to export Chef Automate v1 user data")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	d.writeServiceData("local-user-service", "data/a1_user_data.json", userDataJSON)
}

func (d *deployer) extractA1UserRolesData() {
	if d.err != nil {
		return
	}

	d.writer.Title("Exporting Chef Automate v1 User Roles Data")

	a1Config := d.upgrade.A1Config
	port, err := a1Config.DeliveryRunning.Delivery.PostgreSQL.Port.Int64()
	if err != nil {
		err = status.Wrap(err, status.ConfigError, "Failed to parse PostgreSQL port from Automate 1 configuration")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	connInfo := &pg.A1ConnInfo{
		User: a1Config.DeliveryRunning.Delivery.PostgreSQL.SuperuserUsername,
		Pass: a1Config.DeliverySecrets.Postgresql.SuperuserPassword,
		Host: a1Config.DeliveryRunning.Delivery.PostgreSQL.Vip,
		Port: uint64(port),
	}

	userRolesDataJSON, err := a1upgrade.ExportUserRolesData(connInfo,
		time.Duration(d.upgrade.PgDumpWait)*time.Second)
	if err != nil {
		err = status.Wrap(err, status.ConfigError, "Failed to export Chef Automate v1 user role data")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	d.writeServiceData("local-user-service", "data/a1_user_roles_data.json", userRolesDataJSON)
}

func (d *deployer) extractA1ChefServerCredentials() {
	if d.err != nil {
		return
	}

	if !d.mergedCfg.GetDeployment().GetV1().GetSvc().GetEnableChefServer().GetValue() {
		return
	}

	d.writer.Title("Exporting Chef Server internal credentials")

	pivotalPrivData, err := a1upgrade.RetrievePivotalKey()
	if err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to retrieve Chef Server pivotal user private key")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	pivotalPubData, err := a1upgrade.PubKeyFromPriv(pivotalPrivData)
	if err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to retrieve Chef Server pivotal user public key")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	webuiPrivKeyData, err := a1upgrade.RetrieveWebuiPrivKey()
	if err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to retrieve Chef Server webui private key")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	webuiPubKeyData, err := a1upgrade.RetrieveWebuiPubKey()
	if err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to retrieve Chef Server webui public key")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	for path, data := range map[string]string{
		"data/pivotal.pem":     pivotalPrivData,
		"data/pivotal.pub.pem": pivotalPubData,
		"data/webui_priv.pem":  webuiPrivKeyData,
		"data/webui_pub.pem":   webuiPubKeyData,
	} {
		d.writeServiceData("automate-cs-oc-erchef", path, data)
	}
}

func (d *deployer) writeServiceData(serviceName, relPath, content string) {
	if d.err != nil {
		return
	}

	svcPath := path.Join("/hab/svc", serviceName)
	filePath := path.Join(svcPath, relPath)
	dataDir := path.Dir(filePath)
	err := os.MkdirAll(dataDir, os.ModePerm)
	if err != nil {
		err = status.Wrapf(err, status.FileAccessError, "Could not create service data directory %q", dataDir)
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	err = os.Chmod(dataDir, sys.HabDefaultDataDirPerms)
	if err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to alter hab service directory permissions")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	r := strings.NewReader(content)
	err = fileutils.AtomicWrite(filePath, r, fileutils.WithAtomicWriteFileMode(sys.DefaultFilePerms))
	if err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to write service data")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	u, err := user.Lookup("hab")
	if err != nil {
		err = status.Wrap(err, status.UnknownError, "Failed to lookup local 'hab' user")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	uid, err := strconv.Atoi(u.Uid)
	if err != nil {
		err = status.Wrap(err, status.UnknownError, "Failed to convert hab UID to integer")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	gid, err := strconv.Atoi(u.Gid)
	if err != nil {
		err = status.Wrap(err, status.UnknownError, "Failed to convert hab GID to integer")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	splitPath := strings.Split(relPath, "/")
	pathToChown := svcPath
	for _, p := range splitPath {
		pathToChown = path.Join(pathToChown, p)
		if err := os.Chown(pathToChown, uid, gid); err != nil {
			err = status.Wrapf(err, status.FileAccessError, "Failed to alter directory permissions for '%s", pathToChown)
			d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
			return
		}
	}
}

func (d *deployer) restorePostgresData() {
	if d.err != nil {
		return
	}

	d.writer.Title("Loading Chef Automate v1 PostgreSQL data to A2")

	// We currently expect that postgresql is local and do not
	// support non-local postgresql. Currently we don't have
	// anything like a VIP in the postgresql configuration.
	connInfo := &pg.A2ConnInfo{
		User:  d.mergedCfg.GetPostgresql().GetV1().GetSys().GetSuperuser().GetName().GetValue(),
		Port:  uint64(d.mergedCfg.GetPostgresql().GetV1().GetSys().GetService().GetPort().GetValue()),
		Host:  "localhost",
		Certs: pg.A2SuperuserCerts,
	}

	if err := a1upgrade.RestoreA1PostgresToA2(connInfo, d.upgrade.Databases(), time.Duration(d.upgrade.PgRestoreWait)*time.Second); err != nil {
		err = status.Wrap(err, status.DatabaseError, "Failed to restore Chef Automate v1 PostgreSQL data to A2")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
	}
}

func (d *deployer) moveA1ESData() {
	if d.err != nil {
		return
	}

	a1DataPath := d.upgrade.DeliveryRunning.Delivery.Insights.DataDirectory

	// check the insights data directory in the given A1 delivery
	// running config for the presence of a "chef-insights" subdirectory, which
	// will be present on Automate installs that first installed a version 0.8 or
	// older.
	//
	// For example, in Automate 0.8, the indices directory will be at the path:
	// /var/opt/delivery/elasticsearch/data/chef-insights/nodes/0/indices
	// Whereas, if Automate was first installed on version 1.x, the path would be:
	// /var/opt/delivery/elasticsearch/data/nodes/0/indices
	if _, err := os.Stat(filepath.Join(a1DataPath, "chef-insights")); err == nil {
		a1DataPath = filepath.Join(a1DataPath, "chef-insights")
	}

	d.writer.Title("Moving Chef Automate v1 Elasticsearch data for import to A2")
	m := a1upgrade.NewFileMover(a1DataPath, "automate-elasticsearch", "data")
	err := d.doMoveWithTimeout(m)
	if err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to move Chef Automate v1 Elasticsearch data to A2")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}
}

func (d *deployer) doMoveWithTimeout(m *a1upgrade.FileMover) error {
	m.Timeout = time.Duration(d.upgrade.FileMoveTimeout) * time.Second
	return m.Move(d.writer)
}

func (d *deployer) moveA1ComplianceData() {
	if d.err != nil {
		return
	}

	a1ProfilePath := d.upgrade.DeliveryRunning.Delivery.Compliance.ProfilesPath

	d.writer.Title("Moving Chef Automate v1 Compliance Profile data for import to A2")
	m := a1upgrade.NewFileMover(a1ProfilePath, "compliance-service", "data/profiles")
	if err := d.doMoveWithTimeout(m); err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to move Chef Automate v1 Compliance Profile data to A2")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
	}
}

func (d *deployer) moveA1WorkflowGitRepos() {
	if d.err != nil {
		return
	}
	a1RuleStore := d.upgrade.DeliveryRunning.Delivery.Delivery.GitRepos
	d.writer.Title("Moving Chef Automate v1 Workflow Git repositories for import to A2")
	m := a1upgrade.NewFileMover(a1RuleStore, "automate-workflow-server", "data/git/repos")
	if err := d.doMoveWithTimeout(m); err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to move Chef Automate v1 Workflow Git repositories to A2")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
	}
}

// writeA1WorkflowErlCookie writes the erlang cookie for the automate-workflow-server
func (d *deployer) writeA1WorkflowErlCookie() {
	if d.err != nil {
		return
	}

	a1ErlCookie := d.upgrade.DeliveryRunning.Delivery.Delivery.ErlCookie
	if a1ErlCookie == "" {
		d.writer.Title("Skipping import of Automate 1 Workflow Erlang Cookie")
		return
	}

	d.writeServiceData("automate-workflow-server", "var/.erlang.cookie", a1ErlCookie)
}

func (d *deployer) moveA1NotificationData() {
	if d.err != nil {
		return
	}
	a1RuleStore := d.upgrade.DeliveryRunning.Delivery.Notifications.RuleStore
	// Notifications now uses postgres, but should be able to read A1's rule
	// store files, so we move the file into the hab dir.
	d.writer.Title("Moving Chef Automate v1 Notification Rules data for import to A2")
	m := a1upgrade.NewFileMover(a1RuleStore, "notifications-service", "data/rule_store", a1upgrade.ForceCopy())
	if err := d.doMoveWithTimeout(m); err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to move Chef Automate v1 Notification Rules data to A2")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
	}
}

// writeA1ComplianceSecret writes the secret_key file used to encrypt
// secrets data in the database.  In A1 this was handled by the
// compliance service, in A2 this is now handled by the secrets-service.
func (d *deployer) writeA1ComplianceSecret() {
	if d.err != nil {
		return
	}

	if d.upgrade.DeliveryRunning.Delivery.Delivery.SecretsKey == "" {
		d.writer.Title("Skipping import of Automate 1 Compliance Secret")
		return
	}

	d.writer.Title("Importing Automate v1 Compliance Secrets into A2")
	d.writeServiceData("secrets-service", "data/secrets_key", d.upgrade.DeliveryRunning.Delivery.Delivery.SecretsKey)
}

func (d *deployer) cancelUpgradeInstructions() string {
	restartCmd := "automate-ctl start"
	if d.mergedCfg.GetDeployment().GetV1().GetSvc().GetEnableChefServer().GetValue() {
		restartCmd = fmt.Sprintf("%s\n%s start", restartCmd, a1upgrade.A1ChefServerCtlPath)
	}
	return fmt.Sprintf(cancelUpgradeInstructionsFmt, restartCmd)
}

func (d *deployer) failedPostBackupInstructions() string {
	builder := strings.Builder{}
	builder.WriteString(cancelUpgradeWithRestorePreamble)

	if d.upgrade.SkipBackup {
		builder.WriteString(consolationPrize)
		return builder.String()
	}

	// If delivery-running is incomplete somehow or there's a backup type we
	// didn't code for (unlikely), a1RestoreInstructions returns an error so we
	// fallback to a more general explanation of next steps
	restoreCmd, err2 := d.a1RestoreInstructions()
	if err2 != nil {
		fmt.Fprintf(&builder, howToRestoreFallback, d.a1BackupName())
		return builder.String()
	}

	fmt.Fprintf(&builder, howToRestore, restoreCmd)
	return builder.String()
}

func (d *deployer) deployDataServices() {
	if d.err != nil {
		return
	}

	d.writer.Title("Starting Data Services")
	resp, err := d.client.DeployDataServices(d.ctx, &api.DeployRequest{})
	if err != nil {
		err = status.Wrap(err, status.DeploymentServiceCallError, "Request to to start Chef Automate v2 Elasticsearch and Postgres data stores failed")
		d.err = status.WithRecovery(err, d.failedPostBackupInstructions())
		return
	}

	h := CLIEventWriter{Writer: d.writer}
	err = d.client.StreamDeployEvents(resp.TaskId, d.deploymentID, &h)
	if err != nil {
		err = status.Wrap(err, status.DeploymentServiceCallError, "Request to to start Chef Automate v2 Elasticsearch and Postgres data stores failed")
		d.err = status.WithRecovery(err, d.failedPostBackupInstructions())
		return
	}
}

func (d *deployer) startNonDataServices() {
	if d.err != nil {
		return
	}

	d.writer.Title("Starting remaining services and running stage 1 migrations")
	resp, err := d.client.StartNonDataServices(d.ctx, &api.DeployRequest{})
	if err != nil {
		err = status.Wrap(err, status.ServiceStartError, "Request to start Chef Automate v2 services failed. This may indicate that one or more services was unable to import your Chef Automate v1 Data")
		d.err = status.WithRecovery(err, d.failedPostBackupInstructions())
		return
	}

	h := CLIEventWriter{Writer: d.writer}
	err = d.client.StreamDeployEvents(resp.TaskId, d.deploymentID, &h)
	if err != nil {
		err = status.Wrap(err, status.ServiceStartError, "Request to start Chef Automate v2 services failed. This may indicate that one or more services was unable to import your Chef Automate v1 Data")
		d.err = status.WithRecovery(err, d.failedPostBackupInstructions())
		return
	}
}

const initialMigrationCompleteFmt = `Some data will be upgraded in the background. To monitor the status of the upgrade, run

  chef-automate migrate-from-v1-status

You can access Chef Automate using your existing credentials at

  %s

Enjoy Chef Automate 2.0 and remember, you'll never automate alone!
`
const letterBytes = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

type initialCreds struct {
	URL      string `toml:"url"`
	Username string `toml:"username"`
	Password string `toml:"password"`
}

func (d *deployer) saveDeploymentCreds() {
	if d.err != nil {
		return
	}

	// We don't need to write out any credentials if automate is not deployed.
	// As an example, if we've deployed only chef-server
	if !deployment.ContainsAutomateCollection(d.mergedCfg.GetDeployment()) && d.bootstrapBundlePath == "" {
		return
	}

	// Support legacy configuration. If an email is present that it will have
	// been used as the "username" when the admin was created. If it's not then
	// the "username" would have been used.
	username := d.mergedCfg.GetDeployment().GetV1().GetSvc().GetAdminUser().GetEmail().GetValue()
	if username == "" {
		username = d.mergedCfg.GetDeployment().GetV1().GetSvc().GetAdminUser().GetUsername().GetValue()
	}
	password := d.mergedCfg.GetDeployment().GetV1().GetSvc().GetAdminUser().GetPassword().GetValue()
	fqdn := d.mergedCfg.GetGlobal().GetV1().GetFqdn().GetValue()

	creds := initialCreds{URL: "https://" + fqdn, Username: username, Password: password}

	path, err := credentialsFilePath()
	if err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to open automate-credentials file")
		d.err = status.WithRecovery(err, recoverFromFailedCredentialSave)
		return
	}

	data, err := toml.Marshal(creds)
	if err != nil {
		err = status.Annotate(err, status.MarshalError)
		d.err = status.WithRecovery(err, recoverFromFailedCredentialSave)
		return
	}

	r := strings.NewReader(string(data))
	err = fileutils.AtomicWrite(path, r, fileutils.WithAtomicWriteFileMode(sys.DefaultFilePerms))
	if err != nil {
		err = status.Wrap(err, status.FileAccessError, "Failed to write credentials to file")
		d.err = status.WithRecovery(err, recoverFromFailedCredentialSave)
		return
	}

	fmtStr := "Your credentials have been saved to %s\nAccess the web UI at https://%s/\n\n"
	d.writer.Printf(fmtStr, path, fqdn)
}

func (d *deployer) showTelemetryNotice() {
	if d.err != nil {
		return
	}

	// Expecting telemetry opt_out to be present in the merged config.
	t := d.mergedCfg.GetLicenseControl().GetV1().GetSys().GetTelemetry().GetOptOut().GetValue()

	// telemetry OutOut defaults to false
	if !t {
		d.writer.Println(optedInToTelemetry)
	} else {
		d.writer.Println(pleaseOptInToTelemetry)
	}
}

func (d *deployer) initialMigrationComplete() {
	if d.err != nil {
		return
	}
	fqdn := d.mergedCfg.GetGlobal().GetV1().GetFqdn().GetValue()
	d.writer.Printf(initialMigrationCompleteFmt, "https://"+fqdn)
}

func credentialsFilePath() (shortname string, err error) {
	shortname = "automate-credentials.toml"
	for tries := 0; tries < 10; tries++ {
		// don't use declaration assignment for os.OpenFile to avoid shadowing err
		var f *os.File
		f, err = os.OpenFile(shortname, os.O_RDWR|os.O_CREATE|os.O_EXCL, 0600)
		defer f.Close()

		if err == nil {
			return shortname, nil
		}

		shortname = fmt.Sprintf("automate-credentials-%s.toml", randomTag())
	}

	return "", err
}

func randomTag() string {
	var s strings.Builder
	lettersLen := big.NewInt(int64(len(letterBytes) - 1))
	for i := 0; i < 8; i++ {
		idx, _ := rand.Int(rand.Reader, lettersLen)
		s.WriteByte(letterBytes[int(idx.Int64())])
	}
	return s.String()
}

func (d *deployer) deployAll() {
	if d.err != nil {
		return
	}

	d.writer.Title("Starting deploy")
	resp, err := d.client.Deploy(d.ctx, &api.DeployRequest{
		UsedBootstrapBundle: d.bootstrapBundlePath != "",
	})
	if err != nil {
		d.err = status.Annotate(err, status.DeployError)
		return
	}

	h := CLIEventWriter{Writer: d.writer}
	err = d.client.StreamDeployEvents(resp.TaskId, d.deploymentID, &h)
	if err != nil {
		d.err = status.Annotate(err, status.DeployError)
		return
	}

	d.writer.Title("Deploy Complete")
}

// validateUpgradeableA1Config checks to see whether an A1 installation can be automatically upgraded to A2
// It looks for A1 config that indicates use of A1 features that are
// 1. not currently supported in A2 (workflow, FIPS, DR), or
// 2. require manual configuration to work in A2 (elasticsearch, proxy), or.
// 3. are otherwise problematic (backups not configured)
func (d *deployer) validateUpgradableA1Config() {
	if d.upgrade.SkipUpgradePreflight {
		return
	}

	checker := a1upgrade.NewCompatChecker()

	d.writer.Title("Checking if your Chef Automate v1 installation uses features that are not compatible with Chef Automate v2...")

	u := d.upgrade
	skips := a1upgrade.CompatCheckerSkips{
		BackupCheck:           u.SkipBackupCheck,
		DisasterRecoveryCheck: u.SkipDisasterRecoveryCheck,
		ExternalESCheck:       u.SkipExternalESCheck,
		FIPSCheck:             u.SkipFIPSCheck,
		SAMLCheck:             u.SkipSAMLCheck,
		WorkflowCheck:         u.SkipWorkflowCheck,
	}

	// @afiune delete me when workflow feature is completed, as well as the skip flags
	if d.upgrade.EnableWorkflow {
		skips.SkipWorkflowCheck()
	}

	err := checker.RunAutomateChecks(u.A1Config, skips)
	if err != nil {
		err = status.Wrap(err, status.PreflightError, "Failed to determine if your Chef Automate v1 configuration is suitable for upgrade")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	rollupMsg := checker.Msgs.String()
	if checker.Failures == 0 && checker.Warnings == 0 {
		d.writer.Body("Your Chef Automate v1 config passed compatibility checks. Continuing with your upgrade to Chef Automate v2.")
	}

	if checker.Failures > 0 {
		sum := checker.Failures + checker.Warnings
		builder := strings.Builder{}
		fmt.Fprintf(&builder, "We found %d potential compatibility issue(s) between your Chef Automate v1 configuration with Chef Automate v2:\n", sum)
		fmt.Fprintf(&builder, "%s\n", rollupMsg)
		builder.WriteString("Please address these issues to continue with your upgrade to Chef Automate v2")
		d.err = status.WithRecovery(
			status.New(status.PreflightError, "Upgrade compatibility checks failed"),
			builder.String(),
		)
		return
	}

	if checker.Warnings > 0 {
		d.writer.Bodyf("We found %d potential compatibility issue(s) between your Chef Automate v1 configuration and Chef Automate v2:\n", checker.Warnings)
		d.writer.Bodyf("%s\n", rollupMsg)
		continueUpgrade, err := d.writer.Confirm("Do you wish to continue upgrading to Chef Automate v2?")
		if err != nil || !continueUpgrade {
			d.err = status.Annotate(err, status.InvalidCommandArgsError)
		}
	}
}

func (d *deployer) validateUpgradableChefServerConfig() {
	if d.upgrade.SkipUpgradePreflight {
		return
	}
	if !d.mergedCfg.GetDeployment().GetV1().GetSvc().GetEnableChefServer().GetValue() {
		return
	}
	checker := a1upgrade.NewCompatChecker()
	a1Config := d.upgrade.A1Config
	err := checker.RunChefServerChecks(a1Config)
	if err != nil {
		err = status.Wrap(err, status.PreflightError, "Failed to determine if your Chef Automate v1 configuration is suitable for upgrade")
		d.err = status.WithRecovery(err, d.cancelUpgradeInstructions())
		return
	}

	// NOTE: if you add a check that is a WARNING, you need to add the warning messaging code here

	if checker.Failures > 0 {
		rollupMsg := checker.Msgs.String()
		sum := checker.Failures + checker.Warnings
		builder := strings.Builder{}
		fmt.Fprintf(&builder, "We found %d issue(s) with your Chef Server configuration preventing it from being included in the Chef Automate upgrade:\n", sum)
		fmt.Fprintf(&builder, "%s\n", rollupMsg)
		builder.WriteString("Please address these issues to continue with your upgrade to Chef Automate v2")
		d.err = status.WithRecovery(
			status.New(status.PreflightError, "Upgrade compatibility checks failed"),
			builder.String(),
		)
	}
}
