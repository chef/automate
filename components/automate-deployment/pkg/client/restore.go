package client

import (
	"context"
	"fmt"
	"os"
	"path"
	"path/filepath"
	"time"

	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/preflight"

	"github.com/pkg/errors"

	dc "github.com/chef/automate/api/config/deployment"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/backup"
	"github.com/chef/automate/components/automate-deployment/pkg/bootstrap"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/platform/command"
)

const (
	deploymentServiceName = "deployment-service"
)

// RestoreBackup makes a gRPC request to the deployment service to start a
// new backup restore routine. The server returns a restore ID which can be used
// to stream backup events.
func RestoreBackup(conTimeout, reqTimeout time.Duration, restoreTask *api.BackupRestoreTask) (*api.RestoreBackupResponse, error) {
	con, ctx, cancel, err := newCon(conTimeout, reqTimeout)
	defer cancel()
	if err != nil {
		return &api.RestoreBackupResponse{}, status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connecting to deployment-service failed",
		)
	}

	res, err := con.RestoreBackup(
		ctx,
		&api.RestoreBackupRequest{
			Restore: restoreTask,
		},
	)

	if err != nil {
		err = status.Wrapf(
			err,
			status.DeploymentServiceCallError,
			"Request to restore backup %s failed",
			restoreTask.GetBackup().TaskID(),
		)
	}

	return res, err
}

// DeploymentRestore represents a local deployment-service restoration.
type DeploymentRestore struct {
	restoreTask             *api.BackupRestoreTask
	skipPreflight           bool
	skipBootrap             bool
	target                  target.Target
	writer                  cli.FormatWriter
	metadata                *backup.Metadata
	a2Manifest              manifest.ReleaseManifest
	service                 *deployment.Service
	mergedCfg               *dc.AutomateConfig
	airgapInstallBundlePath string
}

// DeploymentRestoreOpt represents an optional configuration function for a
// DeploymentRestore.
type DeploymentRestoreOpt func(*DeploymentRestore)

// NewDeploymentRestore returns a new instance of DeploymentRestore
func NewDeploymentRestore(opts ...DeploymentRestoreOpt) *DeploymentRestore {
	restore := &DeploymentRestore{
		skipPreflight: false,
	}

	for _, opt := range opts {
		opt(restore)
	}

	offlineMode := restore.airgapInstallBundlePath != ""
	restore.target = target.NewLocalTarget(offlineMode)

	return restore
}

// WithDeploymentRestoreTask sets restore task for the deployment restore
func WithDeploymentRestoreTask(task *api.BackupRestoreTask) DeploymentRestoreOpt {
	return func(restore *DeploymentRestore) {
		restore.restoreTask = task
	}
}

// WithDeploymentRestoreSkipPreflight determines whether or not to skip preflight
// checks when restoring a backup.
func WithDeploymentRestoreSkipPreflight(skip bool) DeploymentRestoreOpt {
	return func(restore *DeploymentRestore) {
		restore.skipPreflight = skip
	}
}

// WithDeploymentRestoreSkipBootstrap determines whether or not to skip bootstrapping
// Habitat from the manifest during a restore.
func WithDeploymentRestoreSkipBootstrap(skip bool) DeploymentRestoreOpt {
	return func(restore *DeploymentRestore) {
		restore.skipBootrap = skip
	}
}

// WithDeploymentRestoreWriter sets the writer to use for CLI I/O
func WithDeploymentRestoreWriter(writer cli.FormatWriter) DeploymentRestoreOpt {
	return func(restore *DeploymentRestore) {
		restore.writer = writer
	}
}

func WithDeploymentRestoreAirgapInstallBundle(airgapInstallBundlePath string) DeploymentRestoreOpt {
	return func(restore *DeploymentRestore) {
		restore.airgapInstallBundlePath = airgapInstallBundlePath
	}
}

func (r *DeploymentRestore) loadDeploymentConfig(bucket backup.Bucket) error {
	c, err := backup.LoadDeploymentConfig(bucket, r.metadata.Verifier())
	if err != nil {
		return wrapBackupOrChecksumErr(err, "Loading deployment configuration from the backup directory failed")
	}

	r.mergedCfg = c
	return nil
}

func (r *DeploymentRestore) loadMetadata(bucket backup.Bucket) error {
	verifier, err := backup.LoadMetadataVerifier(bucket, r.restoreTask.Sha256)
	if err != nil {
		return wrapBackupOrChecksumErr(err, "Loading backup metadata checksums from the backup directory failed")
	}

	metadata, err := backup.LoadServiceMetadata(
		bucket,
		deploymentServiceName,
		verifier,
	)
	if err != nil {
		return wrapBackupOrChecksumErr(err, "Loading service metadata from the backup directory failed")
	}

	r.metadata = metadata

	return nil
}

func (r *DeploymentRestore) loadManifest(bucket backup.Bucket) error {
	man, err := backup.LoadBackupManifest(bucket, r.restoreTask)

	if err != nil {
		return wrapBackupOrChecksumErr(err, "Loading backup manifest from the backup directory failed")
	}

	r.a2Manifest = man
	return nil
}

func (r *DeploymentRestore) loadService() error {
	svc := deployment.ServiceFromManifest(r.a2Manifest, deploymentServiceName)

	if svc == nil {
		return status.New(
			status.BackupRestoreError,
			"Restoring deployment-service from backup package manifest failed",
		)
	}

	r.service = svc
	return nil
}

func (r *DeploymentRestore) stopDeploymentService() error {
	if err := r.target.UnloadService(r.service); err != nil {
		return status.Wrap(
			err,
			status.ServiceUnloadError,
			"Stopping the deployment-service prior to restoration failed",
		)
	}

	startTime := time.Now()
	timeout := 10 * time.Second
	sleepTime := 500 * time.Millisecond
	ctx := context.Background()

	// Wait for Habitat to "unload" the process
	for {
		deployed, err := r.target.DeployedServices(ctx)
		if err == nil {
			alive := false
			for _, svc := range deployed {
				if svc.Pkg.Name() == deploymentServiceName {
					alive = true
				}
			}

			if !alive {
				// There's a small delay between hab "unloading" the service and terminating
				// it's PID. To ensure that it's dead enough we'll ping the service until
				// the connection fails.
				con, err := Connection(500 * time.Millisecond)
				// If we can't connect to the service we can assume it's down
				if err != nil {
					break
				}

				// Ping the service. If it fails we can assume the service isn't
				// accepting new request.
				_, err = con.Ping(ctx, &api.PingRequest{})

				if err != nil {
					break
				}
			}

			if time.Since(startTime) > timeout {
				return errors.New("Timed out waiting for deployment-service to shut down")
			}

			time.Sleep(sleepTime)
		}

	}

	return nil
}

func (r *DeploymentRestore) serviceRelease() (string, error) {
	release := ""
	con, err := Connection(1 * time.Second)
	if err != nil {
		return release, status.Wrap(
			err,
			status.DeploymentServiceUnreachableError,
			"Connecting to the deployment-service failed",
		)
	}

	svcVers, err := con.ServiceVersions(context.Background(), &api.ServiceVersionsRequest{})
	if err != nil {
		return release, status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request for service version information failed",
		)
	}

	for _, svcV := range svcVers.GetServices() {
		if svcV.Name == deploymentServiceName {
			release = svcV.Release
		}
	}

	return release, nil
}

func (r *DeploymentRestore) startDeploymentService() error {
	if err := r.target.LoadService(r.service); err != nil {
		return status.Wrap(
			err,
			status.ServiceStartError,
			"failed to load deployment-service on target",
		)
	}

	var (
		startTime  = time.Now()
		timeout    = 20 * time.Second
		sleepTime  = 500 * time.Millisecond
		desiredRel = deployment.ServiceFromManifest(r.a2Manifest, deploymentServiceName).Release()
	)

	for {
		// NOTE 2019-02-20: We assume that if the
		// deployment-service is responding and returning the
		// correct version than it is up. We don't currently
		// depend on the Habitat health status here because
		// that status can be delayed for up to 30 seconds if
		// the first health check happens to fail.
		currentRel, err := r.serviceRelease()
		if err != nil {
			currentRel = "unknown" // make the error message pretty
		}

		if currentRel == desiredRel {
			return nil
		}

		if time.Since(startTime) > timeout {
			msg := `
Timed out waiting for deployment-service to start.
    Expected deployment-service release: %s
       Found deployment-service release: %s`
			return status.Errorf(status.TimedOutError, msg, desiredRel, currentRel)
		}

		Disconnect()
		time.Sleep(sleepTime)
	}
}

func (r *DeploymentRestore) writeConvergeDisableFile() error {
	err := os.MkdirAll(filepath.Dir(api.ConvergeDisableFilePath), 0755)
	if err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			"Creating the converge disable file destination directory failed",
		)
	}

	// Remove read and execute from other
	err = os.Chmod(filepath.Dir(api.ConvergeDisableFilePath), 0750)
	if err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			"Setting the converge disable file destination directory permissions failed",
		)
	}

	_, err = os.OpenFile(api.ConvergeDisableFilePath, os.O_RDONLY|os.O_CREATE, 0700)
	if err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			"Creating the converge disable file failed",
		)
	}

	return nil
}

func (r *DeploymentRestore) installAirgapBundle() error {
	metadata, err := airgap.Unpack(r.airgapInstallBundlePath)
	if err != nil {
		return status.Wrap(err, status.AirgapUnpackInstallBundleError, "Failed to unpack airgap install bundle")
	}
	// We need to set the path for the hab binary so that the deployer does not
	// try to go to the internet to get it
	pathEnv := os.Getenv("PATH")

	err = os.Setenv("PATH", fmt.Sprintf("%s:%s", path.Dir(metadata.HabBinPath), pathEnv))
	if err != nil {
		return errors.Wrap(err, "Failed to add hab binary to the path")
	}
	return nil
}

// Restore bootstraps the deployment-service and restores the backup
func (r *DeploymentRestore) Restore() error {
	preflightCheckOpts := preflight.DeployPreflightCheckOptions{
		SkipA2DeployedCheck: true,
	}
	// Run preflight checks
	if !r.skipPreflight {
		r.writer.Title("Beginning pre-flight checks")
		if r.restoreTask.Airgap {
			preflightCheckOpts.Airgap = true
		}
		out, err := preflight.RunDeployPreflightCheck(preflightCheckOpts)
		if err != nil {
			return status.Wrap(
				err,
				status.PreflightError,
				"Running the preflight checks failed\n",
			)
		}
		r.writer.Title(out)
	}

	// Initialize bucket. Its view should only be from the backup task id in question
	locationSpec := backup.NewRemoteLocationSpecificationFromRestoreTask(r.restoreTask)
	bucket := locationSpec.ToBucket(r.restoreTask.Backup.TaskID())

	// Write the converge loop disable sentinel file so we don't try and
	// run a converge before we've restored data and services.
	if err := r.writeConvergeDisableFile(); err != nil {
		return err
	}

	if r.restoreTask.Airgap {
		// It's important that we disable converging before continuing. Otherwise,
		// we might end up executing the manifest from the airgap install bundle
		r.writer.Title("Installing from airgap bundle file")
		if err := r.installAirgapBundle(); err != nil {
			return err
		}
	}

	if err := r.loadMetadata(bucket); err != nil {
		return err
	}

	if err := r.loadManifest(bucket); err != nil {
		return err
	}

	if err := r.loadDeploymentConfig(bucket); err != nil {
		return err
	}

	SetProxyEnvironment(r.mergedCfg.Deployment)

	b := bootstrap.NewCompatBootstrapper(r.target)
	// Set up Habitat
	if !r.skipBootrap {
		r.writer.Title("Bootstrapping Chef Automate")
		r.writer.Body("Installing Habitat")
		err := b.InstallHabitat(r.a2Manifest, r.writer)
		if err != nil {
			return status.Wrap(
				err,
				status.ServiceStartError,
				"Bootstrapping habitat failed",
			)
		}

		err = b.SetupSupervisor(r.mergedCfg.Deployment, r.a2Manifest, r.writer)
		if err != nil {
			return status.Wrap(
				err,
				status.ServiceStartError,
				"Failed to install Chef Automate service",
			)
		}
	}

	if err := r.loadService(); err != nil {
		return err
	}

	if err := r.stopDeploymentService(); err != nil {
		return err
	}

	// We don't actually care about the operation events because we'll get
	// any error that might occur later. But since the executor needs an
	// event channel we'll buffer them and close them.
	eventChan := make(chan api.DeployEvent_Backup_Operation, 30)
	errChan := make(chan error, 1)
	defer close(errChan)
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	restoreCtx := backup.NewContext(
		backup.WithContextCtx(ctx),
		backup.WithContextBackupRestoreTask(r.restoreTask),
		backup.WithContextBackupLocationSpecification(locationSpec),
		backup.WithContextReleaseManifest(r.a2Manifest),
	)

	// Make the spec actually execute commands
	spec := *r.metadata.Spec
	backup.SetCommandExecutor(spec, command.NewExecExecutor())

	executor := backup.NewExecutor(
		backup.WithEventChan(eventChan),
		backup.WithErrorChan(errChan),
		backup.WithSpec(spec),
		backup.WithCancel(cancel),
	)

	r.writer.Title("Restoring deployment-service")
	if err := r.stopDeploymentService(); err != nil {
		return err
	}

	r.writer.Body("Restoring deployment-service configuration")
	if err := executor.RestoreSyncPaths(restoreCtx, r.metadata); err != nil {
		return wrapBackupOrChecksumErr(err, "Restoring deployment-service path data failed")
	}

	r.writer.Body("Installing deployment-service")
	if err := b.InstallDeploymentService(r.mergedCfg.Deployment, r.a2Manifest); err != nil {
		return status.Wrap(
			err,
			status.PackageInstallError,
			"Installing deployment-service failed",
		)
	}

	r.writer.Body("Restoring deployment-service database")
	if err := executor.RestoreSyncCmds(restoreCtx, r.metadata); err != nil {
		return wrapBackupOrChecksumErr(err, "Restoring deployment-service database failed")
	}

	r.writer.Body("Starting deployment-service")
	if err := r.startDeploymentService(); err != nil {
		return err
	}

	return nil
}

func wrapBackupOrChecksumErr(err error, message string) error {
	cause := status.Cause(err)
	if backup.IsSnapshotChecksumError(cause) {
		// Added context from wraps tends to be irrelevant for this error so remove it.
		return status.Annotate(cause, status.SnapshotChecksumMismatchError)
	}
	return status.Wrap(
		err,
		status.BackupRestoreError,
		message,
	)
}
