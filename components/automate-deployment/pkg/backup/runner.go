package backup

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"strings"
	"sync"
	"time"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/grpc/secureconn"

	"github.com/golang/protobuf/ptypes"
	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/events"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	es "github.com/chef/automate/components/es-sidecar-service/pkg/elastic"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/platform/pg"
)

// Runner represents a new backup runner
type Runner struct {
	eventSender      events.EventSender
	specs            []Spec
	eventChan        chan api.DeployEvent_Backup_Operation
	eventTerm        chan struct{}
	errChan          chan error
	lockedDeployment *deployment.Deployment
	target           target.Target
	failed           bool

	taskOperationsMutex *sync.Mutex
	// map[task-id][service-name] = operation-status
	TaskOperations map[string]map[string]api.DeployEvent_Backup_Operation
	// map[task-id] = cancel()

	// Since we rely on a deployment lock for operations that change state
	// we only need to track a single task and don't need another mutex.
	runningTask *CancellableTask

	// Used to save the backup manifest at the end of the restore
	deploymentStore persistence.DeploymentStore

	pgConnInfo      pg.ConnInfo
	esSidecarInfo   ESSidecarConnInfo
	connFactory     *secureconn.Factory
	releaseManifest manifest.ReleaseManifest
	configRenderer  func(*deployment.Service) (string, error)

	locationSpec             LocationSpecification // backup-gateway
	restoreLocationSpec      LocationSpecification // filesystem or S3
	builderMinioLocationSpec LocationSpecification // automate-minioew
	backupTask               *api.BackupTask
	restoreTask              *api.BackupRestoreTask
}

// RunnerOpt represents an optional configuration function for a Runner
type RunnerOpt func(*Runner)

// NewRunner returns a new instance of a backup job runner
func NewRunner(opts ...RunnerOpt) *Runner {
	runner := &Runner{
		TaskOperations:      map[string]map[string]api.DeployEvent_Backup_Operation{},
		taskOperationsMutex: &sync.Mutex{},
		configRenderer: func(*deployment.Service) (string, error) {
			return "", errors.New("invalid config renderer")
		},
	}

	for _, opt := range opts {
		opt(runner)
	}

	return runner
}

// Configure updates the instance with RunnerOpt's
func (r *Runner) Configure(opts ...RunnerOpt) *Runner {
	if r == nil {
		return nil
	}

	for _, opt := range opts {
		opt(r)
	}

	return r
}

// WithConfigRenderer configures the runner to unlock the given deployment.
func WithConfigRenderer(f func(*deployment.Service) (string, error)) RunnerOpt {
	return func(runner *Runner) {
		runner.configRenderer = f
	}
}

// WithSpecs configures the backup specifications to run
func WithSpecs(specs []Spec) RunnerOpt {
	return func(runner *Runner) {
		runner.specs = specs
	}
}

// WithBackupLocationSpecification sets the backup-gateway location
func WithBackupLocationSpecification(locationSpec LocationSpecification) RunnerOpt {
	return func(runner *Runner) {
		runner.locationSpec = locationSpec
	}
}

func WithBuilderMinioLocationSpec(locationSpec LocationSpecification) RunnerOpt {
	return func(runner *Runner) {
		runner.builderMinioLocationSpec = locationSpec
	}
}
func WithPGConnInfo(pgConnInfo pg.ConnInfo) RunnerOpt {
	return func(runner *Runner) {
		runner.pgConnInfo = pgConnInfo
	}
}

// WithConnFactory configures the secureconn.Factory to use to connect to services
func WithConnFactory(connFactory *secureconn.Factory) RunnerOpt {
	return func(ctx *Runner) {
		ctx.connFactory = connFactory
	}
}

// WithEsSidecarInfo configures the EsSidecar connection info
func WithEsSidecarInfo(esSidecarInfo ESSidecarConnInfo) RunnerOpt {
	return func(ctx *Runner) {
		ctx.esSidecarInfo = esSidecarInfo
	}
}

// WithTarget sets the runners deployment target
func WithTarget(target target.Target) RunnerOpt {
	return func(runner *Runner) {
		runner.target = target
	}
}

// WithReleaseManifest sets the release manifest for the runner
func WithReleaseManifest(releaseManifest manifest.ReleaseManifest) RunnerOpt {
	return func(runner *Runner) {
		runner.releaseManifest = releaseManifest
	}
}

// WithDeploymentStore sets the deployment store for the runner
func WithDeploymentStore(deploymentStore persistence.DeploymentStore) RunnerOpt {
	return func(runner *Runner) {
		runner.deploymentStore = deploymentStore
	}
}

// CreateBackup creates an Automate Backup
func (r *Runner) CreateBackup(ctx context.Context, dep *deployment.Deployment, sender events.EventSender) (*api.BackupTask, error) {
	r.backupTask = &api.BackupTask{Id: ptypes.TimestampNow()}

	r.infof("Backup running")
	r.eventSender = sender
	r.lockedDeployment = dep
	r.publishBackupEvent(r.backupTask.TaskID(), api.DeployEvent_RUNNING)

	go r.startBackupOperations(ctx)

	return r.backupTask, nil
}

func (r *Runner) BackupIntegrityShow(ctx context.Context, req *api.BackupIntegrityShowRequest) ([]*api.SnapshotIntegrity, error) {
	r.infof("Showing backup integrity")

	repo := NewArtifactRepo(r.locationSpec)

	metadata, err := repo.ReadSnapshotIntegrityMetadata(ctx)
	if err != nil {
		return nil, errors.Wrap(err, "reading snapshot integrity metadata file")
	}

	return r.repoIntegrityMetadataToSnapshotIntegritySlice(metadata)
}

func (r *Runner) ValidateBackupIntegrity(ctx context.Context, dep *deployment.Deployment, backupTasks []*api.BackupTask) ([]*api.SnapshotIntegrity, error) {
	r.infof("Validating Backup Integrity")

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	var err error

	r.lockedDeployment = dep
	defer r.unlockDeployment()

	opts := []FilterSnapshotOpt{}
	snapshots := []string{}

	if len(backupTasks) > 0 {
		for _, task := range backupTasks {
			snapshots = append(snapshots, task.TaskID())
		}

		opts = append(opts, OnlySnapshots(snapshots))
	}

	r.runningTask, err = newCancellableTask(api.BackupStatusResponse_VERIFY_INTEGRITY, snapshots, cancel)
	if err != nil {
		return nil, errors.Wrap(err, "configuring operation cancellation")
	}
	defer r.clearRunningTask()

	logrus.WithField("snapshots", snapshots).Debug("Starting integrity verifier")
	repo := NewArtifactRepo(r.locationSpec)

	if err = repo.ValidateSnapshotIntegrity(ctx, opts...); err != nil {
		return nil, errors.Wrap(err, "validating snapshot integrity")
	}

	metadata, err := repo.ReadSnapshotIntegrityMetadata(ctx)
	if err != nil {
		return nil, errors.Wrap(err, "reading snapshot integrity metadata file")
	}

	return r.repoIntegrityMetadataToSnapshotIntegritySlice(metadata)
}

func (r *Runner) repoIntegrityMetadataToSnapshotIntegritySlice(metadata *ArtifactRepoIntegrityMetadata) ([]*api.SnapshotIntegrity, error) {
	snapshots := []*api.SnapshotIntegrity{}
	for snapshot, metadata := range metadata.Snapshots {
		idt, err := time.Parse(api.BackupTaskFormat, snapshot)
		if err != nil {
			return nil, errors.Wrapf(err, "parsing snapshot '%s' id", snapshot)
		}
		id, err := ptypes.TimestampProto(idt)
		if err != nil {
			return nil, errors.Wrapf(err, "parsing snapshot '%s' id", snapshot)
		}

		lvt, err := time.Parse(api.BackupTaskFormat, metadata.LastVerified)
		if err != nil {
			return nil, errors.Wrapf(err, "parsing snapshot '%s' last verified time", snapshot)
		}
		lv, err := ptypes.TimestampProto(lvt)
		if err != nil {
			return nil, errors.Wrapf(err, "parsing snapshot '%s' last verified time", snapshot)
		}

		snapshots = append(snapshots, &api.SnapshotIntegrity{
			Id:           id,
			LastVerified: lv,
			Missing:      metadata.Missing,
			Corrupted:    metadata.Corrupted,
		})
	}

	return snapshots, nil
}

// DeleteBackups deletes one or many Automate Backups
func (r *Runner) DeleteBackups(ctx context.Context, dep *deployment.Deployment, backupTasks []*api.BackupTask) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	var err error

	r.lockedDeployment = dep
	defer r.unlockDeployment()

	taskIDs := []string{}
	for _, t := range backupTasks {
		taskIDs = append(taskIDs, t.TaskID())
	}
	r.runningTask, err = newCancellableTask(api.BackupStatusResponse_DELETE, taskIDs, cancel)
	if err != nil {
		return errors.Wrap(err, "Failed to configure operation cancellation")
	}
	defer r.clearRunningTask()

	// Build a context that we can pass to each operation
	for _, backupTask := range backupTasks {
		logrus.Infof("Delete backup %s", backupTask.TaskID())

		r.eventTerm = make(chan struct{})

		deleteCtx := NewContext(
			WithContextCtx(ctx),
			WithContextBackupTask(backupTask),
			WithContextBackupLocationSpecification(r.locationSpec),
			WithContextPgConnInfo(r.pgConnInfo),
			WithContextEsSidecarInfo(r.esSidecarInfo),
			WithContextConnFactory(r.connFactory),
			WithContextReleaseManifest(r.releaseManifest),
			WithContextBuilderMinioLocationSpec(r.builderMinioLocationSpec),
		)

		// don't verify the contents of the backup we are just going to delete
		// anyway
		verifier := NoOpObjectVerifier{}

		specs, specless, err := LoadAllSpecsFromBackup(ctx, deleteCtx.bucket, &verifier)
		if err != nil {
			return errors.Wrap(err, "Failed to load specs")
		}

		if len(specs) == 0 {
			if len(specless) == 0 {
				logrus.Warnf("Backup %s does not exist", backupTask.TaskID())
				err := deleteCtx.DeleteBackupMetadata()
				if err != nil {
					logrus.WithError(err).Error("Could not clean up backup metadata")
					return errors.Wrapf(err, "Could not clean up backup metadata for %s", backupTask.TaskID())
				}
			}

			logrus.Warnf("No specs for %s. Trying to cleanup", backupTask.TaskID())
		}

		err = deleteCtx.TransactDelete(func(deleteCtx Context) error {
			for _, spec := range specs {
				executor := NewExecutor(
					WithSpec(spec),
					WithCancel(cancel),
				)
				if err := executor.DeleteBackup(deleteCtx); err != nil {
					return err
				}
			}
			// Delete objects for services without specs
			if len(specless) > 0 {
				logrus.Warnf("Found services with missing specs: %v", specless)
				for _, service := range specless {
					objs, _, err := deleteCtx.bucket.List(ctx, service, false)
					if err != nil {
						return errors.Wrap(err, "Failed to list bucket")
					}
					objPaths := ToObjectPaths(objs)
					logrus.Warnf("Deleting extra backup objects found for %s: %v", service, objPaths)
					if err := deleteCtx.bucket.Delete(ctx, objPaths); err != nil && !IsNotExist(err) {
						return errors.Wrapf(err, "Failed to delete backup objects: %v", objPaths)
					}
				}
			}

			return nil
		})

		if err != nil {
			return errors.Wrapf(err, "failed to delete backup %s", backupTask.TaskID())
		}

		// Retry deleting remaining objects until the timeout expires.
		go func() {
			for {
				if ctx.Err() != nil {
					logrus.WithError(err).Errorf("Giving up deleting backup %s", backupTask.TaskID())
					return
				}
				// If any objects remain, something bad has happened
				remainingObjs, _, err := deleteCtx.bucket.List(ctx, "", false)
				if err != nil {
					logrus.WithError(err).Warn("Failed to get remaining objects")
					time.Sleep(5 * time.Second)
					continue
				}

				if len(remainingObjs) > 0 {
					err := errors.Errorf("Failed to delete backup %s. The following files remain: %v", backupTask.TaskID(), remainingObjs)
					logrus.WithError(err).Error("Backup delete failed")
					time.Sleep(5 * time.Second)
					continue
				}

				close(r.eventTerm)
				return
			}
		}()

		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-r.eventTerm:
		}
	}

	return nil
}

func (r *Runner) backupTaskFromSharedPrefix(sharedPrefix string) *api.BackupTask {
	ts0, err := time.Parse(api.BackupTaskFormat, sharedPrefix)
	if err != nil {
		r.warnf(err, "skipping unrecognized key '%s' in configured backup location", sharedPrefix)
		return nil
	}

	ts1, err := ptypes.TimestampProto(ts0)
	if err != nil {
		r.warnf(err, "skipping '%s' that could not be converted to BackupTask ID", sharedPrefix)
		return nil
	}
	return &api.BackupTask{
		Id: ts1,
	}
}

// ListBackups lists all of the available backups
func (r *Runner) ListBackups(ctx context.Context) ([]*api.BackupTask, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	bucket := r.locationSpec.ToBucket("")
	tasks := []*api.BackupTask{}

	// TODO: this is not very efficient. We don't want to scan all
	// the keys. We can make this faster by searching for .incomplete/20
	// and 20 key prefixes. bucket.List currently assumes the key prefix
	// it's searching for ends in '/'
	_, sharedPrefixes, err := bucket.List(ctx, "", true)
	if err != nil {
		return nil, err
	}

	for _, prefix := range sharedPrefixes {
		prefixStr := string(prefix)
		if prefixStr[len(prefixStr)-1] == '/' {
			prefixStr = prefixStr[0 : len(prefixStr)-1]
		}

		// We ignore .incomplete, automate-elasticsearch-data, and .tmp (minioMetaBucket)
		// We no longer need to look in .incomplete as we'll use a status file
		// to keep track of backups
		if prefixStr != ".incomplete" && prefixStr != es.RepoBaseName && prefixStr != ".tmp" && prefixStr != "shared" {
			t := r.backupTaskFromSharedPrefix(prefixStr)
			state := api.BackupTask_IN_PROGRESS // nolint:ineffassign

			reader, err := bucket.NewReader(ctx, path.Join(prefixStr, ".status"), &NoOpObjectVerifier{})
			if err != nil {
				if IsNotExist(err) {
					state = api.BackupTask_COMPLETED
				} else {
					return nil, err
				}
			} else {
				defer reader.Close()
				status, err := ioutil.ReadAll(reader)
				if err != nil {
					return nil, err
				}

				statusStr := string(status)
				if strings.HasPrefix(statusStr, string(BackupStatusFailed)) {
					state = api.BackupTask_FAILED
				} else if strings.HasPrefix(statusStr, string(BackupStatusInProgress)) {
					state = api.BackupTask_IN_PROGRESS
				} else if strings.HasPrefix(statusStr, string(BackupStatusDeleting)) {
					state = api.BackupTask_DELETING
				} else {
					state = api.BackupTask_COMPLETED
				}
			}
			if t != nil {
				t.State = state
				tasks = append(tasks, t)
			}
		}
	}
	return tasks, nil
}

func (r *Runner) ShowBackup(ctx context.Context, t *api.BackupTask) (*api.BackupDescription, error) {
	bucket := r.locationSpec.ToBucket(t.TaskID())
	sha256, err := ShowBackupChecksum(bucket)
	if err != nil {
		return nil, err
	}

	manifestLocation, err := NewBucketManifestLocation(ctx, bucket)
	if err != nil {
		return nil, err
	}
	// The string in the second arg does not matter since we are using the in-memory
	// provider.
	m, err := manifestLocation.Provider().GetCurrentManifest(ctx, "")
	if err != nil {
		return nil, err
	}

	var cliRelease string
	for _, value := range m.Packages {
		if value.Name() != "automate-cli" {
			continue
		} else {
			cliRelease = value.Release()
			break
		}
	}

	desc := &api.BackupDescription{
		Id:            t.TaskID(),
		Sha256:        sha256,
		ServerVersion: m.Build,
		CliVersion:    cliRelease,
	}
	return desc, nil
}

// RunningTask returns the currently running backup task
func (r *Runner) RunningTask(ctx context.Context) *CancellableTask {
	if r.runningTask == nil {
		r.clearRunningTask()
	}

	return r.runningTask
}

// Cancel cancels the currently running backup operation. If the current operation
// is IDLE it will return an error.
func (r *Runner) Cancel(ctx context.Context) error {
	task := r.RunningTask(ctx)
	if task == nil {
		return errors.New("no configured task")
	}

	if task.Status.GetOpType() == api.BackupStatusResponse_IDLE {
		return errors.New("unable to cancel backup operation because runner is idle")
	}

	task.Cancel()
	r.clearRunningTask()

	return nil
}

// RestoreBackup starts a backup restoration in a go routine and returns the
// task.
func (r *Runner) RestoreBackup(
	ctx context.Context,
	dep *deployment.Deployment,
	sender events.EventSender,
	bgw LocationSpecification,
	remote LocationSpecification,
	rt *api.BackupRestoreTask) (*api.BackupRestoreTask, error) {

	r.lockedDeployment = dep
	r.eventSender = sender
	r.locationSpec = bgw
	r.restoreLocationSpec = remote
	r.restoreTask = rt

	r.infof("Backup restore running")
	r.publishBackupEvent(r.restoreTask.TaskID(), api.DeployEvent_RUNNING)

	go r.startRestoreOperations(ctx)

	return r.restoreTask, nil
}

// startRestoreOperations starts unloads services, loads the backed up package
// manifest and builds a slice of topologically sorted services that are to be
// restored. By restoring them in this order we ensure that any dependent
// services are operational for each service as it is restored.
func (r *Runner) startRestoreOperations(ctx context.Context) {
	var err error
	deadline, ok := ctx.Deadline()
	if !ok {
		deadline = time.Now().Add(2 * time.Hour)
	}
	ctx, cancel := context.WithDeadline(context.Background(), deadline)

	r.eventChan = make(chan api.DeployEvent_Backup_Operation)
	r.eventTerm = make(chan struct{})
	r.errChan = make(chan error, 1)
	defer func() {
		cancel()

		// Make sure we re-enable the converge loop if the restoration was
		// successful. If the restore failed we want to leave the file in
		// place so that we can troubleshoot the issue and/or restore again
		// without the converge loop starting all the services. If the services
		// are started immediately following a restoration they will start up
		// in their default mode without data leading to confusion and a
		// partially restored cluster.
		if !r.failed {
			os.Remove(api.ConvergeDisableFilePath)
		}

		// Make sure that the event publisher always exits, we unlock the deployment,
		// and that any listeners on the event stream will get the task complete
		// signal.
		if r.eventSender != nil {
			r.eventSender.TaskComplete()
		}
		close(r.errChan)
		close(r.eventTerm)
		r.clearRunningTask()
		r.unlockDeployment()
	}()

	r.runningTask, err = newCancellableTask(api.BackupStatusResponse_RESTORE, []string{r.restoreTask.TaskID()}, cancel)
	if err != nil {
		r.failf(err, "Failed to configure operation cancellation")
		return
	}

	// Start the event publisher
	r.startRestoreEventPublisher()
	r.publishBackupEvent(r.restoreTask.TaskID(), api.DeployEvent_RUNNING)

	// Populate our expected services from our persisted manifest
	if err := r.lockedDeployment.UpdateExpectedServicesFromManifest(); err != nil {
		r.failf(err, "Failed to update expected services")
		return
	}

	// Build a context that we can pass to each operation
	restoreCtx := NewContext(
		WithContextCtx(ctx),
		WithContextBackupRestoreTask(r.restoreTask),
		WithContextBackupLocationSpecification(r.locationSpec),
		WithContextBackupRestoreLocationSpecification(r.restoreLocationSpec),
		WithContextPgConnInfo(r.pgConnInfo),
		WithContextEsSidecarInfo(r.esSidecarInfo),
		WithContextConnFactory(r.connFactory),
		WithContextReleaseManifest(r.releaseManifest),
		WithContextBuilderMinioLocationSpec(r.builderMinioLocationSpec),
	)

	// Build a slice of services that we wish to restore
	backupManifest := r.lockedDeployment.CurrentReleaseManifest
	bgwSvc, err := r.loadSvcFromManifest(backupManifest, "backup-gateway")
	if err != nil {
		r.failf(err, "Failed to locate backup-gateway in backup manifest")
		return
	}

	desiredServices := []*deployment.Service{bgwSvc}
	// Repeat restoration for all remaining services
	for _, svc := range r.lockedDeployment.ExpectedServices {
		if svc.Name() == "backup-gateway" {
			continue // We've already added backup-gateway
		}

		svc, err := r.loadSvcFromManifest(backupManifest, svc.Name())
		if err != nil {
			r.failf(err, "Failed to determine habitat package information for %s", svc.Name())
			return
		}

		desiredServices = append(desiredServices, svc)
	}

	r.infof("Unloading services")
	if err = r.unloadServices(restoreCtx.ctx, desiredServices); err != nil {
		r.failf(err, "Failed to unload services")
		return
	}

	if err = r.restoreAutomateCLI(ctx, backupManifest); err != nil {
		r.failf(err, "Failed to restore automate-cli")
		return
	}

	if err = r.restoreServices(ctx, desiredServices, restoreCtx, cancel); err != nil {
		r.failf(err, "Failed to restore services")
		return
	}

	r.infof("Backup restore finished")
	r.publishBackupEvent(r.restoreTask.TaskID(), api.DeployEvent_COMPLETE_OK)
}

// Install and binlink automate-cli if it exists in the given manifest
func (r *Runner) restoreAutomateCLI(ctx context.Context, manifest manifest.ReleaseManifest) error {
	svc := deployment.ServiceFromManifest(manifest, "automate-cli")
	if svc == nil {
		// NOTE(ssd) 2019-02-19: Do we have backups where
		// automate-cli wasn't in the manifest?
		r.infof("automate-cli not found in manifest, skipping installation")
		return nil
	}

	channel := r.lockedDeployment.Config.GetDeployment().GetV1().GetSvc().GetChannel().GetValue()
	err := r.target.InstallService(ctx, svc, channel)
	if err != nil {
		return errors.Wrap(err, "failed to install automate-cli")
	}

	cmdOutput, err := r.target.BinlinkPackage(ctx, svc, "chef-automate")
	if err != nil {
		return errors.Wrapf(err, "failed to binlink automate-cli: %s", cmdOutput)
	}

	return nil
}

func (r *Runner) restoreServices(ctx context.Context, desiredServices []*deployment.Service, restoreCtx Context, cancel func()) error {
	channel := r.lockedDeployment.Config.GetDeployment().GetV1().GetSvc().GetChannel().GetValue()
	var bucket Bucket

	// Load the verifier from the remote bucket since the backup gateway won't be
	// available yet by this point.
	verifier, err := LoadMetadataVerifier(ctx, restoreCtx.restoreBucket, r.restoreTask.Sha256)
	if err != nil {
		r.failf(err, "Failed to load service metadata checksum information")
		return err
	}

	// Restore the services in topological order.
	for _, svc := range desiredServices {
		r.infof("Restoring %s", svc.Name())

		// Install the package
		err := r.target.InstallService(restoreCtx.ctx, svc, channel)
		if err != nil {
			r.failf(err, "Failed to install habitat package for %s", svc.Name())
			return err
		}

		// Install any binlinks for this package
		binlinks := services.BinlinksForPackage(svc.Name())
		for _, cmd := range binlinks {
			cmdOutput, err := r.target.BinlinkPackage(restoreCtx.ctx, svc, cmd)
			if err != nil {
				r.failf(err, "failed to binlink command %q for service %q: %s", cmd, svc.Name(), cmdOutput)
				return err
			}
		}

		config, err := r.configRenderer(svc)
		if err != nil {
			r.failf(err, "Failed to render habitat configuration for %s", svc.Name())
			return err
		}

		err = r.target.SetUserToml(svc.Name(), config)
		if err != nil {
			r.failf(err, "Failed to write habitat configuration for %s", svc.Name())
			return err
		}

		// Load backup metadata for the service. It contains the backup spec
		// that we'll use to restore the service.
		// NOTE: the backup-gateway is a special case that needs the remote
		// bucket
		switch svc.Name() {
		case "backup-gateway":
			bucket = restoreCtx.restoreBucket
		default:
			bucket = restoreCtx.bucket
		}

		var spec Spec
		metadata, err := LoadServiceMetadata(
			ctx,
			bucket,
			svc.Name(),
			verifier,
		)

		// If the metadata file exists but we failed to load it for whatever
		// reason, like a network issue or corrupted metadata file, then we want to
		// error out.
		if err != nil && !IsNotExist(err) {
			r.failf(err, "Failed to load metadata for service %s", svc.Name())
			return err
		}

		// If the backup metadata is missing, we assume the service is stateless or
		// a data service. Therefore, we'll create a blank specification for the
		// service that contains no restore actions.
		if err != nil {
			spec = Spec{Name: svc.Name()}
		} else {
			spec = *metadata.Spec
		}

		// TODO make the executor set the command executor on specs/operations
		SetCommandExecutor(spec, command.NewExecExecutor())

		// Create a new executor to run the restore operations for services'
		// backup spec.
		executor := NewExecutor(
			WithEventChan(r.eventChan),
			WithErrorChan(r.errChan),
			WithSpec(spec),
			WithCancel(cancel),
		)

		// Restore the specification
		// TODO: Right now we assume that the commands that are run can be run
		// before the service is started. That's okay because the only spec that
		// has commands is the deployment-service which is running this code.
		// If that changes we will have to separate pre/post service operational
		// restore operations.
		if err := executor.Restore(restoreCtx, metadata); err != nil {
			r.failf(err, "Failed to restore synchronous operations")
			return err
		}

		bindInfo, err := services.AllBinds.DefaultsForService(svc.Name())
		if err != nil {
			r.failf(err, "Failed to load bind info for service %s", svc.Name())
			return err
		}

		// Now that all data has been restored we need start the service up
		// and wait for it to come up healthy.
		if err := r.target.LoadService(restoreCtx.ctx, svc, target.BindMode(bindInfo.Mode), target.Binds(bindInfo.Specs)); err != nil {
			r.failf(err, "Failed to load service %s", svc.Name())
			return err
		}

		if err := r.waitForHealthy(restoreCtx.ctx, svc.Name()); err != nil {
			r.failf(err, "Timed out waiting for service %s to start", svc.Name())
			return err
		}
	}

	return nil
}

func (r *Runner) loadSvcFromManifest(manifest manifest.ReleaseManifest, name string) (*deployment.Service, error) {
	svc := deployment.ServiceFromManifest(manifest, name)
	if svc == nil {
		return nil, errors.Errorf("Failed to determine habitat package information for %s", name)
	}

	storedSvc, found := r.lockedDeployment.ServiceByName(svc.Name())
	if !found {
		return nil, errors.Errorf("No stored configuration for service %s", name)
	}

	svc.SSLKey = storedSvc.SSLKey
	svc.SSLCert = storedSvc.SSLCert

	return svc, nil
}

func (r *Runner) failf(err error, message string, args ...interface{}) {
	r.errorf(err, message, args...)

	if r.backupTask != nil {
		r.publishBackupEvent(r.backupTask.TaskID(), api.CompleteFail)
		return
	}

	r.publishBackupEvent(r.restoreTask.TaskID(), api.CompleteFail)
	r.failed = true
}

func (r *Runner) errorf(err error, message string, args ...interface{}) {
	logrus.WithFields(r.defaultFields()).WithError(err).Errorf(message, args...)
}

func (r *Runner) warnf(err error, message string, args ...interface{}) {
	logrus.WithFields(r.defaultFields()).WithError(err).Warnf(message, args...)
}

func (r *Runner) infof(message string, args ...interface{}) {
	logrus.WithFields(r.defaultFields()).Infof(message, args...)
}

func (r *Runner) defaultFields() logrus.Fields {
	var fields logrus.Fields

	if r.restoreTask != nil {
		fields = logrus.Fields{
			"backup_id":  r.restoreTask.Backup.TaskID(),
			"restore_id": r.restoreTask.TaskID(),
		}
	}

	if r.backupTask != nil {
		fields = logrus.Fields{"backup_id": r.backupTask.TaskID()}
	}

	return fields
}

func (r *Runner) waitForHealthy(ctx context.Context, serviceName string) error {
	ticker := time.NewTicker(250 * time.Millisecond)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			return errors.Wrapf(ctx.Err(), "Timed out waiting for service %s to be healthy", serviceName)
		case <-ticker.C:
			status := r.target.Status(context.Background(), []string{serviceName})

			if status.ServiceHealthy(serviceName) {
				return nil
			}
		}
	}
}

// unloadServices unloads the services and waits for them to be down
func (r *Runner) unloadServices(ctx context.Context, svcs []*deployment.Service) error {
	svcNames := []string{}

	// Unload the services
	for _, svc := range svcs {
		if err := r.target.UnloadService(ctx, svc); err != nil {
			return err
		}

		svcNames = append(svcNames, svc.Name())
	}

	ticker := time.NewTicker(250 * time.Millisecond)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			b := strings.Builder{}
			status := r.target.Status(ctx, svcNames)

			for _, ss := range status.Services {
				if ss.State != api.ServiceState_DOWN {
					b.WriteString(fmt.Sprintf("%s, ", ss.Name))
				}
			}

			return errors.Wrapf(ctx.Err(), "Timed out waiting for %s to unload", b.String())
		case <-ticker.C:
			status := r.target.Status(ctx, svcNames)

			unloaded := true
			for _, ss := range status.Services {
				if ss.State != api.ServiceState_DOWN {
					unloaded = false
				}
			}

			if unloaded {
				return nil
			}
		}
	}
}

func (r *Runner) startRestoreEventPublisher() {
	go func() {
		for {
			select {
			case e := <-r.eventChan:
				// publish executor events
				r.updateOperationStatus(r.restoreTask.TaskID(), e)
				r.publishBackupEvent(r.restoreTask.TaskID(), api.DeployEvent_RUNNING)
			case <-r.eventTerm:
				return
			}
		}
	}()
}

func (r *Runner) startBackupEventPublisher() {
	go func() {
		for {
			select {
			case e := <-r.eventChan:
				// publish executor events
				r.updateOperationStatus(r.backupTask.TaskID(), e)
				r.publishBackupEvent(r.backupTask.TaskID(), api.DeployEvent_RUNNING)
			case <-r.eventTerm:
				return
			}
		}
	}()
}

func (r *Runner) unlockDeployment() {
	if r.lockedDeployment != nil {
		r.lockedDeployment.Unlock()
	}
}

func (r *Runner) clearRunningTask() {
	var err error
	r.runningTask, err = newCancellableTask(api.BackupStatusResponse_IDLE, []string{}, func() {})
	if err != nil {
		r.failf(err, "Failed to clear running task")
	}
}

func (r *Runner) startBackupOperations(ctx context.Context) {
	var err error
	deadline, ok := ctx.Deadline()
	if !ok {
		deadline = time.Now().Add(2 * time.Hour)
	}
	ctx, cancel := context.WithDeadline(context.Background(), deadline)
	defer cancel()

	r.eventChan = make(chan api.DeployEvent_Backup_Operation)
	r.eventTerm = make(chan struct{})
	r.errChan = make(chan error, 1)
	r.runningTask, err = newCancellableTask(api.BackupStatusResponse_CREATE, []string{r.backupTask.TaskID()}, cancel)
	if err != nil {
		r.failf(err, "Failed to configure operation cancellation")
		return
	}

	// Make sure that the event publisher always exits and that any listeners
	// on the event stream will get the task complete signal.
	defer func() {
		r.eventSender.TaskComplete()
		close(r.eventTerm)
		r.clearRunningTask()
		r.unlockDeployment()
	}()

	backupCtx := NewContext(
		WithContextCtx(ctx),
		WithContextBackupLocationSpecification(r.locationSpec),
		WithContextPgConnInfo(r.pgConnInfo),
		WithContextBackupTask(r.backupTask),
		WithContextEsSidecarInfo(r.esSidecarInfo),
		WithContextConnFactory(r.connFactory),
		WithContextReleaseManifest(r.releaseManifest),
		WithContextBuilderMinioLocationSpec(r.builderMinioLocationSpec),
	)

	// Start the event publisher
	r.startBackupEventPublisher()

	backupSHA256, err := backupCtx.Transact(func(bctx Context) error {
		// If an operation fails it should publish it's error message to the err
		// channel. Since we only care about the first error we'll leave the buffer
		// size at one so that we don't end up publishing errors that were generated
		// by the context being cancelled.
		wg := sync.WaitGroup{}
		lock := &sync.Mutex{}
		// Start the executors
		for _, s := range r.specs {
			wg.Add(1)
			spec := s
			go func() {
				defer wg.Done()

				// Make sure another spec hasn't already failed before we start
				// more.
				select {
				case <-ctx.Done():
					return
				default:
				}

				executor := NewExecutor(
					WithEventChan(r.eventChan),
					WithErrorChan(r.errChan),
					WithSpec(spec),
					WithCancel(cancel),
					WithLock(lock),
				)
				_ = executor.Backup(bctx)
			}()
		}

		// wait for them to complete. The executor is responsible for signaling
		// the error and cancelling the context in the event of an issue.
		wg.Wait()

		if bctx.ctx.Err() != nil {
			select {
			// We don't block on errchan. If it's not available, we'll use the error from the context
			// Not all paths seem to write to errChan, for example when a timeout is hit
			case err := <-r.errChan:
				return err
			default:
				return bctx.ctx.Err()
			}
		}

		return nil
	})

	// If our context was canceled by timeout or an executor signaled it to
	// cancel because of an error, log and publish failure events and
	// exit.
	if err != nil {
		r.failf(err, "Backup failed")

		return
	}

	desc := &api.BackupDescription{Sha256: backupSHA256}

	infoMsg := api.DeployEvent_Backup{
		Status:      api.DeployEvent_COMPLETE_OK,
		Description: desc,
	}
	r.eventSender.Backup(infoMsg)

	r.infof("Created backup %s with SHA256: %s", r.backupTask.TaskID(), desc.Sha256)
	r.infof("Backup complete")
}

// publishBackupEvent compiles the latest aggregate backup status and publishes
// it to the tasks corresponding event stream.
func (r *Runner) publishBackupEvent(taskID string, status api.DeployEvent_Status) {
	r.eventSender.Backup(r.createBackupEvent(taskID, status))
}

// createBackupEvent creates the latest aggregate backup status and returns
// a new backup event.
func (r *Runner) createBackupEvent(taskID string, status api.DeployEvent_Status) api.DeployEvent_Backup {
	ops := []*api.DeployEvent_Backup_Operation{}

	r.taskOperationsMutex.Lock()
	defer r.taskOperationsMutex.Unlock()

	for _, o := range r.TaskOperations[taskID] {
		p := o // :facepalm:
		ops = append(ops, &p)
	}

	return api.DeployEvent_Backup{
		Status:     status,
		Operations: ops,
	}
}

// Update our internal task operation map with a new operation status.
func (r *Runner) updateOperationStatus(taskID string,
	op api.DeployEvent_Backup_Operation) {

	defer r.taskOperationsMutex.Unlock()
	r.taskOperationsMutex.Lock()

	if r.TaskOperations[taskID] == nil {
		r.TaskOperations[taskID] = map[string]api.DeployEvent_Backup_Operation{}
	}

	r.TaskOperations[taskID][op.Name] = op
}
