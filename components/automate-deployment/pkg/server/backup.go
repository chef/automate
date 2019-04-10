package server

import (
	"context"
	"os"
	"time"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	dc "github.com/chef/automate/api/config/deployment"
	papi "github.com/chef/automate/api/config/platform"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/backup"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/platform"
	"github.com/chef/automate/lib/platform/pg"
)

func buildPlatformConfig(automateConfig *dc.AutomateConfig) *platform.Config {
	platformConfig := platform.Config{
		Config: &papi.Config{
			Postgresql: &papi.Config_Postgresql{
				Ip: automateConfig.GetPgGateway().GetV1().GetSys().GetService().GetHost().GetValue(),
				Cfg: &papi.Config_Postgresql_Cfg{
					Port: int64(automateConfig.GetPgGateway().GetV1().GetSys().GetService().GetPort().GetValue()),
				},
			},
			Platform: &papi.Config_Platform{
				ExternalPostgresql: automateConfig.GetGlobal().GetV1().GetExternal().GetPostgresql(),
			},
		},
	}
	return &platformConfig
}

// CreateBackup creates an Automate Backup
func (s *server) CreateBackup(ctx context.Context,
	req *api.CreateBackupRequest) (*api.CreateBackupResponse, error) {
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	// Get the correct ConnInfo
	// platformConfig knows how to deal with superuser, and external vs internal PG
	platformConfig := buildPlatformConfig(s.deployment.Config)
	superuser, err := platformConfig.PGSuperUser()
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}
	pgConnInfo, err := platformConfig.GetPGConnInfoURI(superuser)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	// lock the deployment for the duration of the backup
	// this gives us the following properties:
	// - only one backup can take place at a time
	// - we don't converge the deployment during a backup
	// this means that we won't run into the situation where
	// we're deploying new services or configuration while
	// we're trying to back up data
	//
	// NOTE: server.DumpDB doesn't require a lock, so we're
	// okay here
	s.deployment.Lock() // Unlocked by the backup runner

	chefServerEnabled := s.deployment.Config.GetDeployment().GetV1().GetSvc().GetEnableChefServer().GetValue()
	workflowEnabled := s.deployment.Config.GetDeployment().GetV1().GetSvc().GetEnableWorkflow().GetValue()

	esSidecarInfo := backup.ESSidecarConnInfo{
		Host: s.deployment.Config.EsSidecar.V1.Sys.Service.Host.Value,
		Port: s.deployment.Config.EsSidecar.V1.Sys.Service.Port.Value,
	}

	locationSpec, err := s.backupGatewayLocationSpec()
	if err != nil {
		s.deployment.Unlock()
		// We should have had a valid location specification. We should never have accepted
		// an invalid one. Something has gone wrong
		return nil, status.Error(codes.Internal, err.Error())
	}

	sender := s.newEventSender()

	opts := []backup.RunnerOpt{
		backup.WithEventSender(sender),
		backup.WithSpecs(backup.DefaultSpecs(chefServerEnabled, workflowEnabled)),
		backup.WithBackupLocationSpecification(locationSpec),
		backup.WithPGConnInfo(pgConnInfo),
		backup.WithLockedDeployment(s.deployment),
		backup.WithEsSidecarInfo(esSidecarInfo),
		backup.WithConnFactory(s.connFactory),
		backup.WithReleaseManifest(s.deployment.CurrentReleaseManifest),
	}

	if req.BackupTimeoutSeconds > 0 {
		opts = append(opts, backup.WithTimeout(time.Second*time.Duration(req.GetBackupTimeoutSeconds())))
	}

	runner := backup.NewRunner(opts...)
	task, err := runner.CreateBackup()
	if err != nil {
		// CreateBackup doesn't look like it can return an error. But if it did,
		// the mutex needs to be unlocked?
		logrus.WithError(err).Error("Failed to create backup")
		return nil, status.Error(codes.Internal, err.Error())
	}
	s.senderStore.Set(task.TaskID(), sender)

	return &api.CreateBackupResponse{Backup: task}, nil
}

// ListBackups lists Automate Backups
func (s *server) ListBackups(ctx context.Context,
	req *api.ListBackupsRequest) (*api.ListBackupsResponse, error) {
	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	locationSpec, err := s.backupGatewayLocationSpec()
	if err != nil {
		return nil, ErrorNotConfigured
	}

	runner := backup.NewRunner(backup.WithBackupLocationSpecification(locationSpec))

	backups, err := runner.ListBackups()
	if err != nil {
		logrus.WithError(err).Error("Failed to list backup")
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &api.ListBackupsResponse{Backups: backups}, nil
}

// ShowBackup shows details about an AutomateBackup
func (s *server) ShowBackup(ctx context.Context,
	req *api.ShowBackupRequest) (*api.ShowBackupResponse, error) {

	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	locationSpec, err := s.backupGatewayLocationSpec()
	if err != nil {
		return nil, ErrorNotConfigured
	}

	runner := backup.NewRunner(backup.WithBackupLocationSpecification(locationSpec))

	description, err := runner.ShowBackup(req.Backup)
	if err != nil {
		logrus.WithError(err).Error("Failed to list backup")
		return nil, status.Error(codes.Internal, err.Error())
	}
	return &api.ShowBackupResponse{Backup: req.Backup, Description: description}, nil
}

// DeleteBackups deletes a set of 1 or more Automate Backups
func (s *server) DeleteBackups(ctx context.Context,
	req *api.DeleteBackupsRequest) (*api.DeleteBackupsResponse, error) {

	// We're taking the lock so we don't ask ES to do multiple snapshot ops
	// at the same time
	s.deployment.Lock()
	defer s.deployment.Unlock()

	pgConnInfo := &pg.A2ConnInfo{
		Host:  s.deployment.Config.Postgresql.V1.Sys.Service.Host.Value,
		Port:  uint64(s.deployment.Config.Postgresql.V1.Sys.Service.Port.Value),
		User:  s.deployment.Config.Postgresql.V1.Sys.Superuser.Name.Value,
		Certs: pg.A2SuperuserCerts,
	}
	esSidecarInfo := backup.ESSidecarConnInfo{
		Host: s.deployment.Config.EsSidecar.V1.Sys.Service.Host.Value,
		Port: s.deployment.Config.EsSidecar.V1.Sys.Service.Port.Value,
	}

	locationSpec, err := s.backupGatewayLocationSpec()
	if err != nil {
		// We should have had a valid location specification. We should never have accepted
		// an invalid one. Something has gone wrong
		return nil, status.Error(codes.Internal, err.Error())
	}

	sender := s.newEventSender()

	opts := []backup.RunnerOpt{
		backup.WithEventSender(sender),
		backup.WithBackupLocationSpecification(locationSpec),
		backup.WithPGConnInfo(pgConnInfo),
		backup.WithEsSidecarInfo(esSidecarInfo),
		backup.WithConnFactory(s.connFactory),
	}

	runner := backup.NewRunner(opts...)

	if err := runner.DeleteBackups(req.GetBackups()); err != nil {
		logrus.WithError(err).Error("Failed to delete backup")
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &api.DeleteBackupsResponse{}, nil
}

// Restore backup restore an Automate backup
func (s *server) RestoreBackup(ctx context.Context,
	req *api.RestoreBackupRequest) (*api.RestoreBackupResponse, error) {
	res := &api.RestoreBackupResponse{}

	if err := s.updateUserOverrideConfigFromRestoreBackupRequest(req); err != nil {
		return res, err
	}

	// In local mode the default bucket is backups
	bucket := "backups"
	basePath := ""

	// If we are restoring from S3 we'll override the bucket and base path
	// with the configuration that has been given.
	if req.Restore.GetS3BackupLocation().GetBucketName() != "" {
		bucket = req.Restore.GetS3BackupLocation().GetBucketName()
		basePath = req.Restore.GetS3BackupLocation().GetBasePath()
	}

	bgwLocationSpec, err := backup.NewBackupGatewayLocationSpec(
		s.deployment.BackupGatewayEndpoint(),
		bucket,
		basePath,
		[]byte(s.deployment.CA().RootCert()),
		s.secretStore,
	)
	if err != nil {
		logrus.WithError(err).Error("generate backup-gateway location specification")
		return res, status.Error(codes.InvalidArgument, err.Error())
	}

	remoteRestoreSpec := backup.NewRemoteLocationSpecificationFromRestoreTask(req.Restore)

	pgConnInfo := &pg.A2ConnInfo{
		Host:  s.deployment.Config.GetPostgresql().GetV1().GetSys().GetService().GetHost().GetValue(),
		Port:  uint64(s.deployment.Config.GetPostgresql().GetV1().GetSys().GetService().GetPort().GetValue()),
		User:  s.deployment.Config.GetPostgresql().GetV1().GetSys().GetSuperuser().GetName().GetValue(),
		Certs: pg.A2SuperuserCerts,
	}
	esSidecarInfo := backup.ESSidecarConnInfo{
		Host: s.deployment.Config.GetEsSidecar().GetV1().GetSys().GetService().GetHost().GetValue(),
		Port: s.deployment.Config.GetEsSidecar().GetV1().GetSys().GetService().GetPort().GetValue(),
	}

	target := target.NewLocalTarget(airgap.AirgapInUse())
	sender := s.newEventSender()

	configRenderer, err := s.configRenderer()
	if err != nil {
		return res, err
	}

	// Lock the deployment and stop the converge loop. The runner will
	// handle unlocking the deployment and removing the converge disable
	// file.
	s.deployment.Lock()
	// This file should have already been written during the deployment-service
	// restore, but if somebody were to trigger the server side restore without
	// first restoring the deployment-service it's possible this file won't
	// be there so we'll write it out.
	f, err := os.OpenFile(api.ConvergeDisableFilePath, os.O_RDONLY|os.O_CREATE, 0700)
	if err != nil {
		return res, status.Error(codes.Internal, err.Error())
	}
	defer f.Close() // nolint errcheck

	// TODO: Instead of creating a new runner every time we do a backup and
	// restore request we should create a single runner when the server
	// initializes and use it to make requests. This will allow us to track
	// operations and to implement backup and restore cancellation.
	runner := backup.NewRunner(
		backup.WithConfigRenderer(configRenderer),
		backup.WithEventSender(sender),
		backup.WithPGConnInfo(pgConnInfo),
		backup.WithEsSidecarInfo(esSidecarInfo),
		backup.WithConnFactory(s.connFactory),
		backup.WithLockedDeployment(s.deployment),
		backup.WithRestoreTask(req.Restore),
		backup.WithTarget(target),
		backup.WithBackupLocationSpecification(bgwLocationSpec),
		backup.WithBackupRestoreLocationSpecification(remoteRestoreSpec),
	)
	task, err := runner.RestoreBackup()
	if err != nil {
		logrus.WithError(err).Error("Failed to restore backup")
		return res, status.Error(codes.Internal, err.Error())
	}

	s.senderStore.Set(task.TaskID(), sender)

	res.Restore = task
	return res, nil
}

func (s *server) backupGatewayLocationSpec() (backup.LocationSpecification, error) {
	var bucket string
	var basePath string

	switch s.deployment.Config.GetGlobal().GetV1().GetBackups().GetLocation().GetValue() {
	case "s3":
		bucket = s.deployment.Config.GetGlobal().GetV1().GetBackups().GetS3().GetBucket().GetName().GetValue()
		basePath = s.deployment.Config.GetGlobal().GetV1().GetBackups().GetS3().GetBucket().GetBasePath().GetValue()
	default:
		bucket = "backups"
	}

	return backup.NewBackupGatewayLocationSpec(
		s.deployment.BackupGatewayEndpoint(),
		bucket,
		basePath,
		[]byte(s.deployment.CA().RootCert()),
		s.secretStore,
	)
}
