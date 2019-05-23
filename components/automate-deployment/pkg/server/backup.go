package server

import (
	"context"
	"os"

	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/backup"
)

// CreateBackup creates an Automate Backup
func (s *server) CreateBackup(ctx context.Context, req *api.CreateBackupRequest) (*api.CreateBackupResponse, error) {

	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
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
	err := s.acquireLock(ctx) // Unlocked by the backup runner
	if err != nil {
		logrus.WithError(err).Error("Failed to acquire lock")
		return nil, err
	}

	sender := s.newEventSender()

	task, err := s.backupRunner.CreateBackup(ctx, s.deployment, sender)
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
func (s *server) ListBackups(ctx context.Context, req *api.ListBackupsRequest) (*api.ListBackupsResponse, error) {

	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	backups, err := s.backupRunner.ListBackups(ctx)
	if err != nil {
		logrus.WithError(err).Error("Failed to list backup")
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &api.ListBackupsResponse{Backups: backups}, nil
}

// ShowBackup shows details about an AutomateBackup
func (s *server) ShowBackup(ctx context.Context, req *api.ShowBackupRequest) (*api.ShowBackupResponse, error) {

	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	description, err := s.backupRunner.ShowBackup(ctx, req.Backup)
	if err != nil {
		logrus.WithError(err).Error("Failed to list backup")
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &api.ShowBackupResponse{Backup: req.Backup, Description: description}, nil
}

// CancelBackup cancels the running backup operation
func (s *server) CancelBackup(ctx context.Context, req *api.CancelBackupRequest) (*api.CancelBackupResponse, error) {

	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	err := s.backupRunner.Cancel(ctx)
	if err != nil {
		logrus.WithError(err).Error("failed to cancel backup operation")
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &api.CancelBackupResponse{}, nil
}

// DeleteBackups deletes a set of 1 or more Automate Backups
func (s *server) DeleteBackups(ctx context.Context, req *api.DeleteBackupsRequest) (*api.DeleteBackupsResponse, error) {

	err := s.acquireLock(ctx) // unlocked by the backupRunner
	if err != nil {
		logrus.WithError(err).Error("Failed to acquire lock")
		return nil, err
	}

	if err := s.backupRunner.DeleteBackups(ctx, s.deployment, req.GetBackups()); err != nil {
		logrus.WithError(err).Error("Failed to delete backup")
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &api.DeleteBackupsResponse{}, nil
}

// Restore backup restore an Automate backup
func (s *server) RestoreBackup(ctx context.Context, req *api.RestoreBackupRequest) (*api.RestoreBackupResponse, error) {
	res := &api.RestoreBackupResponse{}

	// Update the config and reload the backup runner because our config has
	// changed.
	if err := s.updateUserOverrideConfigFromRestoreBackupRequest(req); err != nil {
		return res, err
	}

	if err := s.reloadBackupRunner(); err != nil {
		logrus.WithError(err).Error("failed to load backup runner")
		return res, status.Error(codes.Internal, err.Error())
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

	sender := s.newEventSender()

	// Lock the deployment and stop the converge loop. The runner will
	// handle unlocking the deployment and removing the converge disable
	// file.
	err = s.acquireLock(ctx)
	if err != nil {
		s.deployment.Unlock()
		logrus.WithError(err).Error("Failed to acquire lock")
		return nil, err
	}

	// This file should have already been written during the deployment-service
	// restore, but if somebody were to trigger the server side restore without
	// first restoring the deployment-service it's possible this file won't
	// be there so we'll write it out.
	f, err := os.OpenFile(api.ConvergeDisableFilePath, os.O_RDONLY|os.O_CREATE, 0700)
	if err != nil {
		return res, status.Error(codes.Internal, err.Error())
	}
	defer f.Close() // nolint errcheck

	task, err := s.backupRunner.RestoreBackup(ctx, s.deployment, sender, bgwLocationSpec, remoteRestoreSpec, req.Restore)
	if err != nil {
		logrus.WithError(err).Error("Failed to restore backup")
		return res, status.Error(codes.Internal, err.Error())
	}

	s.senderStore.Set(task.TaskID(), sender)

	res.Restore = task
	return res, nil
}

// BackupStatus shows the current status of the backup runner
func (s *server) BackupStatus(ctx context.Context, req *api.BackupStatusRequest) (*api.BackupStatusResponse, error) {

	if !s.HasConfiguredDeployment() {
		return nil, ErrorNotConfigured
	}

	task := s.backupRunner.RunningTask(ctx)
	if task == nil {
		logrus.Error("failed to get running backup task")
		return nil, status.Error(codes.Internal, "failed to get running backup task")
	}

	return task.Status, nil
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
