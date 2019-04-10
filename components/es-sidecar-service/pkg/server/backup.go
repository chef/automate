package server

import (
	"context"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	api "github.com/chef/automate/api/interservice/es_sidecar"
)

var errBackupsDisabled = status.Error(codes.FailedPrecondition, "Elasticsearch backup/restore is disabled. You must configure backups when using an external Elasticsearch. Read the docs for more")

// CreateRepository creates a snapshot repository for the service specified in
// the request. This API is exposed for expert use; end-user backups should
// only need to request a snapshot and let the repo be created implicitly.
func (s *EsSidecarServer) CreateRepository(ctx context.Context, in *api.CreateRepositoryRequest) (*api.CreateRepositoryResponse, error) {
	if s.backupsConfig.BackupsDisabled() {
		return nil, errBackupsDisabled
	}
	err := s.es.CreateSnapshotRepository(ctx, in.ServiceName, s.backupsConfig)
	return &api.CreateRepositoryResponse{}, err
}

// ConvergeRepositorySettings updates settings such as backup/restore throttle
// settings on an existing repository. Elasticsearch's APIs don't distinguish
// between create/update so the underlying implementation does not either,
// i.e., there is no error when updating a repo that doesn't exist, it gets
// created instead
func (s *EsSidecarServer) ConvergeRepositorySettings(ctx context.Context, in *api.ConvergeRepositorySettingsRequest) (*api.ConvergeRepositorySettingsResponse, error) {
	if s.backupsConfig.BackupsDisabled() {
		return nil, errBackupsDisabled
	}
	err := s.es.CreateSnapshotRepository(ctx, in.ServiceName, s.backupsConfig)
	return &api.ConvergeRepositorySettingsResponse{}, err
}

// RemoveRepository deletes the snapshot repository for the given service
func (s *EsSidecarServer) RemoveRepository(ctx context.Context, in *api.RemoveRepositoryRequest) (*api.RemoveRepositoryResponse, error) {
	err := s.es.RemoveSnapshotRepository(ctx, in.ServiceName, s.backupsConfig)
	return &api.RemoveRepositoryResponse{}, err
}

// CreateSnapshot creates an Es snapshot for the service specified in the
// request. The repository type and settings are specified by the backupsConfig
// set on the server. The repository will be created/updated as necessary prior
// to the actual snapshot creation.
func (s *EsSidecarServer) CreateSnapshot(ctx context.Context, in *api.CreateSnapshotRequest) (*api.CreateSnapshotResponse, error) {
	if s.backupsConfig.BackupsDisabled() {
		return nil, errBackupsDisabled
	}
	err := s.es.CreateSnapshot(ctx, in.ServiceName, in.BackupId, in.MultiIndexSpecification, s.backupsConfig)
	return &api.CreateSnapshotResponse{}, err
}

// CreateSnapshotStatus gives the status of snapshot creation
func (s *EsSidecarServer) CreateSnapshotStatus(ctx context.Context, in *api.CreateSnapshotStatusRequest) (*api.CreateSnapshotStatusResponse, error) {
	if s.backupsConfig.BackupsDisabled() {
		return nil, errBackupsDisabled
	}
	status, err := s.es.GetCreateSnapshotStatus(ctx, in.ServiceName, in.BackupId)
	if err != nil {
		return &api.CreateSnapshotStatusResponse{}, err
	}
	r := &api.CreateSnapshotStatusResponse{
		SnapshotState:      api.SnapshotState(api.SnapshotState_value[status.State]),
		ProgressPercentage: status.ProgressPercentage,
		Message:            status.Message,
	}
	return r, nil
}

// RestoreSnapshot initiates the restore of an Es snapshot for the service
// specified in the request. The repository type and settings are specified by
// the backupsConfig set on the server.
func (s *EsSidecarServer) RestoreSnapshot(ctx context.Context, in *api.RestoreSnapshotRequest) (*api.RestoreSnapshotResponse, error) {
	if s.backupsConfig.BackupsDisabled() {
		return nil, errBackupsDisabled
	}
	err := s.es.RestoreSnapshot(ctx, in.ServiceName, in.BackupId, s.backupsConfig)
	return &api.RestoreSnapshotResponse{}, err
}

// RestoreSnapshotStatus gives the status of snapshot restore
func (s *EsSidecarServer) RestoreSnapshotStatus(ctx context.Context, in *api.RestoreSnapshotStatusRequest) (*api.RestoreSnapshotStatusResponse, error) {
	if s.backupsConfig.BackupsDisabled() {
		return nil, errBackupsDisabled
	}
	status, err := s.es.GetRestoreSnapshotStatus(ctx, in.ServiceName, in.BackupId)
	if err != nil {
		return &api.RestoreSnapshotStatusResponse{}, nil
	}
	r := &api.RestoreSnapshotStatusResponse{
		SnapshotState:      api.SnapshotState(api.SnapshotState_value[status.State]),
		ProgressPercentage: status.ProgressPercentage,
		Message:            status.Message,
	}
	return r, nil
}

func (s *EsSidecarServer) DeleteSnapshot(ctx context.Context, in *api.DeleteSnapshotRequest) (*api.DeleteSnapshotResponse, error) {
	if s.backupsConfig.BackupsDisabled() {
		return nil, errBackupsDisabled
	}
	err := s.es.DeleteSnapshot(ctx, in.ServiceName, in.BackupId)
	return &api.DeleteSnapshotResponse{}, err
}
