package migrations

import (
	"context"

	gwreq "github.com/chef/automate/api/external/infra_proxy/migrations/request"
	gwres "github.com/chef/automate/api/external/infra_proxy/migrations/response"
	infra_req "github.com/chef/automate/api/interservice/infra_proxy/migrations/request"
	infra_res "github.com/chef/automate/api/interservice/infra_proxy/migrations/response"
)

// GetMigrationStatus fetches the latest migration status against migration id
func (a *InfraProxyMigrationServer) GetMigrationStatus(ctx context.Context, r *gwreq.GetMigrationStatusRequest) (*gwres.GetMigrationStatusResponse, error) {
	req := &infra_req.GetMigrationStatusRequest{
		MigrationId: r.MigrationId,
	}
	res, err := a.migrationClient.GetMigrationStatus(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.GetMigrationStatusResponse{
		MigrationId:     res.GetMigrationId(),
		MigrationType:   res.GetMigrationType(),
		MigrationStatus: res.GetMigrationStatus(),
	}, nil
}

// CancelMigration cancel the ongoing migration
func (a *InfraProxyMigrationServer) CancelMigration(ctx context.Context, r *gwreq.CancelMigrationRequest) (*gwres.CancelMigrationResponse, error) {
	req := &infra_req.CancelMigrationRequest{
		MigrationId: r.MigrationId,
		ServerId:    r.ServerId,
	}
	res, err := a.migrationClient.CancelMigration(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.CancelMigrationResponse{
		Success: res.Success,
		Errors:  res.Errors,
	}, nil
}

// GetStagedData Get the staged data from db for preview screen
func (a *InfraProxyMigrationServer) GetStagedData(ctx context.Context, r *gwreq.GetStagedDataRequest) (*gwres.GetStagedDataResponse, error) {
	req := &infra_req.GetStagedDataRequest{
		MigrationId: r.MigrationId,
	}
	res, err := a.migrationClient.GetStagedData(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.GetStagedDataResponse{
		MigrationId: res.MigrationId,
		StagedData:  getStagedData(res.StagedData),
	}, nil
}

func getStagedData(stageData *infra_res.StagedData) *gwres.StagedData {
	users := []*gwres.User{}
	if stageData != nil {
		for _, user := range stageData.Users {
			users = append(users, getStagedUser(user))
		}
	}

	return &gwres.StagedData{
		OrgsToMigrate: stageData.GetOrgsToMigrate(),
		OrgsToSkip:    stageData.GetOrgsToSkip(),
		OrgsToUpdate:  stageData.GetOrgsToUpdate(),
		OrgsToDelete:  stageData.GetOrgsToDelete(),
		Users:         users,
	}
}

func getStagedUser(user *infra_res.User) *gwres.User {
	stagedUser := &gwres.User{}
	stagedUser.Username = user.Username
	stagedUser.Email = user.Email
	stagedUser.DisplayName = user.DisplayName
	stagedUser.FirstName = user.FirstName
	stagedUser.LastName = user.LastName
	stagedUser.MiddleName = user.MiddleName
	stagedUser.AutomateUsername = user.AutomateUsername
	stagedUser.Connector = user.Connector
	stagedUser.IsConflicting = user.IsConflicting
	stagedUser.IsAdmin = user.IsAdmin
	return stagedUser
}

// ConfirmPreview trigger the pipline function
func (a *InfraProxyMigrationServer) ConfirmPreview(ctx context.Context, r *gwreq.ConfirmPreviewRequest) (*gwres.ConfirmPreviewResponse, error) {
	req := &infra_req.ConfirmPreviewRequest{
		MigrationId: r.MigrationId,
	}
	res, err := a.migrationClient.ConfirmPreview(ctx, req)
	if err != nil {
		return nil, err
	}

	return &gwres.ConfirmPreviewResponse{
		MigrationId: res.MigrationId,
	}, nil
}
