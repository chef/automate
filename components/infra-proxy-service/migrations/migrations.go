package migrations

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"os"
	"path"

	"github.com/chef/automate/api/interservice/infra_proxy/migrations/request"
	"github.com/chef/automate/api/interservice/infra_proxy/migrations/response"
	"github.com/chef/automate/api/interservice/infra_proxy/migrations/service"
	"github.com/chef/automate/components/infra-proxy-service/constants"
	pipeline_model "github.com/chef/automate/components/infra-proxy-service/pipeline"

	"github.com/chef/automate/components/infra-proxy-service/validation"
	"github.com/gofrs/uuid"
	log "github.com/sirupsen/logrus"
)

// UploadFile Takes the stream of data to upload a file
func (s *MigrationServer) UploadFile(stream service.MigrationDataService_UploadFileServer) error {
	log.Info("Starting the with the request to upload file")
	req, err := stream.Recv()
	serverId := req.ServerId
	fileName := req.GetMeta().GetName()
	ctx := context.Background()
	migrationId, err := createMigrationId()
	if err != nil {
		log.WithError(err).Error("Unable to create migration id")
		res := handleErrorForUploadFileAndMigration(err, migrationId, serverId, s, ctx)
		errStream := stream.SendAndClose(res)
		if errStream != nil {
			log.Errorf(constants.FailedSendAndCloseStream, migrationId, err.Error())
		}
		return err
	}
	log.Info("Starting with migration phase with the upload file for migration id: ", migrationId)
	_, err = s.service.Migration.StartMigration(ctx, migrationId, serverId)
	if err != nil {
		log.Errorf("Unable to insert the migration status Start Migration for  migration id : %s", migrationId)
		return err
	}
	fileData := bytes.Buffer{}
	_, err = s.service.Migration.StartFileUpload(ctx, migrationId, serverId)
	if err != nil {
		log.Errorf("Unable to insert the migration status Start File upload for  migration id : %s", migrationId)
		return err
	}
	for {
		req, err := stream.Recv()

		if err == io.EOF {
			break
		}

		if err != nil {
			log.Errorf("Failed to upload file for migration id %s : %s", migrationId, err.Error())
			res := handleErrorForUploadFileAndMigration(err, migrationId, serverId, s, ctx)
			errStream := stream.SendAndClose(res)
			if errStream != nil {
				log.Errorf(constants.FailedSendAndCloseStream, migrationId, err.Error())
			}
			return err
		}

		chunk := req.GetChunk().Data
		_, err = fileData.Write(chunk)
		if err != nil {
			log.Errorf("Failed to upload file for migration id %s : %s", migrationId, err.Error())
			res := handleErrorForUploadFileAndMigration(err, migrationId, serverId, s, ctx)
			errStream := stream.SendAndClose(res)
			if errStream != nil {
				log.Errorf(constants.FailedSendAndCloseStream, migrationId, err.Error())
			}
			return err
		}
	}

	folderpath, err := saveFile(migrationId, fileName, fileData)
	if err != nil {
		log.Errorf("Failed to save uploaded file for migration id %s : %s", migrationId, err.Error())
		res := handleErrorForUploadFileAndMigration(err, migrationId, serverId, s, ctx)
		errStream := stream.SendAndClose(res)
		if errStream != nil {
			log.Errorf(constants.FailedSendAndCloseStream, migrationId, err.Error())
		}
		return err
	}
	log.Info("File successfully saved in the directory for the requested file for migration id: ", migrationId)

	res := &response.UploadFileResponse{
		MigrationId: migrationId,
		Success:     true,
	}
	_, _ = s.service.Migration.CompleteFileUpload(ctx, migrationId, serverId, 0, 0, 0)
	log.Info("File successfully uploaded in the directory for the requested file for migration id: ", migrationId)
	err = stream.SendAndClose(res)
	if err != nil {
		handleErrorForUploadFileAndMigration(err, migrationId, serverId, s, ctx)
		log.Errorf("Failed to send the response for migration id %s : %s", migrationId, err.Error())
		return err
	}

	pipelineResult := pipeline_model.Result{Meta: pipeline_model.Meta{ZipFile: folderpath, MigrationID: migrationId, ServerID: serverId}}
	go s.phaseOnePipeline.Run(pipelineResult, s.service)
	return nil
}

// GetMigrationStatus fetch migration status against migration id
func (s *MigrationServer) GetMigrationStatus(ctx context.Context, req *request.GetMigrationStatusRequest) (*response.GetMigrationStatusResponse, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// Validate all request fields are required
	err := validation.New(validation.Options{
		Target:          "server",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
	}

	migration, err := s.service.Migration.GetMigrationStatus(ctx, req.MigrationId)
	if err != nil {
		return nil, err
	}

	return &response.GetMigrationStatusResponse{
		MigrationId:     migration.MigrationID,
		MigrationType:   migration.MigrationType,
		MigrationStatus: migration.MigrationStatus,
	}, nil
}

// Takes up file name from service.MigrationDataService_UploadFileServer.MigrationId and creates the file in the same directory
func saveFile(migrationId string, filename string, fileData bytes.Buffer) (string, error) {
	folderPath := path.Join("/hab/svc/infra-proxy-service/data", migrationId)
	err := os.Mkdir(folderPath, 0777)
	if err != nil {
		log.WithError(err).Error("Unable to create directory for migration id", migrationId)
		return "", err
	}
	filePath := path.Join(folderPath, filename)
	file, err := os.Create(filePath)
	if err != nil {
		log.WithError(err).Error("Unable to create zipped file for migration id", migrationId)
		return "", err
	}
	_, err = fileData.WriteTo(file)
	if err != nil {
		return "", err
	}
	return filePath, nil

}

func createMigrationId() (string, error) {
	uuid, err := uuid.NewV4()
	if err != nil {
		log.WithError(err).Error("Failed to make a V4 UUID")
		return "", err
	}
	return uuid.String(), nil
}

//handleErrorForUploadFileAndMigration handles the error for the file upload
func handleErrorForUploadFileAndMigration(err error, migrationId string, serviceId string, s *MigrationServer, ctx context.Context) *response.UploadFileResponse {
	response := createResponseWithErrors(err, migrationId)
	_, _ = s.service.Migration.FailedFileUpload(ctx, migrationId, serviceId, err.Error(), 0, 0, 0)
	//ToDo to add the Failed migration status as well
	_, _ = s.service.Migration.FailedMigration(ctx, migrationId, serviceId, err.Error(), 0, 0, 0)
	return response

}

//createResponseWithErrors created a response with errors
func createResponseWithErrors(err error, migrationId string) *response.UploadFileResponse {
	errors := []string{err.Error()}
	return &response.UploadFileResponse{
		Success:     false,
		MigrationId: migrationId,
		Errors:      errors,
	}
}

// CancelMigration cancel the ongoing migration
func (s *MigrationServer) CancelMigration(ctx context.Context, req *request.CancelMigrationRequest) (*response.CancelMigrationResponse, error) {

	// Cancellation is allowed for a running pipeline only if the parsing is done but the data commitment is yet to be performed
	currentMigrationPhase, err := s.service.Migration.GetMigrationStatus(ctx, req.MigrationId)
	if err != nil {
		return nil, err
	}
	if currentMigrationPhase.MigrationTypeID != int64(constants.CreatePreview) {
		return nil, fmt.Errorf("cancellation is not allowed when migration is not in Create Preview State: %s", req.MigrationId)
	}

	//  Remove the migration folder
	folderPath := path.Join("/hab/svc/infra-proxy-service/data", req.MigrationId)
	err = os.RemoveAll(folderPath)
	if err != nil {
		_, _ = s.service.Migration.FailedCancelMigration(ctx, req.MigrationId, req.ServerId, err.Error(), 0, 0, 0)
		return nil, err
	}

	// Clear up the stage table
	_, err = s.service.Migration.DeleteMigrationStage(ctx, req.MigrationId)
	if err != nil {
		_, _ = s.service.Migration.FailedCancelMigration(ctx, req.MigrationId, req.ServerId, err.Error(), 0, 0, 0)
		return nil, err
	}

	// Update the migration status
	_, err = s.service.Migration.CancelMigration(ctx, req.MigrationId, req.ServerId, 0, 0, 0)
	if err != nil {
		_, _ = s.service.Migration.FailedCancelMigration(ctx, req.MigrationId, req.ServerId, err.Error(), 0, 0, 0)
		return nil, err
	}

	//TODO: Remove errors array from response, as we are not using it
	//errors := []string{err.Error()}

	return &response.CancelMigrationResponse{
		Success: true,
		Errors:  []string{},
	}, nil
}

// GetStagedData fetch parsed data from db
func (s *MigrationServer) GetStagedData(ctx context.Context, req *request.GetStagedDataRequest) (*response.GetStagedDataResponse, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// Validate all request fields are required
	err := validation.New(validation.Options{
		Target:          "server",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
	}

	migrationStage, err := s.service.Migration.GetMigrationStage(ctx, req.MigrationId)
	if err != nil {
		return nil, err
	}

	return &response.GetStagedDataResponse{
		MigrationId: migrationStage.MigrationID,
		StagedData:  getStagedData(migrationStage.StagedData),
	}, nil
}

func getStagedData(stagedResult pipeline_model.Result) *response.StagedData {
	stageData := &response.StagedData{}
	for _, org := range stagedResult.ParsedResult.Orgs {
		switch org.ActionOps {
		case pipeline_model.Insert:
			stageData.OrgsToMigrate++
		case pipeline_model.Delete:
			stageData.OrgsToDelete++
		case pipeline_model.Update:
			stageData.OrgsToUpdate++
		case pipeline_model.Skip:
			stageData.OrgsToSkip++
		default:
		}
	}
	users := []*response.User{}
	for _, user := range stagedResult.ParsedResult.Users {
		users = append(users, getStagedUser(user))
	}
	stageData.Users = users
	return stageData
}

func getStagedUser(user pipeline_model.User) *response.User {
	stagedUser := &response.User{}
	stagedUser.Username = user.Username
	stagedUser.Email = user.Email
	stagedUser.DisplayName = user.DisplayName
	stagedUser.FirstName = user.FirstName
	stagedUser.LastName = user.LastName
	stagedUser.MiddleName = user.MiddleName
	stagedUser.AutomateUsername = user.AutomateUsername
	stagedUser.Connector = user.Connector
	stagedUser.IsConflicting = user.IsConflicting
	return stagedUser
}

// StoreStagedData stores staged data in db
// This function is used for the sample storage of staged data
func (s *MigrationServer) StoreStagedData(ctx context.Context, migrationId string, stagedData interface{}) error {

	_, err := s.service.Migration.StoreMigrationStage(ctx, migrationId, stagedData)
	if err != nil {
		return err
	}

	return nil
}

// ConfirmPreview trigger the preview pipline
func (s *MigrationServer) ConfirmPreview(ctx context.Context, req *request.ConfirmPreview) (*response.ConfirmPreview, error) {
	// Validate all request fields are required
	// err := validation.New(validation.Options{
	// 	Target:          "server",
	// 	Request:         *req,
	// 	RequiredDefault: true,
	// }).Validate()

	// if err != nil {
	// 	return nil, err
	// }

	migrationStage, err := s.service.Migration.GetMigrationStage(ctx, req.MigrationId)
	if err != nil {
		return nil, err
	}

	// call pipeline function to trigger the phase 2 pipeline
	go s.phaseTwoPipeline.Run(migrationStage.StagedData, s.service)

	return &response.ConfirmPreview{
		MigrationId: req.MigrationId,
	}, nil
}
