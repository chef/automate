package server

import (
	"bytes"
	"context"
	"io"
	"os"
	"path"

	uuid "github.com/chef/automate/lib/uuid4"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/api/interservice/infra_proxy/migrations/response"
	"github.com/chef/automate/api/interservice/infra_proxy/migrations/service"
)

// Takes up file name from service.MigrationDataService_UploadFileServer.MigrationId and creates the file in the same directory
func saveFile(migrationId string, filename string, fileData bytes.Buffer) error {
	folderPath := path.Join("/hab/svc/infra-proxy-service/data", migrationId)
	err := os.Mkdir(folderPath, 0777)
	if err != nil {
		log.WithError(err).Error("Unable to create directory for migration id", migrationId)
		return err
	}
	filePath := path.Join(folderPath, filename)
	file, err := os.Create(filePath)
	if err != nil {
		log.WithError(err).Error("Unable to create zipped file for migration id", migrationId)
		return err
	}
	_, err = fileData.WriteTo(file)
	if err != nil {
		return err
	}
	return nil

}

// UploadFile Takes the stream of data to upload a file
func (s *Server) UploadFile(stream service.MigrationDataService_UploadFileServer) error {
	log.Info("Starting the with the request to upload file")
	req, err := stream.Recv()
	serverId := req.ServerId
	fileName := req.GetMeta().GetName()
	ctx := context.Background()
	migrationId, err := createMigrationId()
	if err != nil {
		log.WithError(err).Error("Unable to create migration id")
		res := handleErrorForUploadFileAndMigration(err, migrationId, serverId, s, ctx)
		stream.SendAndClose(res)
		return err
	}
	log.Info("Starting with migration phase with the upload filei for migration id: ", migrationId)
	_, err = s.service.Migration.StartMigration(ctx, migrationId, serverId)
	fileData := bytes.Buffer{}
	s.service.Migration.StartFileUpload(ctx, migrationId, serverId)
	for {
		req, err := stream.Recv()

		if err == io.EOF {
			break
		}

		if err != nil {
			log.Errorf("Failed to upload file for migration id %s : %s", migrationId, err.Error())
			res := handleErrorForUploadFileAndMigration(err, migrationId, serverId, s, ctx)
			stream.SendAndClose(res)
			return err
		}

		chunk := req.GetChunk().Data
		_, err = fileData.Write(chunk)
		if err != nil {
			log.Errorf("Failed to upload file for migration id %s : %s", migrationId, err.Error())
			res := handleErrorForUploadFileAndMigration(err, migrationId, serverId, s, ctx)
			stream.SendAndClose(res)
			return err
		}
	}

	err = saveFile(migrationId, fileName, fileData)
	if err != nil {
		log.Errorf("Failed to save uploaded file for migration id %s : %s", migrationId, err.Error())
		res := handleErrorForUploadFileAndMigration(err, migrationId, serverId, s, ctx)
		stream.SendAndClose(res)
		return err
	}
	log.Info("File successfully saved in the directory for the requested file for migration id: ", migrationId)

	res := &response.UploadZipFileResponse{
		MigrationId: migrationId,
		Success:     true,
	}
	s.service.Migration.CompleteFileUpload(ctx, migrationId, serverId, 0, 0, 0)
	log.Info("File successfully uploaded in the directory for the requested file for migration id: ", migrationId)
	err = stream.SendAndClose(res)
	if err != nil {
		handleErrorForUploadFileAndMigration(err, migrationId, serverId, s, ctx)
		log.Errorf("Failed to send the response for migration id %s : %s", migrationId, err.Error())
		return err
	}

	return nil
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
func handleErrorForUploadFileAndMigration(err error, migrationId string, serviceId string, s *Server, ctx context.Context) *response.UploadZipFileResponse {
	response := createResponseWithErrors(err, migrationId)
	s.service.Migration.FailedFileUpload(ctx, migrationId, serviceId, err.Error(), 0, 0, 0)
	//ToDo to add the Failed migration status as well
	s.service.Migration.FailedMigration(ctx, migrationId, serviceId, err.Error(), 0, 0, 0)
	return response

}

//createResponseWithErrors created a response with errors
func createResponseWithErrors(err error, migrationId string) *response.UploadZipFileResponse {
	errors := []string{err.Error()}
	return &response.UploadZipFileResponse{
		Success:     false,
		MigrationId: migrationId,
		Errors:      errors,
	}
}
