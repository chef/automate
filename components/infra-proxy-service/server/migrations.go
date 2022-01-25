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
	err := os.Mkdir(migrationId, 0777)
	if err != nil {
		log.WithError(err).Error("Unable to create directory for migration id", migrationId)
		return err
	}
	filePath := path.Join(migrationId, filename)
	file, err := os.Create(filePath)
	if err != nil {
		log.WithError(err).Error("Unable to create file")
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
	var fileName string
	var serverId string

	ctx := context.Background()
	migrationId, err := createMigrationId()
	if err != nil {
		log.WithError(err).Error("Unable to create migration id")
		res := createResponseWithErrors(err, migrationId)
		stream.SendAndClose(res)
		return err
	}

	fileData := bytes.Buffer{}

	for {
		req, err := stream.Recv()

		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}

		serverId = req.ServerId

		_, err = s.service.Migration.StartMigration(ctx, migrationId, serverId)
		if err != nil {
			res := createResponseWithErrors(err, migrationId)
			stream.SendAndClose(res)
			return err
		}

		fileName = req.GetMeta().GetName()
		chunk := req.GetChunk().Data
		_, err = fileData.Write(chunk)
		if err != nil {
			res := createResponseWithErrors(err, migrationId)
			stream.SendAndClose(res)
			return err
		}
	}
	s.service.Migration.StartFileUpload(ctx, migrationId, serverId)
	err = saveFile(migrationId, fileName, fileData)

	if err != nil {
		res := createResponseWithErrors(err, migrationId)
		stream.SendAndClose(res)
		return err
	}

	res := &response.UploadZipFileResponse{
		MigrationId: migrationId,
		Success:     true,
	}
	s.service.Migration.CompleteFileUpload(ctx, migrationId, serverId, 0, 0, 0)

	err = stream.SendAndClose(res)
	if err != nil {
		createResponseWithErrors(err, migrationId)
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

func createResponseWithErrors(err error, migrationId string) *response.UploadZipFileResponse {
	errors := []string{err.Error()}
	return &response.UploadZipFileResponse{
		Success:     false,
		MigrationId: migrationId,
		Errors:      errors,
	}
}
