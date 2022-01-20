package server

import (
	"bytes"
	uuid "github.com/chef/automate/lib/uuid4"
	log "github.com/sirupsen/logrus"
	"io"
	"os"
	"path"

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
func (*Server) UploadFile(stream service.MigrationDataService_UploadFileServer) error {
	in, err := stream.Recv()
	if err != io.EOF && err != nil {
		return err
	}
	migrationId, err := createMigrationId()
	if err != nil {
		log.WithError(err).Error("Unable to create migration id")
	}
	folderName := in.GetMeta().Name
	fileData := bytes.Buffer{}
	for {
		req, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		chunk := req.GetChunk().Data

		_, err = fileData.Write(chunk)
		if err != nil {
			return err
		}
	}
	err = saveFile(migrationId, folderName, fileData)
	if err != nil {
		return err
	}

	res := &response.UploadFileResponse{
		MigrationId: migrationId,
		Status:      "Completed",
	}

	err = stream.SendAndClose(res)
	if err != nil {
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
