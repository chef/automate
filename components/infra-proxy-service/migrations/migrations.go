package migrations

import (
	"archive/zip"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"math/rand"
	"os"
	"path"
	"path/filepath"
	"time"

	"golang.org/x/crypto/bcrypt"
	"google.golang.org/grpc/metadata"

	"github.com/chef/automate/api/interservice/infra_proxy/migrations/request"
	"github.com/chef/automate/api/interservice/infra_proxy/migrations/response"
	"github.com/chef/automate/api/interservice/infra_proxy/migrations/service"
	"github.com/chef/automate/components/infra-proxy-service/constants"
	"github.com/chef/automate/components/infra-proxy-service/pipeline"
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
			ErrSendAndClose(migrationId, err)
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
				ErrSendAndClose(migrationId, err)
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
				ErrSendAndClose(migrationId, err)
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
			ErrSendAndClose(migrationId, err)
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
	stagedUser.HashPassword = user.HashPassword
	stagedUser.ActionOps = int32(user.ActionOps)
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
	err := validation.New(validation.Options{
		Target:          "server",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
	}

	md, _ := metadata.FromIncomingContext(ctx)

	migrationStage, err := s.service.Migration.GetMigrationStage(ctx, req.MigrationId)
	if err != nil {
		return nil, err
	}

	migrationStage.StagedData.ParsedResult.Users = SetStagedUserForConfirmPreview(req.StagedData.Users)

	// call pipeline function to trigger the phase 2 pipeline
	go s.phaseTwoPipeline.Run(md, migrationStage.StagedData, s.service)

	return &response.ConfirmPreview{
		MigrationId: req.MigrationId,
	}, nil
}

// To avoide duplicate logs
func ErrSendAndClose(migrationId string, err error) {
	log.Errorf("Failed to send and close stream file for migration id %s : %s", migrationId, err.Error())
}

func SetStagedUserForConfirmPreview(users []*request.User) []pipeline_model.User {
	usersData := []pipeline_model.User{}

	for _, user := range users {
		stagedUser := pipeline_model.User{}
		stagedUser.Username = user.Username
		stagedUser.Email = user.Email
		stagedUser.DisplayName = user.DisplayName
		stagedUser.FirstName = user.FirstName
		stagedUser.LastName = user.LastName
		stagedUser.MiddleName = user.MiddleName
		stagedUser.AutomateUsername = user.AutomateUsername
		stagedUser.Connector = user.Connector
		stagedUser.IsConflicting = user.IsConflicting
		stagedUser.ActionOps = pipeline_model.ActionOps(user.ActionOps)
		stagedUser.HashPassword = user.HashPassword
		usersData = append(usersData, stagedUser)
	}
	return usersData
}

// CreateBackup Creates sample knife ec back up file
func (s *MigrationServer) CreateBackup(ctx context.Context, req *request.CreateBackupRequest) (*response.CreateBackupResponse, error) {

	// Create org directory
	orgPath := path.Join("./backup/organizations/", req.OrgId, "groups")
	if _, err := os.Stat(orgPath); os.IsNotExist(err) {
		err := os.MkdirAll(orgPath, os.ModePerm)
		if err != nil {
			log.Errorf("Unable to create directory %s", err.Error())
			return nil, err
		}
	}
	//defer os.RemoveAll("./backup")

	// Create data for org file org.json
	orgJsonObj := pipeline.OrgJson{}
	orgJsonObj.Name = req.OrgId
	orgJsonObj.FullName = req.OrgId + time.Now().String()
	orgJson, err := json.Marshal(orgJsonObj)
	if err != nil {
		log.Errorf("Failed to marshal %s ", err.Error())
		return nil, err
	}
	err = writeFile(path.Join("./backup/organizations", req.OrgId, "org.json"), orgJson)
	if err != nil {
		log.Errorf("Failed to write org file %s", err.Error())
		return nil, err
	}

	// Create an object for key_dump.json
	serverUsers := []pipeline.KeyDump{}

	if _, err := os.Stat(path.Join("./backup", "key_dump.json")); err == nil {
		log.Info("File exist check please")
		data, err := ioutil.ReadFile(path.Join("./backup", "key_dump.json"))
		if err != nil {
			return nil, err
		}
		err = json.Unmarshal(data, &serverUsers)
		if err != nil {
			return nil, err
		}
	}

	// Create an object for members.json
	members := []pipeline.MembersJson{}

	// Create an object for admins.json
	admins := pipeline.AdminsJson{}
	admins.Name = "admins"
	admins.Users = append(admins.Users, "pivotal")

	// Random generator to set the values for ExternalAuthenticationUID and admins randomly
	rand.Seed(time.Now().Unix())
	users := []string{pipeline.Local, pipeline.LDAP}
	isAdmins := []bool{true, false}

	for i := 0; i < int(req.NumberOfUsers); i++ {

		// Create an object for key_dump.json
		keyDump := pipeline.KeyDump{}
		nowTime := time.Now().Format("20060102150405")
		keyDump.ID = "user_" + uuid.Must(uuid.NewV4()).String()

		keyDump.Username = keyDump.ID
		keyDump.Email = keyDump.ID + "@email.com"
		keyDump.SerializedObject = "{\"first_name\":\"f_" + keyDump.ID + "\",\"last_name\":\"l_" + keyDump.ID + "\",\"display_name\":\"f_" + keyDump.ID + " l_" + keyDump.ID + "\"}"
		keyDump.CreatedAt = nowTime
		keyDump.UpdatedAt = nowTime
		user := users[rand.Intn(len(users))]
		if user == pipeline.LDAP {
			keyDump.ExternalAuthenticationUID = keyDump.Username
		}
		keyDump.Admin = isAdmins[rand.Intn(len(isAdmins))]
		hashedPassword, err := bcrypt.GenerateFromPassword([]byte("password"), 10)
		if err != nil {
			log.Errorf("Unable to create hashed password for user %s %s", keyDump.Username, err)
			break
		}
		keyDump.HashedPassword = string(hashedPassword)
		serverUsers = append(serverUsers, keyDump)

		// Create org members
		member := pipeline.MembersJson{
			User: pipeline.UsersJson{
				Username: keyDump.Username,
			},
		}
		members = append(members, member)

		// Add to org admin
		if keyDump.Admin {
			admins.Users = append(admins.Users, keyDump.Username)
		}
	}

	// Write server users file key_dump.json
	keyDumpJson, err := json.Marshal(serverUsers)
	if err != nil {
		log.Errorf("Failed to marshal %s", err.Error())
		return nil, err
	}
	err = writeFile(path.Join("./backup", "key_dump.json"), keyDumpJson)
	if err != nil {
		log.Errorf("Failed to write server users file %s", err.Error())
		return nil, err
	}

	// Write key_dump.json file
	membersJson, err := json.Marshal(members)
	if err != nil {
		log.Errorf("Failed to marshal %s", err.Error())
		return nil, err
	}
	err = writeFile(path.Join("./backup/organizations", req.OrgId, "members.json"), membersJson)
	if err != nil {
		log.Errorf("Failed to write server users file %s", err.Error())
		return nil, err
	}

	adminsJson, err := json.Marshal(admins)
	if err != nil {
		log.Errorf("Failed to marshal %s", err.Error())
		return nil, err
	}
	err = writeFile(path.Join("./backup/organizations", req.OrgId, "groups", "admins.json"), adminsJson)
	if err != nil {
		log.Errorf("Failed to write server users file %s", err.Error())
		return nil, err
	}

	// Create zip of backup
	err = zipBackUp("./backup", "./backup.zip")
	if err != nil {
		log.Errorf("Failed to create zip of back up file %s", err.Error())
		return nil, err
	}
	return &response.CreateBackupResponse{}, nil
}

func createFile(path string) error {
	if _, err := os.Stat(path); errors.Is(err, os.ErrNotExist) {
		_, err := os.Create(path)
		if err != nil {
			log.WithError(err).Error("Unable to create file ", err)
			return err
		}
	}
	return nil
}

func writeFile(path string, data []byte) error {
	if _, err := os.Stat(path); os.IsNotExist(err) {
		err = createFile(path)
		if err != nil {
			log.Errorf("Unable to create file %s", err.Error())
			return err
		}
	}
	err := ioutil.WriteFile(path, data, 0644)
	if err != nil {
		log.Errorf("Unable to write file %s", err.Error())
		return err
	}
	return nil
}

func zipBackUp(source, target string) error {
	// Create a writeMembersFileZIP file and zip.Writer
	f, err := os.Create(target)
	if err != nil {
		return err
	}
	defer func() {
		_ = f.Close()
	}()

	writer := zip.NewWriter(f)
	defer func() {
		_ = writer.Close()
	}()
	// Go through all the files of the source
	return filepath.Walk(source, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Create a local file header
		header, err := zip.FileInfoHeader(info)
		if err != nil {
			return err
		}

		// set compression
		header.Method = zip.Deflate

		// Set relative path of a file as the header name
		header.Name, err = filepath.Rel(filepath.Dir(source), path)
		if err != nil {
			return err
		}
		if info.IsDir() {
			header.Name += "/"
		}

		// Create writer for the file header and save content of the file
		headerWriter, err := writer.CreateHeader(header)
		if err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		f, err := os.Open(path)
		if err != nil {
			return err
		}
		defer func() {
			_ = f.Close()
		}()
		_, err = io.Copy(headerWriter, f)
		return err
	})
}
