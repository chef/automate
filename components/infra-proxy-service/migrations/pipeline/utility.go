package pipeline

import (
	"archive/zip"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/infra-proxy-service/pipeline"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	log "github.com/sirupsen/logrus"
)

// StoreOrgs reads the Result struct and populate the orgs table
func StoreOrgs(ctx context.Context, st storage.Storage, mst storage.MigrationStorage, authzProjectClient authz.ProjectsServiceClient, res pipeline.Result) (pipeline.Result, error) {
	var err error
	var msg string
	var totalSucceeded, totalSkipped, totalFailed int64
	_, err = mst.StartOrgMigration(ctx, res.Meta.MigrationID, res.Meta.ServerID)
	if err != nil {
		return res, err
	}
	log.Info("Starting the organisation migration phase for migration id: ", res.Meta.MigrationID)
	for _, org := range res.ParsedResult.Orgs {
		err, _ = StoreOrg(ctx, st, org, res.Meta.ServerID, authzProjectClient)
		if err != nil {
			totalFailed++
			msg = err.Error()
			continue
		}
		if org.ActionOps == pipeline.Skip {
			totalSkipped++
			continue
		}
		totalSucceeded++
	}
	if len(res.ParsedResult.Orgs) == int(totalFailed) {
		log.Errorf("Failed to migrate orgs for migration id %s : %s", res.Meta.MigrationID, err.Error())
		_, _ = mst.FailedOrgMigration(ctx, res.Meta.MigrationID, res.Meta.ServerID, msg, totalSucceeded, totalSkipped, totalFailed)
		return res, err
	}
	_, err = mst.CompleteOrgMigration(ctx, res.Meta.MigrationID, res.Meta.ServerID, totalSucceeded, totalSkipped, totalFailed)
	if err != nil {
		log.Errorf("Failed to update the status for migration id %s : %s", res.Meta.MigrationID, err.Error())
		return res, err
	}
	log.Info("Successfully completed the organisation migration phase for migration id: ", res.Meta.MigrationID)
	return res, err
}

// StoreOrg stores a single Org into DB
func StoreOrg(ctx context.Context, st storage.Storage, org pipeline.Org, serverID string, authzProjectClient authz.ProjectsServiceClient) (error, pipeline.ActionOps) {
	var actionTaken pipeline.ActionOps
	var err error
	switch org.ActionOps {
	case pipeline.Insert:
		projects, err := createProjectFromOrgIdAndServerID(ctx, serverID, org.Name, authzProjectClient)
		if err != nil {
			log.Errorf("Unable to create project for serverid: %s", serverID)
			return err, actionTaken
		}
		_, err = st.StoreOrg(ctx, org.Name, org.FullName, "", "", serverID, projects)
		if err != nil {
			log.Errorf("Unable to insert org for server id: %s", serverID)
		}
		actionTaken = pipeline.Insert
	case pipeline.Delete:
		_, err = st.DeleteOrg(ctx, org.Name, serverID)
		actionTaken = pipeline.Delete
	case pipeline.Update:
		_, err = st.EditOrg(ctx, org.Name, org.FullName, "", serverID, nil)
		actionTaken = pipeline.Update
	default:
	}
	return err, actionTaken
}

//function to create a new iam project for each client
func createProjectFromOrgIdAndServerID(ctx context.Context, serverId string, orgId string, authzProjectClient authz.ProjectsServiceClient) ([]string, error) {

	newProject := &authz.CreateProjectReq{
		Name:         serverId + "_" + orgId,
		Id:           serverId + "_" + orgId,
		SkipPolicies: false,
	}

	projectID, err := authzProjectClient.CreateProject(ctx, newProject)
	if err != nil {
		return nil, err
	}

	return []string{projectID.Project.Name}, nil
}

func ParseOrgs(ctx context.Context, st storage.Storage, mst storage.MigrationStorage, result pipeline.Result) (pipeline.Result, error) {
	var err error
	log.Info("Starting with organisation parsing phase for migration id: ", result.Meta.MigrationID)
	_, err = mst.StartOrgParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID)
	if err != nil {
		log.Errorf("Failed to update the status for start org parssing for the migration id path %s : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}
	orgPath := path.Join(result.Meta.UnzipFolder, "organizations")
	folder, err := os.Open(orgPath)
	if err != nil {
		log.Errorf("Failed to open the folder for the file path %s : %s", orgPath, err.Error())
		_, _ = mst.FailedOrgParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		return result, err
	}

	orgNames, err := folder.Readdir(0)
	if err != nil {
		log.Errorf("Failed to read the files for the file path %s : %s", orgPath, err.Error())
		_, _ = mst.FailedOrgParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		return result, err
	}
	_ = folder.Close()
	orgsPresentInDB, err := st.GetOrgs(ctx, result.Meta.ServerID)
	if err != nil {
		log.Errorf("Failed to read orgs from database for %s:%s", result.Meta.ServerID, err.Error())
		_, _ = mst.FailedOrgParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		return result, err
	}

	result.ParsedResult.Orgs = append(result.ParsedResult.Orgs, insertOrUpdateOrg(orgNames, orgsPresentInDB, orgPath)...)

	result.ParsedResult.Orgs = append(result.ParsedResult.Orgs, deleteOrgsIfNotPresentInCurrentFile(orgNames, orgsPresentInDB)...)

	_, err = mst.CompleteOrgParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID, 0, 0, 0)
	if err != nil {
		log.Errorf("Failed to update the complete status while parsing for migration id %s : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}

	log.Info("Successfully completed the organisation parsing phase for migration id: ", result.Meta.MigrationID)
	return result, nil

}

//CreatePreview Stores the staged data in db
func CreatePreview(ctx context.Context, st storage.Storage, mst storage.MigrationStorage, result pipeline.Result) (pipeline.Result, error) {
	log.Info("Starting with create preview phase for migration id: ", result.Meta.MigrationID)

	_, err := mst.StartCreatePreview(ctx, result.Meta.MigrationID, result.Meta.ServerID)
	if err != nil {
		log.Errorf("Failed to update the status for create preview for the migration id  %s : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}

	resultByte, err := json.Marshal(result)
	if err != nil {
		log.Errorf("Failed to marshal the staged data for the migration id %s : %s", result.Meta.MigrationID, err.Error())
		_, _ = mst.FailedCreatePreview(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		return result, err
	}

	_, err = mst.StoreMigrationStage(ctx, result.Meta.MigrationID, resultByte)
	if err != nil {
		log.Errorf("Failed to store the staged data %s : %s ", result.Meta.MigrationID, err.Error())
		_, _ = mst.FailedCreatePreview(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		return result, err
	}
	_, err = mst.CompleteCreatePreview(ctx, result.Meta.MigrationID, result.Meta.ServerID, 0, 0, 0)
	if err != nil {
		log.Errorf("Failed to update the complete status while creating preview for migration id %s : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}
	log.Info("Successfully completed the create preview phase for migration id: ", result.Meta.MigrationID)

	return result, nil
}

func createDatabaseOrgsMap(orgs []storage.Org) map[string]string {
	orgMap := make(map[string]string)
	for _, s := range orgs {
		orgMap[s.ID] = s.Name
		//Values for comparison
	}
	return orgMap
}

func createFileOrgsMap(orgs []os.FileInfo) map[string]string {
	orgMap := make(map[string]string)
	for _, s := range orgs {
		if s.IsDir() {
			orgMap[s.Name()] = ""
			//No value required for comparison
		}
	}
	return orgMap
}

func insertOrUpdateOrg(orgsInFiles []os.FileInfo, orgsInDB []storage.Org, orgPath string) []pipeline.Org {
	var orgList []pipeline.Org
	orgDatabaseMap := createDatabaseOrgsMap(orgsInDB)
	var orgJson pipeline.OrgJson
	log.Info("Comparing the organisations from database and backup file for insert,update and skip action")
	//For insert, update and skip action
	for _, org := range orgsInFiles {
		if org.IsDir() {
			orgInfo, valuePresent := orgDatabaseMap[org.Name()]
			orgJson = openOrgFolder(org, orgPath)
			if valuePresent {
				if orgJson.FullName != orgInfo {
					//Update org in the result actions
					orgList = append(orgList, createOrgStructForAction(orgJson.Name, orgJson.FullName, pipeline.Update))
				} else {
					//Skip org action if full names are not equal
					orgList = append(orgList, createOrgStructForAction(orgJson.Name, orgJson.FullName, pipeline.Skip))

				}
			} else {
				//Insert org action if not present in database
				orgList = append(orgList, createOrgStructForAction(orgJson.Name, orgJson.FullName, pipeline.Insert))
			}
		}
	}
	log.Info("Completed comparing the organisations from database and backup file for insert,update and skip action")
	return orgList
}

func deleteOrgsIfNotPresentInCurrentFile(orgsInFiles []os.FileInfo, orgsInDB []storage.Org) []pipeline.Org {
	var orgList []pipeline.Org
	orgFilesMap := createFileOrgsMap(orgsInFiles)
	log.Info("Comparing the organisations from database and backup file for delete action")
	//For delete action by comparing database orgs with file orgs
	for _, org := range orgsInDB {
		_, valuePresent := orgFilesMap[org.ID]
		if !valuePresent {
			orgList = append(orgList, createOrgStructForAction(org.ID, org.Name, pipeline.Delete))
		}
	}
	log.Info("Completed comparing the organisations from database and backup file for delete action")
	return orgList
}

func openOrgFolder(org os.FileInfo, fileLocation string) pipeline.OrgJson {
	var orgJson pipeline.OrgJson
	jsonPath := path.Join(fileLocation, org.Name(), "org.json")
	jsonFile, err := os.Open(jsonPath)
	// if we os.Open returns an error then handle it
	if err != nil {
		fmt.Println(err)
	}
	log.Info("Successfully opened the file at location", jsonPath)
	defer func() {
		_ = jsonFile.Close()
	}()
	// defer the closing of our jsonFile so that we can parse it later on
	_ = json.NewDecoder(jsonFile).Decode(&orgJson)
	return orgJson
}

func createOrgStructForAction(orgId string, orgName string, ops pipeline.ActionOps) pipeline.Org {
	return pipeline.Org{
		Name:      orgId,
		FullName:  orgName,
		ActionOps: ops,
	}
}

// Unzip will decompress a zip file and sets the UnzipFolder
func Unzip(ctx context.Context, mst storage.MigrationStorage, result pipeline.Result) (pipeline.Result, error) {

	var fpath string
	_, err := mst.StartUnzip(ctx, result.Meta.MigrationID, result.Meta.ServerID)
	if err != nil {
		log.Errorf("Failed to update status in DB: %s :%s", result.Meta.MigrationID, err)
	}

	reader, err := zip.OpenReader(result.Meta.ZipFile)
	if err != nil {
		log.Errorf("cannot open reader: %s.", err)
		_, err := mst.FailedUnzip(ctx, result.Meta.MigrationID, result.Meta.ServerID, "cannot open zipfile", 0, 0, 0)
		if err != nil {
			log.Errorf("Failed to update status in DB: %s", err)
		}
		return result, err
	}

	for _, file := range reader.File {

		fpath = filepath.Join(filepath.Dir(result.Meta.ZipFile), file.Name)

		if file.FileInfo().IsDir() {
			os.MkdirAll(fpath, os.ModePerm)
			continue
		}

		// Creating the files in the target directory
		if err = os.MkdirAll(filepath.Dir(fpath), os.ModePerm); err != nil {
			log.Errorf("cannot create directory: %s. ", err)
			mst.FailedUnzip(ctx, result.Meta.MigrationID, result.Meta.ServerID, "cannot create directory", 0, 0, 0)
			return result, err
		}

		// The created file will be stored in
		// outFile with permissions to write &/or truncate
		outFile, err := os.OpenFile(fpath,
			os.O_WRONLY|os.O_CREATE|os.O_TRUNC,
			file.Mode())
		if err != nil {
			log.Errorf("cannot create a file: %s.", err)
			mst.FailedUnzip(ctx, result.Meta.MigrationID, result.Meta.ServerID, "cannot create a file", 0, 0, 0)
			return result, err
		}

		readClose, err := file.Open()
		if err != nil {
			log.Errorf("cannot open file")
			mst.FailedUnzip(ctx, result.Meta.MigrationID, result.Meta.ServerID, "cannot open file", 0, 0, 0)
			return result, err
		}

		_, err = io.Copy(outFile, readClose)
		if err != nil {
			log.Errorf("cannot copy file")
			mst.FailedUnzip(ctx, result.Meta.MigrationID, result.Meta.ServerID, "cannot copy file", 0, 0, 0)
			return result, err
		}

		outFile.Close()
		readClose.Close()

		if err != nil {
			log.Errorf("cannot copy a file")
			mst.FailedUnzip(ctx, result.Meta.MigrationID, result.Meta.ServerID, "cannot copy a file", 0, 0, 0)
			return result, err
		}
	}

	result.Meta.UnzipFolder = filepath.Dir(fpath)
	reader.Close()
	_, err = mst.CompleteUnzip(ctx, result.Meta.MigrationID, result.Meta.ServerID, 0, 0, 0)
	if err != nil {
		log.Errorf("Failed to update status in DB: %s :%s", result.Meta.MigrationID, err)
	}
	return result, nil
}

func GetUsersForBackup(ctx context.Context, st storage.Storage, mst storage.MigrationStorage, result pipeline.Result) (pipeline.Result, error) {
	log.Info("starting with user parseing phase for migration id: ", result.Meta.MigrationID)

	_, err := mst.StartUsersParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID)
	if err != nil {
		log.Error("failed to update the status for user parsing for the migration id: %s, error : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}
	file := path.Join(result.Meta.UnzipFolder, "key_dump.json")

	keyDumpByte, err := ioutil.ReadFile(file)
	if err != nil {
		log.Error("failed to read keydump file for user parsing for the migration id: %s, error : %s", result.Meta.MigrationID, err.Error())
		mst.FailedUsersParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		return result, err
	}

	var keyDumps []pipeline.KeyDump
	if err := json.Unmarshal(keyDumpByte, &keyDumps); err != nil {
		log.Error("failed to unmarshal for user parsing for the migration id: %s, error : %s", result.Meta.MigrationID, err.Error())
		mst.FailedUsersParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		return result, err
	}

	serverUsers := keyDumpTOUser(keyDumps)
	automateUsers, err := st.GetUsers(ctx, result.Meta.ServerID)
	if err != nil {
		log.Error("failed to read keydump file for user parsing for the migration id: %s, error : %s", result.Meta.MigrationID, err.Error())
		mst.FailedUsersParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		return result, err
	}

	mappedUsers := MapUsers(serverUsers, automateUsers)
	result.ParsedResult.Users = mappedUsers
	return result, nil
}

// Clean serialized_object and Polulate Users struct
func keyDumpTOUser(keyDump []pipeline.KeyDump) []pipeline.User {
	var users []pipeline.User
	for _, kd := range keyDump {
		sec := map[string]string{}
		if err := json.Unmarshal([]byte(kd.SerializedObject), &sec); err != nil {
			panic(err)
		}
		user := pipeline.User{
			Username:    kd.Username,
			Email:       kd.Email,
			DisplayName: sec["display_name"],
			FirstName:   sec["first_name"],
			LastName:    sec["last_name"],
			MiddleName:  sec["middle_name"],
		}
		users = append(users, user)
	}
	return users
}

func MapUsers(serverUser []pipeline.User, automateUser []storage.User) []pipeline.User {
	var serverCleanUser []pipeline.User
	for _, aUser := range automateUser {
		if !IsExisting(serverUser, aUser.InfraServerUsername) {
			serverCleanUser = append(serverCleanUser, pipeline.User{
				Username:  aUser.InfraServerUsername,
				ActionOps: 3,
			})
		} else {
			for _, sUser := range serverUser {
				if sUser.Username == aUser.InfraServerUsername {
					if sUser.FirstName == aUser.FirstName && sUser.Email == aUser.Email {
						sUser.ActionOps = 2
						serverCleanUser = append(serverCleanUser, sUser)

					} else {
						sUser.ActionOps = 4
						serverCleanUser = append(serverCleanUser, sUser)
					}

				} else {
					sUser.ActionOps = 1
					serverCleanUser = append(serverCleanUser, sUser)
				}
			}
		}
	}
	return serverCleanUser
}

func IsExisting(users []pipeline.User, user string) bool {
	for _, v := range users {
		if user == v.Username {
			return true
		}
	}
	return false
}
