package pipeline

import (
	"archive/zip"
	"context"
	"encoding/json"
	"fmt"

	"errors"
	"io"
	"os"
	"path"
	"path/filepath"

	"github.com/chef/automate/api/interservice/local_user"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/infra-proxy-service/pipeline"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	log "github.com/sirupsen/logrus"
)

// StoreOrgs reads the Result struct and populate the orgs table
func StoreOrgs(ctx context.Context, st storage.Storage, authzProjectClient authz.ProjectsServiceClient, res pipeline.Result) (pipeline.Result, error) {
	var err error

	log.Info("Starting the organization migration phase for migration id: ", res.Meta.MigrationID)
	for _, org := range res.ParsedResult.Orgs {
		if org.ActionOps == pipeline.Skip {
			res.ParsedResult.OrgsCount.Skipped++
			continue
		}
		err, _ = StoreOrg(ctx, st, org, res.Meta.ServerID, authzProjectClient)
		if err != nil {
			res.ParsedResult.OrgsCount.Failed++
			continue
		}
		res.ParsedResult.OrgsCount.Succeeded++
	}
	if len(res.ParsedResult.Orgs) == int(res.ParsedResult.OrgsCount.Failed) {
		log.Errorf("Failed to migrate orgs for migration id %s : %s", res.Meta.MigrationID, err.Error())
		return res, err
	}
	log.Info("Successfully completed the organization migration phase for migration id: ", res.Meta.MigrationID)
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
	log.Info("Starting with organization parsing phase for migration id: ", result.Meta.MigrationID)

	orgPath := path.Join(result.Meta.UnzipFolder, "organizations")
	folder, err := os.Open(orgPath)
	if err != nil {
		log.Errorf("Failed to open the folder for the file path %s : %s", orgPath, err.Error())
		return result, err
	}

	orgNames, err := folder.Readdir(0)
	if err != nil {
		log.Errorf("Failed to read the files for the file path %s : %s", orgPath, err.Error())
		return result, err
	}
	_ = folder.Close()
	orgsPresentInDB, err := st.GetOrgs(ctx, result.Meta.ServerID)
	if err != nil {
		log.Errorf("Failed to read orgs from database for %s:%s", result.Meta.ServerID, err.Error())
		return result, err
	}

	result.ParsedResult.Orgs = append(result.ParsedResult.Orgs, insertOrUpdateOrg(orgNames, orgsPresentInDB, orgPath)...)

	result.ParsedResult.Orgs = append(result.ParsedResult.Orgs, deleteOrgsIfNotPresentInCurrentFile(orgNames, orgsPresentInDB)...)

	log.Info("Successfully completed the organization parsing phase for migration id: ", result.Meta.MigrationID)
	return result, nil

}

//CreatePreview Stores the staged data in db
func CreatePreview(ctx context.Context, st storage.Storage, mst storage.MigrationStorage, result pipeline.Result) (pipeline.Result, error) {
	log.Info("Starting with create preview phase for migration id: ", result.Meta.MigrationID)

	resultByte, err := json.Marshal(result)
	if err != nil {
		log.Errorf("Failed to marshal the staged data for the migration id %s : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}

	_, err = mst.StoreMigrationStage(ctx, result.Meta.MigrationID, resultByte)
	if err != nil {
		log.Errorf("Failed to store the staged data %s : %s ", result.Meta.MigrationID, err.Error())
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
	log.Info("Comparing the organization from database and backup file for insert,update and skip action")
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
	log.Info("Completed comparing the organization from database and backup file for insert,update and skip action")
	return orgList
}

func deleteOrgsIfNotPresentInCurrentFile(orgsInFiles []os.FileInfo, orgsInDB []storage.Org) []pipeline.Org {
	var orgList []pipeline.Org
	orgFilesMap := createFileOrgsMap(orgsInFiles)
	log.Info("Comparing the organization from database and backup file for delete action")
	//For delete action by comparing database orgs with file orgs
	for _, org := range orgsInDB {
		_, valuePresent := orgFilesMap[org.ID]
		if !valuePresent {
			orgList = append(orgList, createOrgStructForAction(org.ID, org.Name, pipeline.Delete))
		}
	}
	log.Info("Completed comparing the organization from database and backup file for delete action")
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
func Unzip(ctx context.Context, result pipeline.Result) (pipeline.Result, error) {

	var fpath string

	reader, err := zip.OpenReader(result.Meta.ZipFile)
	if err != nil {
		log.Errorf("cannot open reader migration id: %s, %s", result.Meta.MigrationID, err.Error())
		return result, err
	}

	for _, file := range reader.File {

		fpath = filepath.Join(filepath.Dir(result.Meta.ZipFile), file.Name)

		if file.FileInfo().IsDir() {
			err = os.MkdirAll(fpath, os.ModePerm)
			if err != nil {
				log.Errorf("cannot create dir for migration id: %s, %s", result.Meta.MigrationID, err.Error())
			}
			continue
		}

		// Creating the files in the target directory
		if err = os.MkdirAll(filepath.Dir(fpath), os.ModePerm); err != nil {
			log.Errorf("cannot create directory for migration id: %s, %s", result.Meta.MigrationID, err.Error())
			return result, err
		}

		// The created file will be stored in
		// outFile with permissions to write &/or truncate
		outFile, err := os.OpenFile(fpath,
			os.O_WRONLY|os.O_CREATE|os.O_TRUNC,
			file.Mode())
		if err != nil {
			log.Errorf("cannot create a file for migration id: %s, %s", result.Meta.MigrationID, err.Error())
			return result, err
		}

		readClose, err := file.Open()
		if err != nil {
			log.Errorf("cannot open file for migration id: %s, %s", result.Meta.MigrationID, err.Error())
			return result, err
		}

		_, err = io.Copy(outFile, readClose)
		if err != nil {
			log.Errorf("cannot copy file for migration id: %s, %s", result.Meta.MigrationID, err.Error())
			return result, err
		}
		_ = outFile.Close()
		_ = readClose.Close()

	}

	result.Meta.UnzipFolder = filepath.Dir(fpath)
	_ = reader.Close()

	return result, nil
}

func ValidateZip(ctx context.Context, st storage.Storage, mst storage.MigrationStorage, result pipeline.Result) (pipeline.Result, error) {
	var err error
	foundOrg := false
	// Find under unzip folder where org folder exists
	if err := filepath.Walk(result.Meta.UnzipFolder, func(path string, f os.FileInfo, err error) error {

		// check if base path is `organizations`
		if filepath.Base(path) == "organizations" {
			// reassign unzipFolder
			foundOrg = true
			result.Meta.UnzipFolder = filepath.Dir(path)
		}
		return nil
	}); err != nil {
		log.Errorf("Failed to find organizations folder for migration id %s : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}
	if !foundOrg {
		return result, errors.New("cannot find organizations folder")
	}
	// Check if the reassigned unzip folder contains key_dump.json
	_, err = os.Stat(result.Meta.UnzipFolder + "/key_dump.json")
	if err != nil {
		log.Errorf("Failed to validate unzip folder for migration id %s : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}

	result.Meta.IsValid = true
	return result, nil
}

// ParseOrgUserAssociation  sync the automate org users with chef server org users
func ParseOrgUserAssociation(ctx context.Context, st storage.Storage, result pipeline.Result) (pipeline.Result, error) {
	log.Info("Starting with the parsing org user association for migration id :", result.Meta.MigrationID)
	var orgUserAssociations []pipeline.OrgsUsersAssociations
	var err error
	orgUserAssociations, err = getActionForOrgUsers(ctx, st, result)
	if err != nil {
		log.Errorf("Unable to parse org user association for migration id : %s : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}
	result.ParsedResult.OrgsUsers = append(result.ParsedResult.OrgsUsers, orgUserAssociations...)
	log.Info("Completed with the parsing org user association for migration id :", result.Meta.MigrationID)
	return result, nil
}

func getActionForOrgUsers(ctx context.Context, st storage.Storage, result pipeline.Result) ([]pipeline.OrgsUsersAssociations, error) {
	orgUserAssociations := make([]pipeline.OrgsUsersAssociations, 0)
	orgPath := path.Join(result.Meta.UnzipFolder, "organizations")
	for _, org := range result.ParsedResult.Orgs {
		var userAssociations []pipeline.UserAssociation
		log.Info("Getting actions for org id", org.Name)
		var chefServerOrgUsers []pipeline.UserAssociation
		// check whether org directory exist or not
		if _, err := os.Stat(path.Join(orgPath, org.Name)); !os.IsNotExist(err) {
			chefServerOrgUsers, err = getChefServerOrgUsers(org.Name, orgPath)
			if err != nil {
				log.Errorf("Unable to get the chef server organization users %s ", err)
				return nil, err
			}
		}
		if org.ActionOps == pipeline.Insert {
			userAssociations = append(userAssociations, createInsertUserAssociation(chefServerOrgUsers)...)
			orgUserAssociations = append(orgUserAssociations, pipeline.OrgsUsersAssociations{OrgName: org, Users: userAssociations})
			continue
		}
		orgUsersInDb, err := st.GetAutomateOrgUsers(ctx, org.Name)
		if err != nil {
			log.Errorf("Unable to fetch automate Users for org %s : %s", org.Name, err.Error())
			return nil, err
		}
		if org.ActionOps == pipeline.Delete {
			userAssociations = append(userAssociations, createDeleteUserAssociation(orgUsersInDb)...)
		} else {
			userAssociations = append(userAssociations, insertOrUpdateActionForOrgUsers(orgUsersInDb, chefServerOrgUsers)...)
			userAssociations = append(userAssociations, deleteActionForOrgUses(orgUsersInDb, chefServerOrgUsers)...)
		}
		orgUserAssociations = append(orgUserAssociations, pipeline.OrgsUsersAssociations{OrgName: org, Users: userAssociations})
	}
	return orgUserAssociations, nil
}

func createInsertUserAssociation(chefServerOrgUsers []pipeline.UserAssociation) []pipeline.UserAssociation {
	userAssociation := make([]pipeline.UserAssociation, 0)
	for _, user := range chefServerOrgUsers {
		userAssociation = append(userAssociation, pipeline.UserAssociation{Username: user.Username, IsAdmin: user.IsAdmin, ActionOps: pipeline.Insert})
	}
	return userAssociation
}

func createDeleteUserAssociation(orgUsersInDb []storage.OrgUser) []pipeline.UserAssociation {
	userAssociation := make([]pipeline.UserAssociation, 0)
	for _, user := range orgUsersInDb {
		userAssociation = append(userAssociation, pipeline.UserAssociation{Username: user.InfraServerUsername, IsAdmin: user.IsAdmin, ActionOps: pipeline.Delete})
	}
	return userAssociation
}

func insertOrUpdateActionForOrgUsers(orgUsers []storage.OrgUser, chefServerOrgUsers []pipeline.UserAssociation) []pipeline.UserAssociation {
	var userAssociation []pipeline.UserAssociation
	orgUserMapDB := createMapForOrgUsersInDB(orgUsers)
	for _, user := range chefServerOrgUsers {
		isAdmin, valuePresent := orgUserMapDB[user.Username]
		if valuePresent {
			//check for the org admins
			if user.IsAdmin != isAdmin {
				userAssociation = append(userAssociation, pipeline.UserAssociation{Username: user.Username, IsAdmin: user.IsAdmin, ActionOps: pipeline.Update})
			} else {
				userAssociation = append(userAssociation, pipeline.UserAssociation{Username: user.Username, IsAdmin: user.IsAdmin, ActionOps: pipeline.Skip})
			}
		} else {
			userAssociation = append(userAssociation, pipeline.UserAssociation{Username: user.Username, IsAdmin: user.IsAdmin, ActionOps: pipeline.Insert})
		}
	}
	return userAssociation
}

func deleteActionForOrgUses(orgUsers []storage.OrgUser, chefServerOrgUsers []pipeline.UserAssociation) []pipeline.UserAssociation {
	var userAssociation []pipeline.UserAssociation
	orgUserJsonMap := createMapForOrgUsersInJson(chefServerOrgUsers)
	for _, user := range orgUsers {
		_, valuePresent := orgUserJsonMap[user.InfraServerUsername]
		if !valuePresent {
			userAssociation = append(userAssociation, pipeline.UserAssociation{Username: user.InfraServerUsername, IsAdmin: user.IsAdmin, ActionOps: pipeline.Delete})
		}
	}

	return userAssociation
}

func createMapForOrgUsersInDB(orgUsers []storage.OrgUser) map[string]bool {
	orgUsersMap := make(map[string]bool)
	for _, s := range orgUsers {
		orgUsersMap[s.InfraServerUsername] = s.IsAdmin
	}
	return orgUsersMap
}

func createMapForOrgUsersInJson(chefServerOrgUsers []pipeline.UserAssociation) map[string]string {
	orgUsersMap := make(map[string]string)
	for _, user := range chefServerOrgUsers {
		orgUsersMap[user.Username] = ""
	}
	return orgUsersMap
}

// getChefServerOrgUsers returns the chef server organization users from backup file
func getChefServerOrgUsers(orgName, fileLocation string) ([]pipeline.UserAssociation, error) {
	orgUsers := make([]pipeline.UserAssociation, 0)

	members, err := getOrgMembers(orgName, fileLocation)
	if err != nil {
		log.Errorf("Unable to get organization members %s", err)
		return nil, err
	}
	admins, err := getOrgAdmins(orgName, fileLocation)
	if err != nil {
		log.Errorf("Unable to get organization admins %s", err)
		return nil, err
	}
	orgAdminMap := createMapForOrgAdminsInJson(admins)
	for _, member := range members {
		orgUser := pipeline.UserAssociation{}
		if member.User.Username == "pivotal" {
			continue
		}
		orgUser.Username = member.User.Username
		_, valuePresent := orgAdminMap[member.User.Username]
		if valuePresent {
			orgUser.IsAdmin = true
		}
		orgUsers = append(orgUsers, orgUser)
	}
	return orgUsers, nil
}

// getOrgMembers Get the data of members.json
func getOrgMembers(orgName, fileLocation string) ([]pipeline.MembersJson, error) {
	var orgMembers []pipeline.MembersJson
	usersJsonPath := path.Join(fileLocation, orgName, "members.json")
	usersjsonFile, err := os.Open(usersJsonPath)
	// if we os.Open returns an error then handle it
	if err != nil {
		log.Errorf("Unable to open org members file at the location : %s", usersJsonPath)
		return nil, err
	}
	log.Info("Successfully opened the org members file at location", usersJsonPath)
	// defer the closing of our jsonFile so that we can parse it later on
	defer func() {
		_ = usersjsonFile.Close()
	}()
	err = json.NewDecoder(usersjsonFile).Decode(&orgMembers)
	if err != nil {
		log.Errorf("Unable to decode the org members file %s %s", usersJsonPath, err)
		return nil, err
	}
	return orgMembers, nil
}

// getOrgAdmins Get the data of admins.json
func getOrgAdmins(orgName, fileLocation string) (pipeline.AdminsJson, error) {
	var orgAdmins pipeline.AdminsJson
	adminJsonPath := path.Join(fileLocation, orgName, "groups", "admins.json")
	jsonFile, err := os.Open(adminJsonPath)
	// if we os.Open returns an error then handle it
	if err != nil {
		log.Errorf("Unable to open org admins file at the location : %s %s", adminJsonPath, err)
		return pipeline.AdminsJson{}, err
	}
	log.Info("Successfully opened the org admins file at location", adminJsonPath)
	// defer the closing of our jsonFile so that we can parse it later on
	defer func() {
		_ = jsonFile.Close()
	}()
	err = json.NewDecoder(jsonFile).Decode(&orgAdmins)
	if err != nil {
		log.Errorf("Unable to decode the org admins file %s %s", adminJsonPath, err)
		return pipeline.AdminsJson{}, err
	}
	return orgAdmins, nil
}

func createMapForOrgAdminsInJson(adminsJson pipeline.AdminsJson) map[string]string {
	orgAdminsMap := make(map[string]string)
	for _, username := range adminsJson.Users {
		orgAdminsMap[username] = ""
	}
	return orgAdminsMap

}

//GetUsersForBackup get all the users details from back up file
func GetUsersForBackup(ctx context.Context, st storage.Storage, localUserClient local_user.UsersMgmtServiceClient, result pipeline.Result) (pipeline.Result, error) {
	log.Info("starting with user parsing phase for migration id: ", result.Meta.MigrationID)

	file := path.Join(result.Meta.UnzipFolder, "key_dump.json")
	var keyDumps []pipeline.KeyDump

	keyDumpFile, err := os.Open(file)
	if err != nil {
		log.Errorf("failed to open keydump file for user parsing for the migration id: %s : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}

	if err = json.NewDecoder(keyDumpFile).Decode(&keyDumps); err != nil {
		log.Errorf("failed to decode keydump json for the migration id: %s : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}

	serverUsers := keyDumpTOUser(keyDumps)
	automateUsers, err := st.GetAutomateInfraServerUsers(ctx, result.Meta.ServerID)
	if err != nil {
		log.Errorf("failed to get users form db for user parsing for the migration id: %s : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}

	var mappedUsers []pipeline.User

	mappedUsers = append(mappedUsers, deleteUser(serverUsers, automateUsers)...)
	mappedUsers = append(mappedUsers, insertUpdateSkipUser(ctx, serverUsers, automateUsers, localUserClient)...)

	result.ParsedResult.Users = mappedUsers
	return result, nil
}

// Clean serialized_object and Populate Users struct
func keyDumpTOUser(keyDump []pipeline.KeyDump) []pipeline.User {
	users := make([]pipeline.User, 0)
	for _, kd := range keyDump {
		sec := map[string]string{}
		if err := json.Unmarshal([]byte(kd.SerializedObject), &sec); err != nil {
			log.Errorf("failed to parse user's first, middle and last name: %s", err.Error())
		}
		user := &pipeline.User{
			Username:     kd.Username,
			Email:        kd.Email,
			DisplayName:  sec["display_name"],
			FirstName:    sec["first_name"],
			LastName:     sec["last_name"],
			MiddleName:   sec["middle_name"],
			HashPassword: kd.HashedPassword,
		}
		user.SetConnector(kd.ExternalAuthenticationUID)
		user.SetAutomateUsername(kd.ExternalAuthenticationUID)

		users = append(users, *user)
	}
	return users
}

func automateMap(automateUser []storage.User) map[string]storage.User {
	automateMap := map[string]storage.User{}
	for _, aUser := range automateUser {
		automateMap[aUser.InfraServerUsername] = aUser
	}
	return automateMap
}

func serverMap(server []pipeline.User) map[string]pipeline.User {
	serverMap := map[string]pipeline.User{}
	for _, sUser := range server {
		serverMap[sUser.Username] = sUser
	}
	return serverMap
}

func insertUpdateSkipUser(ctx context.Context, serverUser []pipeline.User, automateUser []storage.User, localUserClient local_user.UsersMgmtServiceClient) []pipeline.User {
	var parsedUsers []pipeline.User
	autoMap := automateMap(automateUser)
	for _, sUser := range serverUser {
		if _, ok := autoMap[sUser.Username]; ok {
			emptyVal := pipeline.User{}
			returnedVal := skipOrUpdate(autoMap, sUser)
			if returnedVal != emptyVal {
				parsedUsers = append(parsedUsers, returnedVal)
			}
		} else {
			if sUser.Username == "pivotal" {
				continue
			} else {
				if sUser.Connector == pipeline.Local {
					userExists := checkUserExist(ctx, localUserClient, sUser)
					sUser.IsConflicting = userExists
				}
				sUser.ActionOps = pipeline.Insert
				parsedUsers = append(parsedUsers, sUser)
			}

		}

	}

	return parsedUsers
}
func skipOrUpdate(autoMap map[string]storage.User, sUser pipeline.User) pipeline.User {
	if autoMap[sUser.Username].InfraServerUsername == sUser.Username {
		if autoMap[sUser.Username].InfraServerUsername == sUser.Username && autoMap[sUser.Username].Email == sUser.Email &&
			autoMap[sUser.Username].DisplayName == sUser.DisplayName && autoMap[sUser.Username].FirstName == sUser.FirstName &&
			autoMap[sUser.Username].LastName == sUser.LastName && autoMap[sUser.Username].MiddleName == sUser.MiddleName {
			sUser.ActionOps = pipeline.Skip
			return sUser

		} else {
			sUser.ActionOps = pipeline.Update
			return sUser
		}
	}
	return pipeline.User{}
}

func deleteUser(serverUser []pipeline.User, automateUser []storage.User) []pipeline.User {
	var parsedUsers []pipeline.User
	serverMap := serverMap(serverUser)
	for _, aUser := range automateUser {
		if _, ok := serverMap[aUser.InfraServerUsername]; !ok {
			parsedUsers = append(parsedUsers, pipeline.User{
				Username:  aUser.InfraServerUsername,
				ActionOps: pipeline.Delete,
			})
		}

	}

	return parsedUsers
}

//checkUserExist check user exists in the local automate or not
func checkUserExist(ctx context.Context, localUserClient local_user.UsersMgmtServiceClient, user pipeline.User) bool {
	_, err := localUserClient.GetUser(ctx, &local_user.Email{Email: user.AutomateUsername})
	if err != nil {
		log.Errorf(err.Error())
		log.Errorf("Unable to fetch user")
		return false
	}
	return true
}

func createLocalUser(ctx context.Context, localUserClient local_user.UsersMgmtServiceClient, user pipeline.User) error {
	_, err := localUserClient.CreateUser(ctx, &local_user.CreateUserReq{
		Name:     user.DisplayName,
		Id:       user.AutomateUsername,
		Password: user.HashPassword,
		Email:    user.AutomateUsername,
		IsHashed: true,
	})
	if err != nil {
		log.Errorf("Unable to create user in Automate for user : %s with error %s", user.AutomateUsername, err.Error())
		return err
	}
	return nil
}

// StoreOrgsUsersAssociation  populate the users association with orgs based on the actions
func StoreOrgsUsersAssociation(ctx context.Context, st storage.Storage, result pipeline.Result) (pipeline.Result, error) {
	log.Info("Starting with the storing org user association for migration id :", result.Meta.MigrationID)

	var err error
	var totalUsers int64
	selectedUsersMap := createMapForUsers(result.ParsedResult.Users)
	for _, orgUsers := range result.ParsedResult.OrgsUsers {
		for _, orgUserAssociation := range orgUsers.Users {
			if _, keyPresent := selectedUsersMap[orgUserAssociation.Username]; keyPresent {
				totalUsers++
				if orgUserAssociation.ActionOps == pipeline.Skip {
					result.ParsedResult.OrgsUsersAssociationsCount.Skipped++
					continue
				}
				err, _ = storeOrgUserAssociation(ctx, st, result.Meta.ServerID, orgUsers.OrgName.Name, orgUserAssociation)
				if err != nil {
					log.Errorf("Failed to migrate org user %s of org %s for migration id %s : %s", orgUserAssociation.Username, orgUsers.OrgName.Name, result.Meta.MigrationID, err.Error())
					result.ParsedResult.OrgsUsersAssociationsCount.Failed++
					continue
				}
				result.ParsedResult.OrgsUsersAssociationsCount.Succeeded++
			}
		}
	}
	if totalUsers == result.ParsedResult.OrgsUsersAssociationsCount.Failed {
		return result, err
	}
	log.Info("Completed with the storing org user association for migration id :", result.Meta.MigrationID)
	return result, nil
}

func createMapForUsers(users []pipeline.User) map[string]string {
	usersMap := make(map[string]string)
	for _, user := range users {
		usersMap[user.Username] = ""
	}
	return usersMap
}

// storeOrgUserAssociation stores a single Org user into DB
func storeOrgUserAssociation(ctx context.Context, st storage.Storage, serverID string, orgID string, orgUserAssociation pipeline.UserAssociation) (error, pipeline.ActionOps) {
	var actionTaken pipeline.ActionOps
	var err error
	switch actionTaken = orgUserAssociation.ActionOps; orgUserAssociation.ActionOps {
	case pipeline.Insert:
		_, err = st.StoreOrgUserAssociation(ctx, serverID, orgID, orgUserAssociation.Username, orgUserAssociation.IsAdmin)
	case pipeline.Delete:
		_, err = st.DeleteOrgUserAssociation(ctx, serverID, orgID, orgUserAssociation.Username, orgUserAssociation.IsAdmin)
	case pipeline.Update:
		_, err = st.EditOrgUserAssociation(ctx, serverID, orgID, orgUserAssociation.Username, orgUserAssociation.IsAdmin)
	default:
	}
	return err, actionTaken
}

// StoreUsers reads the Result struct and populate the users table
func StoreUsers(ctx context.Context, st storage.Storage, localUserClient local_user.UsersMgmtServiceClient, res pipeline.Result) (pipeline.Result, error) {
	var err error

	for _, user := range res.ParsedResult.Users {
		if user.ActionOps == pipeline.Skip {
			res.ParsedResult.UsersCount.Skipped++
			continue
		}
		_, err = StoreUser(ctx, st, user, res.Meta.ServerID, localUserClient)
		if err != nil {
			res.ParsedResult.UsersCount.Failed++
			continue
		}
		res.ParsedResult.UsersCount.Succeeded++
	}

	if len(res.ParsedResult.Users) == int(res.ParsedResult.UsersCount.Failed) {
		log.Errorf("Failed to migrate user for migration id %s : %s", res.Meta.MigrationID, err.Error())
		return res, err
	}

	log.Info("Successfully completed the user migration phase for migration id: ", res.Meta.MigrationID)
	return res, err
}

// StoreUser stores a single User into DB
func StoreUser(ctx context.Context, st storage.Storage, user pipeline.User, serverID string, localUserClient local_user.UsersMgmtServiceClient) (pipeline.ActionOps, error) {
	var actionTaken pipeline.ActionOps
	var err error

	storageUser := storage.User{
		ServerID:            serverID,
		InfraServerUsername: user.Username,
		Connector:           user.Connector,
		Email:               user.Email,
		DisplayName:         user.DisplayName,
		FirstName:           user.FirstName,
		LastName:            user.LastName,
		MiddleName:          user.MiddleName,
		AutomateUserID:      user.AutomateUsername,
	}
	switch actionTaken = user.ActionOps; user.ActionOps {
	case pipeline.Insert:
		if user.Connector == pipeline.Local {
			err = createLocalUser(ctx, localUserClient, user)
			if err != nil {
				return actionTaken, err
			}
		}
		_, err = st.InsertUser(ctx, storageUser)
	case pipeline.Delete:
		_, err = st.DeleteUser(ctx, storageUser)
	case pipeline.Update:
		_, err = st.EditUser(ctx, storageUser)
	default:
	}
	return actionTaken, err
}
