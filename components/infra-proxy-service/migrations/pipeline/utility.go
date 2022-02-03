package pipeline

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path"

	"github.com/chef/automate/components/infra-proxy-service/storage"
	log "github.com/sirupsen/logrus"
)

// StoreOrgs reads the Result struct and populate the orgs table
func StoreOrgs(ctx context.Context, st storage.Storage, mst storage.MigrationStorage, res Result) (Result, error) {
	var err error
	var msg string
	var totalSucceeded, totalSkipped, totalFailed int64
	_, err = mst.StartOrgMigration(ctx, res.Meta.MigrationID, res.Meta.ServerID)
	if err != nil {
		return res, err
	}
	log.Info("Starting the organisation migration phase for migration id: ", res.Meta.MigrationID)
	for _, org := range res.ParsedResult.Orgs {
		err, _ = StoreOrg(ctx, st, org, res.Meta.ServerID)
		if err != nil {
			totalFailed++
			msg = err.Error()
			continue
		}
		if org.ActionOps == Skip {
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
func StoreOrg(ctx context.Context, st storage.Storage, org Org, serverID string) (error, ActionOps) {
	var actionTaken ActionOps
	var err error
	switch org.ActionOps {
	case Insert:
		_, err = st.StoreOrg(ctx, org.Name, org.FullName, "", "", serverID, nil)
		actionTaken = Insert
	case Delete:
		_, err = st.DeleteOrg(ctx, org.Name, serverID)
		actionTaken = Delete
	case Update:
		_, err = st.EditOrg(ctx, org.Name, org.FullName, "", serverID, nil)
		actionTaken = Update
	default:
	}
	return err, actionTaken
}

func ParseOrgs(ctx context.Context, st storage.Storage, mst storage.MigrationStorage, result Result) (Result, error) {
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
		mst.FailedOrgParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		mst.FailedMigration(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		return result, err
	}

	defer folder.Close()
	orgNames, err := folder.Readdir(0)
	if err != nil {
		log.Errorf("Failed to read the files for the file path %s : %s", orgPath, err.Error())
		mst.FailedOrgParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		mst.FailedMigration(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		return result, err
	}
	orgsPresentInDB, err := st.GetOrgs(ctx, result.Meta.ServerID)
	if err != nil {
		log.Errorf("Failed to read orgs from database for %s:%s", result.Meta.ServerID, err.Error())
		mst.FailedOrgParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		mst.FailedMigration(ctx, result.Meta.MigrationID, result.Meta.ServerID, err.Error(), 0, 0, 0)
		return result, err
	}

	result.ParsedResult.Orgs = append(result.ParsedResult.Orgs, insertOrUpdateOrg(orgNames, orgsPresentInDB, orgPath)...)

	result.ParsedResult.Orgs = append(result.ParsedResult.Orgs, deleteOrgsIfNotPresentInCurrentFile(orgNames, orgsPresentInDB)...)

	mst.CompleteOrgParsing(ctx, result.Meta.MigrationID, result.Meta.ServerID, 0, 0, 0)
	if err != nil {
		log.Errorf("Failed to update the complete status while parsing for migration id %s : %s", result.Meta.MigrationID, err.Error())
		return result, err
	}

	log.Info("Successfully completed the organisation parsing phase for migration id: ", result.Meta.MigrationID)
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
		orgMap[s.Name()] = ""
		//No value required for comparison
	}
	return orgMap
}

func insertOrUpdateOrg(orgsInFiles []os.FileInfo, orgsInDB []storage.Org, orgPath string) []Org {
	var orgList []Org
	orgDatabaseMap := createDatabaseOrgsMap(orgsInDB)
	var orgJson OrgJson
	//For insert, update and skip action
	for _, org := range orgsInFiles {
		orgInfo, valuePresent := orgDatabaseMap[org.Name()]
		orgJson = openOrgFolder(org, orgPath)
		if valuePresent {
			if orgJson.FullName != orgInfo {
				//Update org in the result actions
				orgList = append(orgList, createOrgStructForAction(orgJson.Name, orgJson.FullName, Update))
			} else {
				//Skip org action if full names are not equal
				orgList = append(orgList, createOrgStructForAction(orgJson.Name, orgJson.FullName, Skip))

			}
		} else {
			//Insert org action if not present in database
			orgList = append(orgList, createOrgStructForAction(orgJson.Name, orgJson.FullName, Insert))
		}
	}
	return orgList
}

func deleteOrgsIfNotPresentInCurrentFile(orgsInFiles []os.FileInfo, orgsInDB []storage.Org) []Org {
	var orgList []Org
	orgFilesMap := createFileOrgsMap(orgsInFiles)
	//For delete action by comparing database orgs with file orgs
	for _, org := range orgsInDB {
		_, valuePresent := orgFilesMap[org.ID]
		if !valuePresent {
			orgList = append(orgList, createOrgStructForAction(org.ID, org.Name, Delete))
		}
	}
	return orgList
}

func openOrgFolder(org os.FileInfo, fileLocation string) OrgJson {
	var orgJson OrgJson
	jsonPath := path.Join(fileLocation, org.Name(), "org.json")
	jsonFile, err := os.Open(jsonPath)
	// if we os.Open returns an error then handle it
	if err != nil {
		fmt.Println(err)
	}
	log.Info("Successfully opened the file at location", jsonPath)
	// defer the closing of our jsonFile so that we can parse it later on
	defer jsonFile.Close()
	byteValue, _ := ioutil.ReadAll(jsonFile)
	json.Unmarshal(byteValue, &orgJson)

	return orgJson
}

func createOrgStructForAction(orgId string, orgName string, ops ActionOps) Org {
	return Org{
		Name:      orgId,
		FullName:  orgName,
		ActionOps: ops,
	}
}
