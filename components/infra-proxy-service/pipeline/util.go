package pipeline

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/chef/automate/components/infra-proxy-service/storage"
	log "github.com/sirupsen/logrus"
	"io/ioutil"
	"os"
	"path"
)

// ParseInfraServerOgs is the function to parse infra server orgs
func ParseInfraServerOgs(ctx context.Context, result Result) Result {

	f, err := os.Open(result.Meta.UnzipFolder)
	if err != nil {
		log.Errorf("Failed to read the files for the file path %s : %s", result.Meta.UnzipFolder, err.Error())
	}

	defer f.Close()

	orgNames, err := f.Readdir(0)
	for _, file := range orgNames {
		fmt.Println(file.Name(), file.IsDir())
	}

	//Get orgs from database after the implementation in org
	orgsPresentInDB := []storage.Org{}
	if err != nil {
		log.Errorf("Failed to read orgs from database for %s:%s", result.Meta.ServerId, err.Error())
		//s.Service.Migration.FailedOrgParsing(ctx, result.Meta.MigrationId, result.Meta.ServerId, err.Error(), 0, 0, 0)
		//s.service.Migration.FailedMigration(ctx, result.Meta.MigrationId, result.Meta.ServerId, err.Error(), 0, 0, 0)
	}

	insertOrUpdateOrg(orgNames, orgsPresentInDB, result)

	deleteOrgsIfNotPresentInCurrentFile(orgNames, orgsPresentInDB, result)

	//s.service.Migration.CompleteOrgParsing(ctx, result.Meta.MigrationId, result.Meta.ServerId, 0, 0, 0)
	return result

}

func createDatabaseOrgsMap(orgs []storage.Org) map[string]string {
	var orgMap map[string]string
	for _, s := range orgs {
		orgMap[s.ID] = s.Name
		// or just keys, without values: elementMap[s] = ""
	}
	return orgMap
}

func createFileOrgsMap(orgs []os.FileInfo) map[string]string {
	var orgMap map[string]string
	for _, s := range orgs {
		orgMap[s.Name()] = s.Name()
		// or just keys, without values: elementMap[s] = ""
	}
	return orgMap
}

func insertOrUpdateOrg(orgsInFiles []os.FileInfo, orgsInDB []storage.Org, result Result) {
	orgDatabaseMap := createDatabaseOrgsMap(orgsInDB)
	var orgJson OrgJson
	//For insert, update and skip action
	for _, org := range orgsInFiles {
		orgInfo, valuePresent := orgDatabaseMap[org.Name()]
		orgJson = openOrgFolder(org, result.Meta.UnzipFolder)
		if valuePresent {
			if !(orgJson.FullName == orgInfo) {
				//Update org in the result actions
				result.ParsedResult.Orgs = append(result.ParsedResult.Orgs, createOrgStructForAction(orgJson.Name, orgJson.FullName, Update))
			} else {
				//Skip org action if full names are not equal
				result.ParsedResult.Orgs = append(result.ParsedResult.Orgs, createOrgStructForAction(orgJson.Name, orgJson.FullName, Skip))

			}
		} else {
			//Insert org action if not present in database
			result.ParsedResult.Orgs = append(result.ParsedResult.Orgs, createOrgStructForAction(orgJson.Name, orgJson.FullName, Insert))
		}
	}
}

func deleteOrgsIfNotPresentInCurrentFile(orgsInFiles []os.FileInfo, orgsInDB []storage.Org, result Result) {
	orgFilesMap := createFileOrgsMap(orgsInFiles)
	//For delete action by comparing database orgs with file orgs
	for _, org := range orgsInDB {
		_, valuePresent := orgFilesMap[org.ID]
		if !valuePresent {
			result.ParsedResult.Orgs = append(result.ParsedResult.Orgs, createOrgStructForAction(org.ID, org.Name, Delete))
		}
	}
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
