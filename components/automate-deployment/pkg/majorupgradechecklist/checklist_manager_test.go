package majorupgradechecklist

import (
	"log"
	"os"
	"testing"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/stretchr/testify/assert"
)

var writer *cli.Writer

func TestCreatePostChecklistFile(t *testing.T) {
	// remove json file
	removeFile()
	// Create json file
	cl := NewCRUDChecklist("3")
	err := cl.CreatePostChecklistFile()
	// return nil if json file is created successfully
	assert.Equal(t, err, nil)
}

func TestReadPostChecklistByIdSuccess(t *testing.T) {
	cl := NewCRUDChecklist("3")
	IsExecuted, err := cl.ReadPostChecklistById("pg_migrate")
	// return nil if checklist by id found
	assert.Equal(t, err, nil)
	// check is executed is true or false
	assert.Equal(t, IsExecuted, false)
}

func TestReadPostChecklistSuccess(t *testing.T) {
	cl := NewCRUDChecklist("3")
	result, err := cl.ReadPendingPostChecklistFile()
	assert.Equal(t, err, nil)
	//get json data as result
	assert.NotEqual(t, result, []string{})
}

func TestReadPostChecklistByIdFailure(t *testing.T) {
	// remove json file
	removeFile()
	cl := NewCRUDChecklist("3")
	IsExecuted, err := cl.ReadPostChecklistById("pg_migrate")
	// return nil if checklist by id found
	assert.Equal(t, err.Error(), "open /hab/svc/deployment-service/var/upgrade_metadata.json: no such file or directory")
	// check is executed is true or false
	assert.Equal(t, IsExecuted, false)
}

func TestReadPostChecklistFailure(t *testing.T) {
	// remove json file
	removeFile()
	cl := NewCRUDChecklist("3")
	result, err := cl.ReadPendingPostChecklistFile()
	assert.Equal(t, err.Error(), "open /hab/svc/deployment-service/var/upgrade_metadata.json: no such file or directory")
	//get json data as result
	assert.NotEqual(t, result, []string{})
}

func removeFile() {
	IsExist := false
	_, err := os.Stat("/hab/svc/deployment-service/var/upgrade_metadata.json")
	if err == nil {
		IsExist = true
	}
	if os.IsNotExist(err) {
		IsExist = false
	}
	if IsExist {
		e := os.Remove("/hab/svc/deployment-service/var/upgrade_metadata.json")
		if e != nil {
			log.Fatal(e)
		}
	}
}
