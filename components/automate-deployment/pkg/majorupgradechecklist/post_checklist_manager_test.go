package majorupgradechecklist

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestNewPostChecklistManager(t *testing.T) {
	//  check version is valid or invalid
	_, err := NewPostChecklistManager("3.0.0")
	assert.NoError(t, err)
	_, err = NewPostChecklistManager("20220221183851")
	assert.Error(t, err)
	assert.Equal(t, "invalid major version", err.Error())
}

func TestCreatePostChecklistFile(t *testing.T) {
	pcm, err := NewPostChecklistManager("3.0.0")
	assert.NoError(t, err)
	// create a new json file
	err = pcm.CreatePostChecklistFile(FILE_NAME, false)
	assert.NoError(t, err)
	// throws an error if th file path is invalid
	err = pcm.CreatePostChecklistFile(UPGRADE_METADATA, false)
	assert.Error(t, err)
	assert.Equal(t, "open /hab/svc/deployment-service/var/upgrade_metadata.json: no such file or directory", err.Error())
}

func TestReadPostChecklistById(t *testing.T) {
	pcm, err := NewPostChecklistManager("3.0.0")
	assert.NoError(t, err)
	// if the file exists, return isExecuted true or false ,
	// if it is not executed then return false, if not true
	res, err := pcm.ReadPostChecklistById("migrate_pg", FILE_NAME)
	assert.NoError(t, err)
	assert.Equal(t, false, res)
	err = pcm.UpdatePostChecklistFile("migrate_pg", FILE_NAME)
	assert.NoError(t, err)
	res, err = pcm.ReadPostChecklistById("migrate_pg", FILE_NAME)
	assert.NoError(t, err)
	assert.Equal(t, true, res)
	_, err = pcm.ReadPostChecklistById("migrate_pg", UPGRADE_METADATA)
	assert.NoError(t, err)
}

func TestReadPendingPostChecklistFile(t *testing.T) {
	// get pending post checklist from the file if major version is valid,
	// and there should be no error if file doesn't exist and
	pcm, err := NewPostChecklistManager("3.0.0")
	assert.NoError(t, err)
	res, err := pcm.ReadPendingPostChecklistFile(FILE_NAME, false)
	assert.NoError(t, err)
	assert.NotEqual(t, []string{}, res)
	res, err = pcm.ReadPendingPostChecklistFile(UPGRADE_METADATA, false)
	assert.Error(t, err)
	assert.Equal(t, 0, len(res))
	assert.Equal(t, "open /hab/svc/deployment-service/var/upgrade_metadata.json: no such file or directory", err.Error())

	// if version is invalid and we have right path for the file
	// then return error
	pcm, err = NewPostChecklistManager("100.0.0")
	assert.Error(t, err)
	assert.Equal(t, "invalid major version", err.Error())
	_, err = pcm.ReadPendingPostChecklistFile(FILE_NAME, false)
	assert.Error(t, err)
	assert.Equal(t, "Failed to read checklist since version didn't match", err.Error())

	// if pg is external then show post checklist only once
	pcm, err = NewPostChecklistManager("3.0.0")
	assert.NoError(t, err)
	res, err = pcm.ReadPendingPostChecklistFile(FILE_NAME, true)
	assert.NoError(t, err)
	assert.NotEqual(t, []string{}, res)

	res, err = pcm.ReadPendingPostChecklistFile(FILE_NAME, true)
	assert.NoError(t, err)
	assert.Equal(t, 0, len(res))
}

func TestUpdatePostChecklistFile(t *testing.T) {
	pcm, err := NewPostChecklistManager("3.0.0")
	assert.NoError(t, err)
	// create a new json file
	err = pcm.CreatePostChecklistFile(FILE_NAME, true)
	assert.NoError(t, err)

	// if the file exists, update the migrate_pg
	err = pcm.UpdatePostChecklistFile("migrate_pg", FILE_NAME)
	assert.NoError(t, err)

	// if the file doesn't exist, return nil (no error)
	err = pcm.UpdatePostChecklistFile("migrate_pg", UPGRADE_METADATA)
	assert.Error(t, err)
	assert.Equal(t, "open /hab/svc/deployment-service/var/upgrade_metadata.json: no such file or directory", err.Error())
}
