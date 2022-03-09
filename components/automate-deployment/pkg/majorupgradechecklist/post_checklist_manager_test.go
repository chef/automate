package majorupgradechecklist

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestNewPostChecklistManager(t *testing.T) {
	_, err := NewPostChecklistManager("3.0.0")
	assert.NoError(t, err)
	_, err = NewPostChecklistManager("20220221183851")
	assert.Error(t, err)
	assert.Equal(t, "invalid major version", err.Error())
}

func TestCreatePostChecklistFile(t *testing.T) {
	pcm, err := NewPostChecklistManager("3.0.0")
	assert.NoError(t, err)
	err = pcm.CreatePostChecklistFile(FILE_NAME, false)
	assert.NoError(t, err)
	err = pcm.CreatePostChecklistFile(UPGRADE_METADATA, false)
	assert.Error(t, err)
}

func TestReadPostChecklistById(t *testing.T) {
	pcm, err := NewPostChecklistManager("3.0.0")
	assert.NoError(t, err)
	res, err := pcm.ReadPostChecklistById("migrate_pg", FILE_NAME)
	assert.NoError(t, err)
	assert.Equal(t, false, res)
	_, err = pcm.ReadPostChecklistById("migrate_pg", UPGRADE_METADATA)
	assert.NoError(t, err)
}

func TestReadPendingPostChecklistFile(t *testing.T) {
	pcm, err := NewPostChecklistManager("3.0.0")
	assert.NoError(t, err)
	_, err = pcm.ReadPendingPostChecklistFile(FILE_NAME, false)
	assert.NoError(t, err)
	_, err = pcm.ReadPendingPostChecklistFile(UPGRADE_METADATA, false)
	assert.NoError(t, err)

	pcm, err = NewPostChecklistManager("4.0.0")
	assert.Error(t, err)
	_, err = pcm.ReadPendingPostChecklistFile(FILE_NAME, false)
	assert.Error(t, err)

	pcm, err = NewPostChecklistManager("3.0.0")
	assert.NoError(t, err)
	res, err := pcm.ReadPendingPostChecklistFile(FILE_NAME, true)
	assert.NoError(t, err)
	assert.NotEqual(t, []string{}, res)

	res, err = pcm.ReadPendingPostChecklistFile(FILE_NAME, true)
	assert.NoError(t, err)
	assert.NotEqual(t, []string{}, res)
	err = pcm.UpdatePostChecklistFile("migrate_pg", FILE_NAME)
	assert.NoError(t, err)
	res, err = pcm.ReadPendingPostChecklistFile(FILE_NAME, true)
	assert.NoError(t, err)
	assert.NotEqual(t, []string{}, res)
}

func TestUpdatePostChecklistFile(t *testing.T) {
	pcm, err := NewPostChecklistManager("3.0.0")
	assert.NoError(t, err)
	err = pcm.CreatePostChecklistFile(FILE_NAME, true)
	assert.NoError(t, err)
	err = pcm.UpdatePostChecklistFile("migrate_pg", FILE_NAME)
	assert.NoError(t, err)
	err = pcm.UpdatePostChecklistFile("migrate_pg", UPGRADE_METADATA)
	assert.NoError(t, err)
}
