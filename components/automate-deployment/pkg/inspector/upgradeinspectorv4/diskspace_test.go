package upgradeinspectorv4

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

type MockFileSystemUtils struct {
	values map[string][]interface{}
}

func (fsu *MockFileSystemUtils) PathExists(path string) (bool, error) {
	return true, nil
}
func (fsu *MockFileSystemUtils) IsSymlink(path string) (bool, error) {
	return true, nil
}
func (fsu *MockFileSystemUtils) CalDirSizeInGB(path string) (float64, error) {
	var err error
	if len(fsu.values["CalDirSizeInGB"]) == 2 {
		err = fsu.values["CalDirSizeInGB"][1].(error)
	}
	return fsu.values["CalDirSizeInGB"][0].(float64), err
}
func (fsu *MockFileSystemUtils) CheckSpaceAvailability(dir string, minSpace float64) (bool, error) {
	return true, nil
}
func (fsu *MockFileSystemUtils) GetFreeSpaceinGB(dir string) (float64, error) {
	var err error
	if dir == "/hab" {
		if len(fsu.values["GetFreeSpaceinGB"]) == 2 {
			err = fsu.values["GetFreeSpaceinGB"][1].(error)
		}
	} else if dir == "/home/ubuntu" {
		if len(fsu.values["GetFreeSpaceinGB"]) == 3 {
			err = fsu.values["GetFreeSpaceinGB"][2].(error)
		}
	}
	return fsu.values["GetFreeSpaceinGB"][0].(float64), err
}
func (fsu *MockFileSystemUtils) GetHabRootPath() string {
	return fsu.values["GetHabRootPath"][0].(string)
}

var testReturnValues = []map[string][]interface{}{
	{"CalDirSizeInGB": {3.0}, "GetFreeSpaceinGB": {2.5}, "GetHabRootPath": {"/hab"}},
	{"CalDirSizeInGB": {-1.0, errors.New("failed to check filesystem")}, "GetFreeSpaceinGB": {2.5}, "GetHabRootPath": {"/hab"}},
	{"CalDirSizeInGB": {-1.0}, "GetFreeSpaceinGB": {2.5, errors.New("failed to check filesystem")}, "GetHabRootPath": {"/hab"}},
	{"CalDirSizeInGB": {-1.0}, "GetFreeSpaceinGB": {2.5, nil, errors.New("failed to check filesystem")}, "GetHabRootPath": {"/hab"}},
}

func TestDiskSpaceInspection(t *testing.T) {
	tw := NewTestWriter()
	tb := NewDiskSpaceInspection(tw.CliWriter, false, "", &MockFileSystemUtils{testReturnValues[0]})
	index := 3
	tb.ShowInfo(&index)
	expected := `3. /hab directory should have 8.8GB of free space. (Currently available space : 2.5GB)
`
	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 3, index)
}

func TestDiskSpaceInspectionWithExternal(t *testing.T) {
	tw := NewTestWriter()
	tb := NewDiskSpaceInspection(tw.CliWriter, true, "", &MockFileSystemUtils{testReturnValues[0]})
	index := 3
	tb.ShowInfo(&index)
	expected := `3. /hab directory should have 5.5GB of free space. (Currently available space : 2.5GB)
`
	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 3, index)
}

func TestDiskSpaceInspectionWithOSDataDir(t *testing.T) {
	tw := NewTestWriter()
	tb := NewDiskSpaceInspection(tw.CliWriter, false, "/home/ubuntu", &MockFileSystemUtils{testReturnValues[0]})
	index := 3
	tb.ShowInfo(&index)
	expected := `3. /hab directory should have 5.5GB of free space. (Currently available space : 2.5GB)
4. /home/ubuntu directory should have 3.3GB of free space. (Currently available space : 2.5GB)
`
	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 4, index)
}

func TestDiskSpaceInspectionFileSystemError(t *testing.T) {
	tw := NewTestWriter()
	tb := NewDiskSpaceInspection(tw.CliWriter, false, "/home/ubuntu", &MockFileSystemUtils{testReturnValues[1]})
	index := 3
	err := tb.ShowInfo(&index)
	expected := ""
	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 3, index)
	if assert.Error(t, err) {
		assert.Equal(t, "failed to check filesystem", err.Error())
	}
}

func TestDiskSpaceInspectionFreeDiskFilesystemError(t *testing.T) {
	tw := NewTestWriter()
	tb := NewDiskSpaceInspection(tw.CliWriter, false, "/home/ubuntu", &MockFileSystemUtils{testReturnValues[2]})
	index := 3
	err := tb.ShowInfo(&index)
	expected := ""
	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 3, index)
	if assert.Error(t, err) {
		assert.Equal(t, "failed to check filesystem", err.Error())
	}
}

func TestDiskSpaceInspectionWithOSDataDirFilesystemError(t *testing.T) {
	tw := NewTestWriter()
	tb := NewDiskSpaceInspection(tw.CliWriter, false, "/home/ubuntu", &MockFileSystemUtils{testReturnValues[3]})
	index := 3
	tb.ShowInfo(&index)
	expected := `3. /hab directory should have 5.5GB of free space. (Currently available space : 2.5GB)
`
	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 4, index)
}
