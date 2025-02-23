package upgradeinspectorv5

import (
	"errors"
	"testing"
	"time"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func CalDirSizeInGB(path string) (float64, error) {
	return 3.0, nil
}

func CalDirSizeInGBError(path string) (float64, error) {
	return 3.0, errors.New("failed to check filesystem")
}

func GetFreeSpaceinGB(dir string) (float64, error) {
	return 2.5, nil
}

func GetFreeSpaceinGBToPassTest(dir string) (float64, error) {
	return 12.5, nil
}

func GetFreeSpaceinGBErrorHab(dir string) (float64, error) {
	if dir == "/hab" {
		return -1.0, errors.New("failed to check filesystem")
	}
	return 2.5, nil
}

func GetFreeSpaceinGBErrorOSDestDir(dir string) (float64, error) {
	if dir != "/hab" {
		return -1.0, errors.New("failed to check filesystem")
	}
	return 2.5, nil
}
func GetHabRootPath() string {
	return "/hab"
}

func TestDiskSpaceInspection(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	tb := NewDiskSpaceInspection(tw.CliWriter, false, false, "", mfs)
	index := 3
	tb.ShowInfo(&index)
	expected := "3. The /hab directory should have at least 8.8GB of free space. (You have current available space : 2.5GB) (y/n)\n"

	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 4, index)
}

func TestDiskSpaceInspectionWithExternalOS(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	tb := NewDiskSpaceInspection(tw.CliWriter, true, false, "", mfs)
	index := 3
	tb.ShowInfo(&index)
	expected := "3. The /hab directory should have at least 5.5GB of free space. (You have current available space : 2.5GB)\n (y/n)\n"
	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 4, index)
}

func TestDiskSpaceInspectionWithExternalPG(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	tb := NewDiskSpaceInspection(tw.CliWriter, false, true, "", mfs)
	index := 3
	tb.ShowInfo(&index)
	expected := `3. The /hab directory should have at least 5.5GB of free space. (You have current available space : 2.5GB)
`
	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 4, index)
}

func TestDiskSpaceInspectionWithOSDataDir(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	tb := NewDiskSpaceInspection(tw.CliWriter, false, false, "/home/ubuntu", mfs)
	index := 3
	tb.ShowInfo(&index)
	expected := `3. The /hab directory should have at least 5.5GB of free space. (You have current available space : 2.5GB)
4. The /home/ubuntu directory should have at least 3.3GB of free space. (You have current available space : 2.5GB)
`
	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 5, index)
}

func TestDiskSpaceInspectionFileSystemError(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGBError,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	tb := NewDiskSpaceInspection(tw.CliWriter, false, false, "/home/ubuntu", mfs)
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
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGBError,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	tb := NewDiskSpaceInspection(tw.CliWriter, false, false, "/home/ubuntu", mfs)
	index := 3
	err := tb.ShowInfo(&index)
	expected := ""
	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 3, index)
	if assert.Error(t, err) {
		assert.Equal(t, "failed to check filesystem", err.Error())
	}
}

func TestDiskSpaceInspectionWithHabFilesystemError(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGBErrorHab,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	tb := NewDiskSpaceInspection(tw.CliWriter, false, false, "/home/ubuntu", mfs)
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
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGBErrorOSDestDir,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	tb := NewDiskSpaceInspection(tw.CliWriter, false, false, "/home/ubuntu", mfs)
	index := 3
	tb.ShowInfo(&index)
	expected := `3. The /hab directory should have at least 5.5GB of free space. (You have current available space : 2.5GB)
`
	assert.Equal(t, expected, tw.Output())
	assert.Equal(t, 4, index)
}

func TestDiskSpaceInspect(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	tb := NewDiskSpaceInspection(tw.CliWriter, false, false, "/home/ubuntu", mfs)
	tb.habDir = "/hab"
	tb.osDestDir = "/home/ubuntu"
	tb.currentHabSpace = 8.5
	tb.requiredHabSpace = 5.5
	tb.currentSpaceInOSDir = 8.5
	tb.requiredOSDestSpace = 5.5
	tb.checkDelay = 100 * time.Millisecond
	err := tb.Inspect()
	expectedBeginHabChecking := "┤  [Checking]\tThe /hab directory should have at least 5.5GB of free space"
	expectedBeginOSDestChecking := "┤  [Checking]\tThe /home/ubuntu directory should have at least 5.5GB of free space"

	expectedPassHabChecking := "✔  [Passed]\tThe /hab directory should have at least 5.5GB of free space"
	expectedPassOSDestChecking := "✔  [Passed]\tThe /home/ubuntu directory should have at least 5.5GB of free space"
	assert.Contains(t, tw.Output(), expectedBeginHabChecking)
	assert.Contains(t, tw.Output(), expectedBeginOSDestChecking)
	assert.Contains(t, tw.Output(), expectedPassHabChecking)
	assert.Contains(t, tw.Output(), expectedPassOSDestChecking)
	assert.NoError(t, err)
}

func TestDiskSpaceInspectHabFailed(t *testing.T) {
	tw := majorupgrade_utils.NewCustomWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGBErrorHab,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	tb := NewDiskSpaceInspection(tw.CliWriter, false, false, "/home/ubuntu", mfs)
	tb.habDir = "/hab"
	tb.osDestDir = "/home/ubuntu"
	tb.currentHabSpace = 8.5
	tb.requiredHabSpace = 10.5
	tb.currentSpaceInOSDir = 8.5
	tb.requiredOSDestSpace = 5.5
	tb.checkDelay = 100 * time.Millisecond
	err := tb.Inspect()
	expectedBeginHabChecking := "┤  [Checking]\tThe /hab directory should have at least 10.5GB of free space"

	expectedResult := `✖  [Failed]	The /hab directory should have at least 10.5GB of free space
 ⊖  [Skipped]	The /home/ubuntu directory should have at least 5.5GB of free space
`
	// 	expectedEnsureMsg := `
	// Please ensure the available free space is 10.5GB
	// and run chef-automate upgrade run --major command again
	// `
	assert.Contains(t, tw.Output(), expectedBeginHabChecking)
	assert.Contains(t, tw.Output(), expectedResult)
	// assert.Contains(t, tw.Output(), expectedEnsureMsg)
	if assert.Error(t, err) {
		assert.Equal(t, "failed in Hab Space Check", err.Error())
	}
}
