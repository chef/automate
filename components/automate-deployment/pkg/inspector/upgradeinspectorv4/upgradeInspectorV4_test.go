package upgradeinspectorv4

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestUpgradeInspectorV4ShowInfoWithNoInspections(t *testing.T) {
	tw := NewTestWriterWithInputs("y")
	expected := `This is a major upgrade!
In this release, Elasticsearch will be migrated to OpenSearch.

You can always change the OpenSearch destination directory by using the flag:
  $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>

For more information, visit 
https://docs.chef.io/automate/major_upgrade 4.x/

Would you like to proceed with the upgrade? (y/n)
`
	ui := NewUpgradeInspectorV4(tw.CliWriter)
	err := ui.ShowInfo()
	assert.Equal(t, expected, tw.Output())
	assert.NoError(t, err)
}

func TestUpgradeInspectorV4ShowInfoWithOSDestDirIsHab(t *testing.T) {
	tw := NewTestWriterWithInputs("y")
	expected := `This is a major upgrade!
In this release, Elasticsearch will be migrated to OpenSearch.

You can always change the OpenSearch destination directory by using the flag:
  $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>

For more information, visit 
https://docs.chef.io/automate/major_upgrade 4.x/

Would you like to proceed with the upgrade? (y/n)
`
	ui := NewUpgradeInspectorV4(tw.CliWriter)
	ui.(*UpgradeInspectorV4).SetOSDestDir("/hab")
	err := ui.ShowInfo()
	assert.Equal(t, expected, tw.Output())
	assert.NoError(t, err)
}

func TestUpgradeInspectorV4ShowInfoWithOSDestDir(t *testing.T) {
	tw := NewTestWriterWithInputs("y")
	expected := `This is a major upgrade!
In this release, Elasticsearch will be migrated to OpenSearch.

For more information, visit 
https://docs.chef.io/automate/major_upgrade 4.x/

Would you like to proceed with the upgrade? (y/n)
`
	ui := NewUpgradeInspectorV4(tw.CliWriter)
	ui.(*UpgradeInspectorV4).SetOSDestDir("/home/ubuntu")
	err := ui.ShowInfo()
	assert.Equal(t, expected, tw.Output())
	assert.NoError(t, err)
}

func TestUpgradeInspectorV4ShowInfoWithNoInput(t *testing.T) {
	tw := NewTestWriterWithInputs("n")
	ui := NewUpgradeInspectorV4(tw.CliWriter)
	expected := `This is a major upgrade!
In this release, Elasticsearch will be migrated to OpenSearch.

You can always change the OpenSearch destination directory by using the flag:
  $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>

For more information, visit 
https://docs.chef.io/automate/major_upgrade 4.x/

Would you like to proceed with the upgrade? (y/n)
`
	expectedError := errors.New("Upgrade process terminated.")
	err := ui.ShowInfo()
	assert.Equal(t, expected, tw.Output())
	if assert.Error(t, err) {
		assert.Equal(t, expectedError.Error(), err.Error())
	}
}

func TestUpgradeInspectorV4ShowInfoWithInspections(t *testing.T) {
	tw := NewTestWriterWithInputs("y")
	expected := `This is a major upgrade!
In this release, Elasticsearch will be migrated to OpenSearch.

Please make sure following things are taken care of
1. You have planned downtime
2. You have taken backup by running command: chef automate backup create
3. /hab directory should have 8.8GB of free space. (Currently available space : 2.5GB)

You can always change the OpenSearch destination directory by using the flag:
  $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>

For more information, visit 
https://docs.chef.io/automate/major_upgrade 4.x/

Would you like to proceed with the upgrade? (y/n)
`
	ui := NewUpgradeInspectorV4(tw.CliWriter)
	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
	ui.AddInspection(NewDiskSpaceInspection(tw.CliWriter, false, "", &MockFileSystemUtils{values: map[string][]interface{}{"CalDirSizeInGB": {3.0}, "GetFreeSpaceinGB": {2.5}, "GetHabRootPath": {"/hab"}}}))

	err := ui.ShowInfo()
	assert.Equal(t, expected, tw.Output())
	assert.NoError(t, err)
}

func TestUpgradeInspectorV4ShowInfoWithInspectionsWithFilesystemError(t *testing.T) {
	tw := NewTestWriterWithInputs("y")
	expected := `This is a major upgrade!
In this release, Elasticsearch will be migrated to OpenSearch.

Please make sure following things are taken care of
1. You have planned downtime
2. You have taken backup by running command: chef automate backup create
`
	ui := NewUpgradeInspectorV4(tw.CliWriter)
	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
	ui.AddInspection(NewDiskSpaceInspection(tw.CliWriter, false, "", &MockFileSystemUtils{values: map[string][]interface{}{"CalDirSizeInGB": {3.0, errors.New("failed to check filesystem")}, "GetFreeSpaceinGB": {2.5}, "GetHabRootPath": {"/hab"}}}))

	err := ui.ShowInfo()
	assert.Equal(t, expected, tw.Output())
	if assert.Error(t, err) {
		assert.Equal(t, "failed to check filesystem", err.Error())
	}

}

func TestUpgradeInspectorV4ShowInfoInvalidInputError(t *testing.T) {
	tw := NewTestWriterWithInputs("t")
	expected := `This is a major upgrade!
In this release, Elasticsearch will be migrated to OpenSearch.

For more information, visit 
https://docs.chef.io/automate/major_upgrade 4.x/

Would you like to proceed with the upgrade? (y/n)
I don't understand 't'. Please type 'y' or 'n'.
Would you like to proceed with the upgrade? (y/n)
`
	ui := NewUpgradeInspectorV4(tw.CliWriter)
	ui.(*UpgradeInspectorV4).SetOSDestDir("/home/ubuntu")
	err := ui.ShowInfo()
	assert.Equal(t, expected, tw.Output())
	if assert.Error(t, err) {
		assert.Equal(t, "failed to read user input in confirmation: EOF", err.Error())
	}
}

func TestUpgradeInspectorV4Inspect(t *testing.T) {
	tw := NewTestWriter()
	ui := NewUpgradeInspectorV4(tw.CliWriter)
	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
	diskSpaceInspection := NewDiskSpaceInspection(tw.CliWriter, false, "", &MockFileSystemUtils{values: map[string][]interface{}{"CalDirSizeInGB": {3.0}, "GetFreeSpaceinGB": {2.5}, "GetHabRootPath": {"/hab"}}})
	ui.AddInspection(diskSpaceInspection)
	diskSpaceInspection.habDir = "/hab"
	diskSpaceInspection.osDestDir = "/home/ubuntu"
	diskSpaceInspection.currentHabSpace = 8.5
	diskSpaceInspection.requiredHabSpace = 5.5
	diskSpaceInspection.currentSpaceInOSDir = 8.5
	diskSpaceInspection.requiredOSDestSpace = 3.3

	expectedChecks := "Pre flight checks"
	expectedBeginHabChecking := "┤  [Checking]     /hab directory should have 5.5GB of free space"
	expectedBeginOSDestChecking := "┤  [Checking]     /home/ubuntu directory should have 3.3GB of free space"
	expectedPassHabChecking := "✔ [Passed]        /hab directory should have 5.5GB of free space"
	expectedPassOSDestChecking := "✔ [Passed]        /home/ubuntu directory should have 3.3GB of free space"

	err := ui.Inspect()
	assert.Contains(t, tw.Output(), expectedChecks)
	assert.Contains(t, tw.Output(), expectedBeginHabChecking)
	assert.Contains(t, tw.Output(), expectedBeginOSDestChecking)
	assert.Contains(t, tw.Output(), expectedPassHabChecking)
	assert.Contains(t, tw.Output(), expectedPassOSDestChecking)
	assert.NoError(t, err)
}

func TestUpgradeInspectorV4InspectHabFailed(t *testing.T) {
	tw := NewTestWriter()
	ui := NewUpgradeInspectorV4(tw.CliWriter)
	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
	diskSpaceInspection := NewDiskSpaceInspection(tw.CliWriter, false, "", &MockFileSystemUtils{values: map[string][]interface{}{"CalDirSizeInGB": {3.0}, "GetFreeSpaceinGB": {2.5}, "GetHabRootPath": {"/hab"}}})
	ui.AddInspection(diskSpaceInspection)
	diskSpaceInspection.habDir = "/hab"
	diskSpaceInspection.osDestDir = "/home/ubuntu"
	diskSpaceInspection.currentHabSpace = 8.5
	diskSpaceInspection.requiredHabSpace = 10.5
	diskSpaceInspection.currentSpaceInOSDir = 8.5
	diskSpaceInspection.requiredOSDestSpace = 3.3

	expectedChecks := "Pre flight checks"
	expectedBeginHabChecking := "┤  [Checking]     /hab directory should have 10.5GB of free space"
	expectedBeginOSDestChecking := "┤  [Checking]     /home/ubuntu directory should have 3.3GB of free space"
	expectedPassHabChecking := "✖ [Failed]        /hab directory should have 10.5GB of free space"
	expectedPassOSDestChecking := "∅ [Skipped]        /home/ubuntu directory should have 3.3GB of free space"

	err := ui.Inspect()
	assert.Contains(t, tw.Output(), expectedChecks)
	assert.Contains(t, tw.Output(), expectedBeginHabChecking)
	assert.Contains(t, tw.Output(), expectedBeginOSDestChecking)
	assert.Contains(t, tw.Output(), expectedPassHabChecking)
	assert.Contains(t, tw.Output(), expectedPassOSDestChecking)
	if assert.Error(t, err) {
		assert.EqualError(t, err, "Upgrade process terminated.: failed in Hab Space Check")
	}
}

func TestUpgradeInspectorV4InspectOSDestFailed(t *testing.T) {
	tw := NewTestWriter()
	ui := NewUpgradeInspectorV4(tw.CliWriter)
	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
	diskSpaceInspection := NewDiskSpaceInspection(tw.CliWriter, false, "", &MockFileSystemUtils{values: map[string][]interface{}{"CalDirSizeInGB": {3.0}, "GetFreeSpaceinGB": {2.5}, "GetHabRootPath": {"/hab"}}})
	ui.AddInspection(diskSpaceInspection)
	diskSpaceInspection.habDir = "/hab"
	diskSpaceInspection.osDestDir = "/home/ubuntu"
	diskSpaceInspection.currentHabSpace = 8.5
	diskSpaceInspection.requiredHabSpace = 5.5
	diskSpaceInspection.currentSpaceInOSDir = 8.5
	diskSpaceInspection.requiredOSDestSpace = 10.5

	expectedChecks := "Pre flight checks"
	expectedBeginHabChecking := "┤  [Checking]     /hab directory should have 5.5GB of free space"
	expectedBeginOSDestChecking := "┤  [Checking]     /home/ubuntu directory should have 10.5GB of free space"
	expectedPassHabChecking := "✔ [Passed]        /hab directory should have 5.5GB of free space"
	expectedPassOSDestChecking := "✖ [Failed]        /home/ubuntu directory should have 10.5GB of free space"

	err := ui.Inspect()
	assert.Contains(t, tw.Output(), expectedChecks)
	assert.Contains(t, tw.Output(), expectedBeginHabChecking)
	assert.Contains(t, tw.Output(), expectedBeginOSDestChecking)
	assert.Contains(t, tw.Output(), expectedPassHabChecking)
	assert.Contains(t, tw.Output(), expectedPassOSDestChecking)
	if assert.Error(t, err) {
		assert.EqualError(t, err, "Upgrade process terminated.: failed in OS Dest Check")
	}
}
