package upgradeinspectorv5

// import (
// 	"errors"
// 	"fmt"
// 	"io"
// 	"strings"
// 	"testing"
// 	"time"

// 	"github.com/chef/automate/lib/io/fileutils"
// 	"github.com/chef/automate/lib/majorupgrade_utils"
// 	"github.com/stretchr/testify/assert"
// )

// const (
// 	TITLE_MSG = `This is a major upgrade!
// In this release, Elasticsearch will be migrated to OpenSearch.`
// 	RUN_WITH_OS_FLAG = `You can always change the OpenSearch destination directory by using the flag:
//   $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>`
// 	PROCEED_CONF = `For more information, visit
// https://docs.chef.io/automate/major_upgrade_4.x/

// Would you like to proceed with the upgrade? (y/n)`
// 	INFO_MSG = `Before proceeding, please ensure:
// 1. You have scheduled downtime for the duration of the upgrade.
// 2. You have taken a backup by running the command: chef automate backup create.
// 3. The /hab directory should have at least 8.8GB of free space. (You have current available space : 2.5GB)`
// 	INSPECTION_LIST = `Following Pre-flight checks will be conducted
// 1. The /hab directory should have at least 5.5GB of free space
// 2. The /home/ubuntu directory should have at least 3.3GB of free space
// `
// 	ALL_SKIPPED_OS = `Pre flight checks
//  ⊖  [Skipped]	The /hab directory should have at least 5.5GB of free space
//  ⊖  [Skipped]	The /home/ubuntu directory should have at least 3.3GB of free space
//  ⊖  [Skipped]	Elasticsearch indices are in version 6

// [Error] Please make sure all services are healthy by running chef-automate status`
// 	HAB_SPACE_ERR = `✖  [Failed]	The /hab directory should have at least 8.8GB of free space
//  ⊖  [Skipped]	Elasticsearch indices are in version 6

// [Error] Required Space : 8.8GB
//         Available space : 2.5GB

// Please ensure the available free space is 8.8GB
// and run chef-automate upgrade run --major command again
// `
// )

// func TestUpgradeInspectorV5ShowInfoWithNoInspections(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("y")
// 	expected := fmt.Sprintf("%s\n\n%s\n\n%s\n", TITLE_MSG, RUN_WITH_OS_FLAG, PROCEED_CONF)
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 	}, mfs, 10)
// 	err := ui.ShowInfo()
// 	t.Log(tw.Output())
// 	t.Log(expected)
// 	assert.Equal(t, expected, tw.Output())
// 	assert.NoError(t, err)
// }

// func TestUpgradeInspectorV5ShowInfoWithOSDestDirIsHab(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("y")
// 	expected := fmt.Sprintf("%s\n\n%s\n\n%s\n", TITLE_MSG, RUN_WITH_OS_FLAG, PROCEED_CONF)
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 	}, mfs, 10)
// 	ui.(*UpgradeInspectorV5).SetOSDestDir("/hab")
// 	err := ui.ShowInfo()
// 	assert.Equal(t, expected, tw.Output())
// 	assert.NoError(t, err)
// }

// func TestUpgradeInspectorV5ShowInfoWithOSDestDir(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("y")
// 	expected := fmt.Sprintf("%s\n\n%s\n", TITLE_MSG, PROCEED_CONF)
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 	}, mfs, 10)
// 	ui.(*UpgradeInspectorV5).SetOSDestDir("/home/ubuntu")
// 	err := ui.ShowInfo()
// 	assert.Equal(t, expected, tw.Output())
// 	assert.NoError(t, err)
// }

// func TestUpgradeInspectorV5ShowInfoWithNoInput(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("n")
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 	}, mfs, 10)
// 	expected := fmt.Sprintf("%s\n\n%s\n\n%s\n", TITLE_MSG, RUN_WITH_OS_FLAG, PROCEED_CONF)
// 	expectedError := errors.New(UPGRADE_TERMINATED)
// 	err := ui.ShowInfo()
// 	assert.Equal(t, expected, tw.Output())
// 	if assert.Error(t, err) {
// 		assert.Equal(t, expectedError.Error(), err.Error())
// 	}
// }

// func TestUpgradeInspectorV5ShowInfoWithInspections(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("y")
// 	expected := fmt.Sprintf("%s\n\n%s\n\n%s\n\n%s\n", TITLE_MSG, INFO_MSG, RUN_WITH_OS_FLAG, PROCEED_CONF)

// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
// 	}, mfs, 10)
// 	ui.(*UpgradeInspectorV5).AddDefaultInspections()

// 	err := ui.ShowInfo()
// 	assert.Equal(t, expected, tw.Output())
// 	assert.NoError(t, err)
// }

// func TestUpgradeInspectorV5ShowInfoWithInspectionsWithFilesystemError(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("y")
// 	expected := fmt.Sprintf(`%s

// Before proceeding, please ensure:
// 1. You have scheduled downtime for the duration of the upgrade.
// 2. You have taken a backup by running the command: chef automate backup create.
// `, TITLE_MSG)
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGBError,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
// 	}, mfs, 10)
// 	ui.(*UpgradeInspectorV5).AddDefaultInspections()
// 	err := ui.ShowInfo()
// 	assert.Equal(t, expected, tw.Output())
// 	if assert.Error(t, err) {
// 		assert.Equal(t, "failed to check filesystem", err.Error())
// 	}

// }

// func TestUpgradeInspectorV5ShowInfoInvalidInputError(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("t")
// 	expected := fmt.Sprintf(`%s

// %s
// I don't understand 't'. Please type 'y' or 'n'.
// Would you like to proceed with the upgrade? (y/n)
// `, TITLE_MSG, PROCEED_CONF)
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 	}, mfs, 10)
// 	ui.(*UpgradeInspectorV5).SetOSDestDir("/home/ubuntu")
// 	err := ui.ShowInfo()
// 	assert.Equal(t, expected, tw.Output())
// 	if assert.Error(t, err) {
// 		assert.Equal(t, "failed to read user input in confirmation: EOF", err.Error())
// 	}
// }

// func TestUpgradeInspectorV5Inspect(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriter()
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 	}, mfs, 10)
// 	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
// 	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
// 	diskSpaceInspection := NewDiskSpaceInspection(tw.CliWriter, false, false, "", mfs)
// 	ui.AddInspection(diskSpaceInspection)
// 	diskSpaceInspection.habDir = "/hab"
// 	diskSpaceInspection.osDestDir = "/home/ubuntu"
// 	diskSpaceInspection.currentHabSpace = 8.5
// 	diskSpaceInspection.requiredHabSpace = 5.5
// 	diskSpaceInspection.currentSpaceInOSDir = 8.5
// 	diskSpaceInspection.requiredOSDestSpace = 3.3
// 	diskSpaceInspection.checkDelay = 100 * time.Millisecond

// 	expectedChecks := "Pre flight checks"
// 	expectedBeginHabChecking := "┤  [Checking]\tThe /hab directory should have at least 5.5GB of free space"
// 	expectedBeginOSDestChecking := "┤  [Checking]\tThe /home/ubuntu directory should have at least 3.3GB of free space"
// 	expectedPassHabChecking := "✔  [Passed]\tThe /hab directory should have at least 5.5GB of free space"
// 	expectedPassOSDestChecking := "✔  [Passed]\tThe /home/ubuntu directory should have at least 3.3GB of free space"

// 	err := ui.Inspect()
// 	assert.Contains(t, tw.Output(), expectedChecks)
// 	assert.Contains(t, tw.Output(), expectedBeginHabChecking)
// 	assert.Contains(t, tw.Output(), expectedBeginOSDestChecking)
// 	assert.Contains(t, tw.Output(), expectedPassHabChecking)
// 	assert.Contains(t, tw.Output(), expectedPassOSDestChecking)
// 	assert.NoError(t, err)
// }

// func TestUpgradeInspectorV5InspectHabFailed(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriter()
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 	}, mfs, 10)
// 	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
// 	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
// 	diskSpaceInspection := NewDiskSpaceInspection(tw.CliWriter, false, false, "", mfs)
// 	ui.AddInspection(diskSpaceInspection)
// 	diskSpaceInspection.habDir = "/hab"
// 	diskSpaceInspection.osDestDir = "/home/ubuntu"
// 	diskSpaceInspection.currentHabSpace = 8.5
// 	diskSpaceInspection.requiredHabSpace = 10.5
// 	diskSpaceInspection.currentSpaceInOSDir = 8.5
// 	diskSpaceInspection.requiredOSDestSpace = 3.3
// 	diskSpaceInspection.checkDelay = 100 * time.Millisecond

// 	expectedChecks := "Pre flight checks"
// 	expectedBeginHabChecking := "┤  [Checking]\tThe /hab directory should have at least 10.5GB of free space"
// 	expectedPassHabChecking := "✖  [Failed]\tThe /hab directory should have at least 10.5GB of free space"
// 	expectedPassOSDestChecking := "⊖  [Skipped]\tThe /home/ubuntu directory should have at least 3.3GB of free space"
// 	expectedEnsureSpace := "Please ensure the available free space is 10.5GB"

// 	err := ui.Inspect()
// 	assert.Error(t, err)
// 	err = ui.RunExitAction()
// 	assert.NoError(t, err)
// 	assert.Contains(t, tw.Output(), expectedChecks)
// 	assert.Contains(t, tw.Output(), expectedBeginHabChecking)
// 	assert.Contains(t, tw.Output(), expectedPassHabChecking)
// 	assert.Contains(t, tw.Output(), expectedPassOSDestChecking)
// 	assert.Contains(t, tw.Output(), expectedEnsureSpace)
// }

// func TestUpgradeInspectorV5InspectOSDestFailed(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriter()
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 	}, mfs, 10)
// 	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
// 	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
// 	diskSpaceInspection := NewDiskSpaceInspection(tw.CliWriter, false, false, "", mfs)
// 	ui.AddInspection(diskSpaceInspection)
// 	diskSpaceInspection.habDir = "/hab"
// 	diskSpaceInspection.osDestDir = "/home/ubuntu"
// 	diskSpaceInspection.currentHabSpace = 8.5
// 	diskSpaceInspection.requiredHabSpace = 5.5
// 	diskSpaceInspection.currentSpaceInOSDir = 8.5
// 	diskSpaceInspection.requiredOSDestSpace = 10.5
// 	diskSpaceInspection.checkDelay = 100 * time.Millisecond

// 	expectedChecks := "Pre flight checks"
// 	expectedBeginHabChecking := "┤  [Checking]\tThe /hab directory should have at least 5.5GB of free space"
// 	expectedBeginOSDestChecking := "┤  [Checking]\tThe /home/ubuntu directory should have at least 10.5GB of free space"
// 	expectedPassHabChecking := "✔  [Passed]\tThe /hab directory should have at least 5.5GB of free space"
// 	expectedPassOSDestChecking := "✖  [Failed]\tThe /home/ubuntu directory should have at least 10.5GB of free space"
// 	expectedEnsureSpace := "Please ensure the available free space is 10.5GB"

// 	err := ui.Inspect()
// 	assert.Error(t, err)
// 	err = ui.RunExitAction()
// 	assert.NoError(t, err)
// 	assert.Contains(t, tw.Output(), expectedChecks)
// 	assert.Contains(t, tw.Output(), expectedBeginHabChecking)
// 	assert.Contains(t, tw.Output(), expectedBeginOSDestChecking)
// 	assert.Contains(t, tw.Output(), expectedPassHabChecking)
// 	assert.Contains(t, tw.Output(), expectedPassOSDestChecking)
// 	assert.Contains(t, tw.Output(), expectedEnsureSpace)
// }

// func TestUpgradeInspectorV5InspectShowInspectionListForOsDest(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriter()
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 	}, mfs, 10)
// 	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
// 	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
// 	diskSpaceInspection := NewDiskSpaceInspection(tw.CliWriter, false, false, "", mfs)
// 	ui.AddInspection(diskSpaceInspection)
// 	diskSpaceInspection.habDir = "/hab"
// 	diskSpaceInspection.osDestDir = "/home/ubuntu"
// 	diskSpaceInspection.currentHabSpace = 8.5
// 	diskSpaceInspection.requiredHabSpace = 5.5
// 	diskSpaceInspection.currentSpaceInOSDir = 8.5
// 	diskSpaceInspection.requiredOSDestSpace = 3.3

// 	expected := INSPECTION_LIST

// 	ui.ShowInspectionList()
// 	assert.Contains(t, tw.Output(), expected)
// }

// func TestUpgradeInspectorV5ShowInspectionListForExternal(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("y", "1")
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGBToPassTest,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return true },
// 		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
// 		GetBackupS3URLFunc: func(timeout int64) (string, error) {
// 			return "https://s3.us-east-1.amazonaws.com", nil
// 		},
// 		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
// 			return "", "", nil
// 		},
// 		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
// 			return false, nil
// 		},
// 		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
// 			return "", "", nil
// 		},
// 	}, mfs, 10)

// 	ui.(*UpgradeInspectorV5).AddDefaultInspections()

// 	expected := "Following Pre-flight checks will be conducted\n1. The /hab directory should have at least 5.5GB of free space"

// 	ui.ShowInfo()
// 	ui.ShowInspectionList()
// 	assert.Contains(t, tw.Output(), expected)
// }

// func TestUpgradeInspectorV5ShowInspectionListForEmbedded(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("y", "1")
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGBToPassTest,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
// 		GetBackupS3URLFunc: func(timeout int64) (string, error) {
// 			return "https://s3.us-east-1.amazonaws.com", nil
// 		},
// 		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
// 			return "", "", nil
// 		},
// 		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
// 			return false, nil
// 		},
// 		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
// 			return "", "", nil
// 		},
// 	}, mfs, 10)

// 	ui.(*UpgradeInspectorV5).AddDefaultInspections()

// 	expected := "Following Pre-flight checks will be conducted\n1. The /hab directory should have at least 8.8GB of free space"

// 	ui.ShowInfo()
// 	ui.ShowInspectionList()
// 	assert.Contains(t, tw.Output(), expected)
// }

// func TestUpgradeInspectorV5RunInspectForOsDestDirSkipped(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("y", "1")
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
// 		ExecRequestFunc:             NewUpgradeV5Utils().ExecRequest,
// 		GetBackupS3URLFunc: func(timeout int64) (string, error) {
// 			return "https://s3.us-east-1.amazonaws.com", nil
// 		},
// 		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
// 			return "", "", nil
// 		},
// 		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
// 			return false, nil
// 		},
// 		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
// 			return "", "", nil
// 		},
// 		GetServicesStatusFunc: func() (bool, error) {
// 			return true, nil
// 		},
// 	}, mfs, 10)

// 	ui.(*UpgradeInspectorV5).SetOSDestDir("/home/ubuntu")
// 	ui.(*UpgradeInspectorV5).AddDefaultInspections()

// 	expected1 := "[Skipped]\tThe /home/ubuntu directory should have at least 3.3GB of free space"
// 	expected2 := "[Skipped]\tElasticsearch indices are in version 6\n"

// 	ui.ShowInfo()
// 	ui.ShowInspectionList()
// 	ui.Inspect()
// 	assert.Contains(t, tw.Output(), expected1)
// 	assert.Contains(t, tw.Output(), expected2)
// }

// func TestUpgradeInspectorV5ExitMessage(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("y", "1")
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 		WriteToFileFunc:      func(filepath string, data []byte) error { return nil },
// 	}
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
// 		GetBackupS3URLFunc: func(timeout int64) (string, error) {
// 			return "https://s3.us-east-1.amazonaws.com", nil
// 		},
// 		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
// 			return "", "", nil
// 		},
// 		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
// 			return false, nil
// 		},
// 		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
// 			return "", "", nil
// 		},
// 		GetServicesStatusFunc: func() (bool, error) {
// 			return true, nil
// 		},
// 		ExecRequestFunc: NewUpgradeV5Utils().ExecRequest,
// 	}, mfs, 10)

// 	expected := HAB_SPACE_ERR

// 	err := ui.RunUpgradeInspector("", false)
// 	assert.True(t, err)
// 	assert.Contains(t, tw.Output(), expected)
// }

// func TestUpgradeInspectorV5ExitMessageFailedMaintenance(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("y", "1")
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGBToPassTest,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 		WriteToFileFunc:      func(filepath string, data []byte) error { return nil },
// 	}
// 	shardingCalledCount := 0
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
// 		GetBackupS3URLFunc: func(timeout int64) (string, error) {
// 			return "https://s3.us-east-1.amazonaws.com", nil
// 		},
// 		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
// 			return "", "", nil
// 		},
// 		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
// 			return false, errors.New("unreachable")
// 		},
// 		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
// 			return "", "", nil
// 		},
// 		GetServicesStatusFunc: func() (bool, error) {
// 			return true, nil
// 		},
// 		ExecRequestFunc: func(url, methodType string, requestBody io.Reader) ([]byte, error) {
// 			if strings.Contains(url, "_cluster/settings") {
// 				shardingCalledCount++
// 				return []byte{}, nil
// 			} else if strings.Contains(url, "index.version") {
// 				return []byte(`{"node-attribute":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}},"comp-2-run-info":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}}}`), nil
// 			} else if strings.Contains(url, "_cluster/stats") {
// 				return []byte(`{"indices":{"shards":{"total":51}}}`), nil
// 			} else if strings.Contains(url, "indices") {
// 				return []byte(INDEX_LIST), nil
// 			} else {
// 				return []byte{}, nil
// 			}
// 		},
// 	}, mfs, 10)

// 	ui.(*UpgradeInspectorV5).AddDefaultInspections()

// 	expected := `[Error] unreachable`

// 	err := ui.ShowInfo()
// 	assert.NoError(t, err)
// 	ui.ShowInspectionList()
// 	err = ui.Inspect()
// 	assert.Error(t, err)
// 	err = ui.RollBackChangesOnError()
// 	assert.NoError(t, err)
// 	err = ui.RunExitAction()
// 	assert.NoError(t, err)
// 	assert.Equal(t, 2, shardingCalledCount)
// 	assert.Contains(t, tw.Output(), expected)
// }

// func TestUpgradeInspectorV5SkipAllEnsureStatusFailure(t *testing.T) {
// 	tw := majorupgrade_utils.NewCustomWriterWithInputs("y", "1")
// 	mfs := &fileutils.MockFileSystemUtils{
// 		CalDirSizeInGBFunc:   CalDirSizeInGB,
// 		GetFreeSpaceinGBFunc: GetFreeSpaceinGBToPassTest,
// 		GetHabRootPathFunc:   GetHabRootPath,
// 		WriteToFileFunc:      func(filepath string, data []byte) error { return nil },
// 	}
// 	shardingCalledCount := 0
// 	ui := NewUpgradeInspectorV5(tw.CliWriter, &MockUpgradeV5UtilsImp{
// 		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
// 		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
// 		GetBackupS3URLFunc: func(timeout int64) (string, error) {
// 			return "https://s3.us-east-1.amazonaws.com", nil
// 		},
// 		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
// 			return "", "", nil
// 		},
// 		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
// 			return false, errors.New("unreachable")
// 		},
// 		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
// 			return "", "", nil
// 		},
// 		GetServicesStatusFunc: func() (bool, error) {
// 			return false, nil
// 		},
// 		ExecRequestFunc: func(url, methodType string, requestBody io.Reader) ([]byte, error) {
// 			if strings.Contains(url, "_cluster/settings") {
// 				shardingCalledCount++
// 				return []byte{}, nil
// 			} else if strings.Contains(url, "index.version") {
// 				return []byte(`{"node-attribute":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}},"comp-2-run-info":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}}}`), nil
// 			} else if strings.Contains(url, "_cluster/stats") {
// 				return []byte(`{"indices":{"shards":{"total":51}}}`), nil
// 			} else if strings.Contains(url, "indices") {
// 				return []byte(INDEX_LIST), nil
// 			} else {
// 				return []byte{}, nil
// 			}
// 		},
// 	}, mfs, 10)

// 	expected := ALL_SKIPPED_OS

// 	iserr := ui.RunUpgradeInspector("/home/ubuntu", false)
// 	assert.Equal(t, iserr, true)
// 	assert.Contains(t, tw.Output(), expected)
// }
