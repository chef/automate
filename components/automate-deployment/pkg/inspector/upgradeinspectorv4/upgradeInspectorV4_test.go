package upgradeinspectorv4

import (
	"errors"
	"io"
	"strings"
	"testing"
	"time"

	"github.com/chef/automate/lib/io/fileutils"
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
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
	}, mfs, 10)
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
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
	}, mfs, 10)
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
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
	}, mfs, 10)
	ui.(*UpgradeInspectorV4).SetOSDestDir("/home/ubuntu")
	err := ui.ShowInfo()
	assert.Equal(t, expected, tw.Output())
	assert.NoError(t, err)
}

func TestUpgradeInspectorV4ShowInfoWithNoInput(t *testing.T) {
	tw := NewTestWriterWithInputs("n")
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
	}, mfs, 10)
	expected := `This is a major upgrade!
In this release, Elasticsearch will be migrated to OpenSearch.

You can always change the OpenSearch destination directory by using the flag:
  $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>

For more information, visit 
https://docs.chef.io/automate/major_upgrade 4.x/

Would you like to proceed with the upgrade? (y/n)
`
	expectedError := errors.New(UPGRADE_TERMINATED)
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
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
	}, mfs, 10)
	ui.(*UpgradeInspectorV4).AddDefaultInspections()

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
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGBError,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
	}, mfs, 10)
	ui.(*UpgradeInspectorV4).AddDefaultInspections()
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
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
	}, mfs, 10)
	ui.(*UpgradeInspectorV4).SetOSDestDir("/home/ubuntu")
	err := ui.ShowInfo()
	assert.Equal(t, expected, tw.Output())
	if assert.Error(t, err) {
		assert.Equal(t, "failed to read user input in confirmation: EOF", err.Error())
	}
}

func TestUpgradeInspectorV4Inspect(t *testing.T) {
	tw := NewTestWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
	}, mfs, 10)
	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
	diskSpaceInspection := NewDiskSpaceInspection(tw.CliWriter, false, "", mfs)
	ui.AddInspection(diskSpaceInspection)
	diskSpaceInspection.habDir = "/hab"
	diskSpaceInspection.osDestDir = "/home/ubuntu"
	diskSpaceInspection.currentHabSpace = 8.5
	diskSpaceInspection.requiredHabSpace = 5.5
	diskSpaceInspection.currentSpaceInOSDir = 8.5
	diskSpaceInspection.requiredOSDestSpace = 3.3
	diskSpaceInspection.checkDelay = 100 * time.Millisecond

	expectedChecks := "Pre flight checks"
	expectedBeginHabChecking := "┤  [Checking]\t/hab directory should have 5.5GB of free space"
	expectedBeginOSDestChecking := "┤  [Checking]\t/home/ubuntu directory should have 3.3GB of free space"
	expectedPassHabChecking := "✔  [Passed]\t/hab directory should have 5.5GB of free space"
	expectedPassOSDestChecking := "✔  [Passed]\t/home/ubuntu directory should have 3.3GB of free space"

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
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
	}, mfs, 10)
	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
	diskSpaceInspection := NewDiskSpaceInspection(tw.CliWriter, false, "", mfs)
	ui.AddInspection(diskSpaceInspection)
	diskSpaceInspection.habDir = "/hab"
	diskSpaceInspection.osDestDir = "/home/ubuntu"
	diskSpaceInspection.currentHabSpace = 8.5
	diskSpaceInspection.requiredHabSpace = 10.5
	diskSpaceInspection.currentSpaceInOSDir = 8.5
	diskSpaceInspection.requiredOSDestSpace = 3.3
	diskSpaceInspection.checkDelay = 100 * time.Millisecond

	expectedChecks := "Pre flight checks"
	expectedBeginHabChecking := "┤  [Checking]\t/hab directory should have 10.5GB of free space"
	expectedPassHabChecking := "✖  [Failed]\t/hab directory should have 10.5GB of free space"
	expectedPassOSDestChecking := "⊖  [Skipped]\t/home/ubuntu directory should have 3.3GB of free space"
	expectedEnsureSpace := "Please ensure the available free space is 10.5GB"

	err := ui.Inspect()
	assert.NoError(t, err)
	err = ui.RunExitAction()
	assert.NoError(t, err)
	assert.Contains(t, tw.Output(), expectedChecks)
	assert.Contains(t, tw.Output(), expectedBeginHabChecking)
	assert.Contains(t, tw.Output(), expectedPassHabChecking)
	assert.Contains(t, tw.Output(), expectedPassOSDestChecking)
	assert.Contains(t, tw.Output(), expectedEnsureSpace)
}

func TestUpgradeInspectorV4InspectOSDestFailed(t *testing.T) {
	tw := NewTestWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
	}, mfs, 10)
	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
	diskSpaceInspection := NewDiskSpaceInspection(tw.CliWriter, false, "", mfs)
	ui.AddInspection(diskSpaceInspection)
	diskSpaceInspection.habDir = "/hab"
	diskSpaceInspection.osDestDir = "/home/ubuntu"
	diskSpaceInspection.currentHabSpace = 8.5
	diskSpaceInspection.requiredHabSpace = 5.5
	diskSpaceInspection.currentSpaceInOSDir = 8.5
	diskSpaceInspection.requiredOSDestSpace = 10.5
	diskSpaceInspection.checkDelay = 100 * time.Millisecond

	expectedChecks := "Pre flight checks"
	expectedBeginHabChecking := "┤  [Checking]\t/hab directory should have 5.5GB of free space"
	expectedBeginOSDestChecking := "┤  [Checking]\t/home/ubuntu directory should have 10.5GB of free space"
	expectedPassHabChecking := "✔  [Passed]\t/hab directory should have 5.5GB of free space"
	expectedPassOSDestChecking := "✖  [Failed]\t/home/ubuntu directory should have 10.5GB of free space"
	expectedEnsureSpace := "Please ensure the available free space is 10.5GB"

	err := ui.Inspect()
	assert.NoError(t, err)
	err = ui.RunExitAction()
	assert.NoError(t, err)
	assert.Contains(t, tw.Output(), expectedChecks)
	assert.Contains(t, tw.Output(), expectedBeginHabChecking)
	assert.Contains(t, tw.Output(), expectedBeginOSDestChecking)
	assert.Contains(t, tw.Output(), expectedPassHabChecking)
	assert.Contains(t, tw.Output(), expectedPassOSDestChecking)
	assert.Contains(t, tw.Output(), expectedEnsureSpace)
}

func TestUpgradeInspectorV4InspectShowInspectionListForOsDest(t *testing.T) {
	tw := NewTestWriter()
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
	}, mfs, 10)
	ui.AddInspection(NewPlannedDownTimeInspection(tw.CliWriter))
	ui.AddInspection(NewTakeBackupInspection(tw.CliWriter))
	diskSpaceInspection := NewDiskSpaceInspection(tw.CliWriter, false, "", mfs)
	ui.AddInspection(diskSpaceInspection)
	diskSpaceInspection.habDir = "/hab"
	diskSpaceInspection.osDestDir = "/home/ubuntu"
	diskSpaceInspection.currentHabSpace = 8.5
	diskSpaceInspection.requiredHabSpace = 5.5
	diskSpaceInspection.currentSpaceInOSDir = 8.5
	diskSpaceInspection.requiredOSDestSpace = 3.3

	expected := `Following Pre-flight checks will be conducted
1. /hab directory should have 5.5GB of free space
2. /home/ubuntu directory should have 3.3GB of free space
`

	ui.ShowInspectionList()
	assert.Contains(t, tw.Output(), expected)
}

func TestUpgradeInspectorV4ShowInspectionListForExternal(t *testing.T) {
	tw := NewTestWriterWithInputs("y", "1")
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGBToPassTest,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return true },
		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
		GetBackupS3URLFunc: func(timeout int64) (string, error) {
			return "https://s3.us-east-1.amazonaws.com", nil
		},
		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
			return false, nil
		},
		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
	}, mfs, 10)

	ui.(*UpgradeInspectorV4).AddDefaultInspections()

	expected := "Following Pre-flight checks will be conducted\n1. /hab directory should have 5.5GB of free space"

	ui.ShowInfo()
	ui.ShowInspectionList()
	assert.Contains(t, tw.Output(), expected)
}

func TestUpgradeInspectorV4ShowInspectionListForEmbedded(t *testing.T) {
	tw := NewTestWriterWithInputs("y", "1")
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGBToPassTest,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
		GetBackupS3URLFunc: func(timeout int64) (string, error) {
			return "https://s3.us-east-1.amazonaws.com", nil
		},
		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
			return false, nil
		},
		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
	}, mfs, 10)

	ui.(*UpgradeInspectorV4).AddDefaultInspections()

	expected := "Following Pre-flight checks will be conducted\n1. /hab directory should have 8.8GB of free space"

	ui.ShowInfo()
	ui.ShowInspectionList()
	assert.Contains(t, tw.Output(), expected)
}

func TestUpgradeInspectorV4RunInspectForOsDestDirSkipped(t *testing.T) {
	tw := NewTestWriterWithInputs("y", "1")
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
		ExecRequestFunc:             ExecRequestNonAutomate,
		GetBackupS3URLFunc: func(timeout int64) (string, error) {
			return "https://s3.us-east-1.amazonaws.com", nil
		},
		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
			return false, nil
		},
		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
		GetServicesStatusFunc: func() (bool, error) {
			return true, nil
		},
	}, mfs, 10)

	ui.(*UpgradeInspectorV4).SetOSDestDir("/home/ubuntu")
	ui.(*UpgradeInspectorV4).AddDefaultInspections()

	expected1 := "[Skipped]\t/home/ubuntu directory should have 3.3GB of free space"
	expected2 := "[Skipped]\tElasticsearch indices are in version 6\n"

	ui.ShowInfo()
	ui.ShowInspectionList()
	ui.Inspect()
	assert.Contains(t, tw.Output(), expected1)
	assert.Contains(t, tw.Output(), expected2)
}

func TestUpgradeInspectorV4ExitMessage(t *testing.T) {
	tw := NewTestWriterWithInputs("y", "1")
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGB,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
		GetBackupS3URLFunc: func(timeout int64) (string, error) {
			return "https://s3.us-east-1.amazonaws.com", nil
		},
		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
			return false, nil
		},
		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
		GetServicesStatusFunc: func() (bool, error) {
			return true, nil
		},
		ExecRequestFunc: ExecRequestNonAutomate,
		WriteToFileFunc: func(filepath string, data []byte) error { return nil },
	}, mfs, 10)

	ui.(*UpgradeInspectorV4).AddDefaultInspections()

	expected := `✖  [Failed]	/hab directory should have 8.8GB of free space
 ⊖  [Skipped]	Elasticsearch indices are in version 6

[Error] Required Space : 8.8GB
        Available space : 2.5GB

Please ensure the available free space is 8.8GB
and run chef-automate upgrade run --major command again
Upgrade process terminated.`

	err := ui.ShowInfo()
	assert.NoError(t, err)
	ui.ShowInspectionList()
	err = ui.Inspect()
	assert.NoError(t, err)
	err = ui.RollBackChangesOnError()
	assert.NoError(t, err)
	err = ui.RunExitAction()
	assert.NoError(t, err)
	t.Log(tw.Output())
	assert.Contains(t, tw.Output(), expected)
}

func TestUpgradeInspectorV4ExitMessageFailedMaintenance(t *testing.T) {
	tw := NewTestWriterWithInputs("y", "1")
	mfs := &fileutils.MockFileSystemUtils{
		CalDirSizeInGBFunc:   CalDirSizeInGB,
		GetFreeSpaceinGBFunc: GetFreeSpaceinGBToPassTest,
		GetHabRootPathFunc:   GetHabRootPath,
	}
	shardingCalledCount := 0
	ui := NewUpgradeInspectorV4(tw.CliWriter, &MockUpgradeV4UtilsImp{
		IsExternalElasticSearchFunc: func(timeout int64) bool { return false },
		GetESBasePathFunc:           func(timeout int64) string { return "http://localhost:10144/" },
		GetBackupS3URLFunc: func(timeout int64) (string, error) {
			return "https://s3.us-east-1.amazonaws.com", nil
		},
		PatchS3backupURLFunc: func(timeout int64) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
		GetMaintenanceStatusFunc: func(timeout int64) (bool, error) {
			return false, errors.New("unreachable")
		},
		SetMaintenanceModeFunc: func(timeout int64, status bool) (stdOut, stdErr string, err error) {
			return "", "", nil
		},
		GetServicesStatusFunc: func() (bool, error) {
			return true, nil
		},
		ExecRequestFunc: func(url, methodType string, requestBody io.Reader) ([]byte, error) {
			if strings.Contains(url, "_cluster/settings") {
				shardingCalledCount++
				return []byte{}, nil
			} else if strings.Contains(url, "index.version") {
				return []byte(`{"node-attribute":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}},"comp-2-run-info":{"settings":{"index":{"version":{"created_string":"5.8.23","created":"6082399"}}}}}`), nil
			} else if strings.Contains(url, "_cluster/stats") {
				return []byte(`{"indices":{"shards":{"total":51}}}`), nil
			} else if strings.Contains(url, "indices") {
				return []byte(INDEX_LIST), nil
			} else {
				return []byte{}, nil
			}
		},
		WriteToFileFunc: func(filepath string, data []byte) error { return nil },
	}, mfs, 10)

	ui.(*UpgradeInspectorV4).AddDefaultInspections()

	expected := `[Error] unreachable
Upgrade process terminated.`

	err := ui.ShowInfo()
	assert.NoError(t, err)
	ui.ShowInspectionList()
	err = ui.Inspect()
	assert.NoError(t, err)
	err = ui.RollBackChangesOnError()
	assert.NoError(t, err)
	err = ui.RunExitAction()
	assert.NoError(t, err)
	assert.Equal(t, 2, shardingCalledCount)
	assert.Contains(t, tw.Output(), expected)
}
