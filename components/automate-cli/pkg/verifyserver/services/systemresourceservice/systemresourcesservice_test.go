package systemresourceservice_test

import (
	"errors"
	"fmt"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/enums"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/systemresourceservice"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/systemresource"
	"github.com/shirou/gopsutil/disk"
	"github.com/stretchr/testify/assert"
)

type testCase struct {
	testCaseDescription string
	nodeType            enums.NodeType
	respWant            *models.Checks
	mockSystemResource  systemresource.SystemResourceInfo
	mockFileUtils       fileutils.FileUtils
}

const (
	DISK_SPACE_CALC_ERR = "error occured while fetching total and/or free disk space"
)

func TestGetCpuCountCheck(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := systemresourceservice.NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})

	testCases := []testCase{
		{
			testCaseDescription: "Cpu count is expected",
			respWant:            srv.GetChecksModel(true, constants.CPU_COUNT_CHECK_TITLE, fmt.Sprintf("CPU count is >=%v", constants.MIN_CPU_COUNT), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetNumberOfCPUFunc: func() int {
					return constants.MIN_CPU_COUNT
				},
			},
		},
		{
			testCaseDescription: "Cpu count is not expected",
			respWant:            srv.GetChecksModel(false, constants.CPU_COUNT_CHECK_TITLE, "", fmt.Sprintf("CPU count is %v", constants.MIN_CPU_COUNT-1), fmt.Sprintf("CPU count should be >=%v", constants.MIN_CPU_COUNT)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetNumberOfCPUFunc: func() int {
					return constants.MIN_CPU_COUNT - 1
				},
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.SystemResourceInfo = testCase.mockSystemResource
			respGet := srv.CheckCpuCount()
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetCpuSpeedCheck(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := systemresourceservice.NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})
	testCases := []testCase{
		{
			testCaseDescription: "CPU speed is expected",
			respWant:            srv.GetChecksModel(true, constants.CPU_SPEED_CHECK_TITLE, fmt.Sprintf("CPU speed should be >=%vGHz", constants.MIN_CPU_SPEED), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetCPUSpeedFunc: func() (float64, error) {
					return constants.MIN_CPU_SPEED + 1, nil
				},
			},
		},
		{
			testCaseDescription: "CPU speed is not expected",
			respWant:            srv.GetChecksModel(false, constants.CPU_SPEED_CHECK_TITLE, "", fmt.Sprintf("CPU speed is %vGHz", constants.MIN_CPU_SPEED-1), fmt.Sprintf("CPU speed should be >=%vGHz", constants.MIN_CPU_SPEED)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetCPUSpeedFunc: func() (float64, error) {
					return constants.MIN_CPU_SPEED - 1, nil
				},
			},
		},
		{
			testCaseDescription: "System is not running on supported platform",
			respWant:            srv.GetChecksModel(false, constants.CPU_SPEED_CHECK_TITLE, "", errors.New("error occured while fetching cpu speed").Error(), constants.RESOLUTION_MSG),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetCPUSpeedFunc: func() (float64, error) {
					return 0, errors.New("error occured while fetching cpu speed")
				},
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.SystemResourceInfo = testCase.mockSystemResource
			respGet := srv.CheckCpuSpeed()
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetMemorySizeCheck(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := systemresourceservice.NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})
	testCases := []testCase{
		{
			testCaseDescription: "Memory value is expected",
			respWant:            srv.GetChecksModel(true, constants.MEMORY_SIZE_CHECK_TITLE, fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetMemoryFunc: func() (float64, error) {
					return float64(constants.MIN_MEMORY), nil
				},
			},
		},
		{
			testCaseDescription: "Memory value is not expected",
			respWant:            srv.GetChecksModel(false, constants.MEMORY_SIZE_CHECK_TITLE, "", fmt.Sprintf("Memory is %0.2fGB", float64(constants.MIN_MEMORY-2)), fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetMemoryFunc: func() (float64, error) {
					return float64(constants.MIN_MEMORY - 2), nil
				},
			},
		},
		{
			testCaseDescription: "Error while reading memory size",
			respWant:            srv.GetChecksModel(false, constants.MEMORY_SIZE_CHECK_TITLE, "", "error occured while reading memory size", constants.RESOLUTION_MSG),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetMemoryFunc: func() (float64, error) {
					return float64(0), errors.New("error occured while reading memory size")
				},
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.SystemResourceInfo = testCase.mockSystemResource
			respGet := srv.CheckMemorySize()
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetFreeDiskSpaceCheckOfDir(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := systemresourceservice.NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})
	successMockCheckDirFunc := func(s string) (bool, error) {
		return true, nil
	}

	testCasesTmpSpaceCheck := []testCase{
		{
			testCaseDescription: "Checking free disk in /var/tmp",
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Temp"), fmt.Sprintf(constants.SUCCESS_MSG, "/var/tmp", constants.TMP_FREE_DISK_IN_GB)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64(11 * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in /var/tmp",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Temp"), "", fmt.Sprintf(constants.ERROR_MSG, "/var/tmp", float64(9)), fmt.Sprintf(constants.SUCCESS_MSG, "/var/tmp", constants.TMP_FREE_DISK_IN_GB)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64(9 * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in /var/tmp",
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Temp"), fmt.Sprintf(constants.SUCCESS_MSG, "/var/tmp", constants.TMP_FREE_DISK_IN_GB)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(300 * constants.GB_TO_BYTES),
						Free:  uint64(16 * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in /var/tmp",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Temp"), "", fmt.Sprintf(constants.ERROR_MSG, "/var/tmp", float64(14)), fmt.Sprintf(constants.SUCCESS_MSG, "/var/tmp", constants.TMP_FREE_DISK_IN_GB)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(300 * constants.GB_TO_BYTES),
						Free:  uint64(14 * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in /var/tmp | error",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Temp"), "", "error occured while fetching tmp disk space", constants.RESOLUTION_MSG),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{}, errors.New("error occured while fetching tmp disk space")
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
	}

	for _, testCase := range testCasesTmpSpaceCheck {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.SystemResourceInfo = testCase.mockSystemResource
			srv.Fileutils = testCase.mockFileUtils
			respGet := srv.CheckFreeDiskSpaceOfDir("/var/tmp", constants.TMP_FREE_DISK_IN_PER, constants.TMP_FREE_DISK_IN_GB, "Temp")
			assert.Equal(t, testCase.respWant, respGet)
		})
	}

	testCasesRootSpaceCheck := []testCase{
		{
			testCaseDescription: "Checking free disk in / (root)",
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "/(root volume)"), fmt.Sprintf(constants.SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64(21 * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in / (root)",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "/(root volume)"), "", fmt.Sprintf(constants.ERROR_MSG, "/(root volume)", float64(19)), fmt.Sprintf(constants.SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64(19 * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in / (root)",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "/(root volume)"), "", fmt.Sprintf(constants.ERROR_MSG, "/(root volume)", float64(25)), fmt.Sprintf(constants.SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(200 * constants.GB_TO_BYTES),
						Free:  uint64(25 * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in / (root)",
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "/(root volume)"), fmt.Sprintf(constants.SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(200 * constants.GB_TO_BYTES),
						Free:  uint64(42 * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
	}

	for _, testCase := range testCasesRootSpaceCheck {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.SystemResourceInfo = testCase.mockSystemResource
			srv.Fileutils = testCase.mockFileUtils
			respGet := srv.CheckFreeDiskSpaceOfDir("/", constants.ROOT_FREE_DISK_IN_PER, constants.ROOT_FREE_DISK_IN_GB, "/(root volume)")
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetHabFreeSpaceCheckPreDeployment(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := systemresourceservice.NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})
	successMockCheckDirFunc := func(s string) (bool, error) {
		return true, nil
	}

	testCases := []testCase{
		{
			testCaseDescription: "Pre-deployment hab free space check | automate",
			nodeType:            enums.NodeTypeAutomate,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_A2), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64((constants.HAB_FREE_DISK_BEFORE_DEP_A2 + 1) * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | chef-serve",
			nodeType:            enums.NodeTypeChefServer,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_CS), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64((constants.HAB_FREE_DISK_BEFORE_DEP_CS + 1) * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | postgresql",
			nodeType:            enums.NodeTypePostgresql,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_PG), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64((constants.HAB_FREE_DISK_BEFORE_DEP_PG + 1) * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | opensearch",
			nodeType:            enums.NodeTypeOpensearch,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_OS), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64((constants.HAB_FREE_DISK_BEFORE_DEP_OS + 1) * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | bastion",
			nodeType:            enums.NodeTypeBastion,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_BASTION), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64((constants.HAB_FREE_DISK_BEFORE_DEP_BASTION + 1) * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | automate",
			nodeType:            enums.NodeTypeAutomate,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), "", fmt.Sprintf(constants.ERROR_MSG, "/hab", float64(constants.HAB_FREE_DISK_BEFORE_DEP_A2-1)), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_A2)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64((constants.HAB_FREE_DISK_BEFORE_DEP_A2 - 1) * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | error case",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), "", DISK_SPACE_CALC_ERR, constants.RESOLUTION_MSG),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{}, errors.New(DISK_SPACE_CALC_ERR)
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.SystemResourceInfo = testCase.mockSystemResource
			srv.Fileutils = testCase.mockFileUtils
			respGet := srv.CheckHabFreeSpacePreDeployment(testCase.nodeType)
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetHabFreeSpaceCheckPostDeployment(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := systemresourceservice.NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})
	successMockCheckDirFunc := func(s string) (bool, error) {
		return true, nil
	}

	testCases := []testCase{
		{
			testCaseDescription: "Post-deployment hab free space check | automate",
			nodeType:            enums.NodeTypeAutomate,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_A2)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_A2_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64((constants.HAB_FREE_DISK_AFTER_DEP_A2 + 1) * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | automate",
			nodeType:            enums.NodeTypeAutomate,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), "", fmt.Sprintf(constants.ERROR_MSG, "/hab", float64(14)), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_A2)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_A2_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(300 * constants.GB_TO_BYTES),
						Free:  uint64(14 * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | chef-server",
			nodeType:            enums.NodeTypeChefServer,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), "", fmt.Sprintf(constants.ERROR_MSG, "/hab", float64(constants.HAB_FREE_DISK_AFTER_DEP_CS-1)), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_CS)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_CS_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64((constants.HAB_FREE_DISK_AFTER_DEP_CS - 1) * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | opensearch",
			nodeType:            enums.NodeTypeOpensearch,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_OS)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_OS_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64((constants.HAB_FREE_DISK_AFTER_DEP_OS + 1) * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | opensearch",
			nodeType:            enums.NodeTypeOpensearch,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), "", fmt.Sprintf(constants.ERROR_MSG, "/hab", float64(100)), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_OS)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_OS_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(1000 * constants.GB_TO_BYTES),
						Free:  uint64(100 * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | postgresql",
			nodeType:            enums.NodeTypePostgresql,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_PG)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_PG_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(1000 * constants.GB_TO_BYTES),
						Free:  uint64((1000 * constants.HAB_FREE_DISK_AFTER_DEP_PG_IN_PER) * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | bastion",
			nodeType:            enums.NodeTypeBastion,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_BASTION)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_BASTION_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(300 * constants.GB_TO_BYTES),
						Free:  uint64((300 * constants.HAB_FREE_DISK_AFTER_DEP_BASTION_IN_PER) * constants.GB_TO_BYTES),
					}, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | error case",
			nodeType:            enums.NodeTypeAutomate,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), "", "error occured while fetching disk space info", constants.RESOLUTION_MSG),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{}, errors.New("error occured while fetching disk space info")
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.SystemResourceInfo = testCase.mockSystemResource
			srv.Fileutils = testCase.mockFileUtils
			respGet := srv.CheckHabFreeSpacePostDeployment(testCase.nodeType)
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetSystemResourcesForDeployment(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := systemresourceservice.NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})
	successMockCheckDirFunc := func(s string) (bool, error) {
		return true, nil
	}

	type testCase struct {
		testCaseDescription string
		respWant            *models.ApiResult
		mockSystemResource  *systemresource.MockSystemResourceInfoImpl
		mockFsUtils         fileutils.FileUtils
		nodeType            enums.NodeType
		deploymentState     enums.DeploymentState
	}

	validChecks := []models.Checks{
		*srv.GetChecksModel(true, constants.CPU_COUNT_CHECK_TITLE, fmt.Sprintf("CPU count is >=%v", constants.MIN_CPU_COUNT), "", ""),
		*srv.GetChecksModel(true, constants.CPU_SPEED_CHECK_TITLE, fmt.Sprintf("CPU speed should be >=%vGHz", constants.MIN_CPU_SPEED), "", ""),
		*srv.GetChecksModel(true, constants.MEMORY_SIZE_CHECK_TITLE, fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY), "", ""),
		*srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_A2), "", ""),
		*srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Temp"), fmt.Sprintf(constants.SUCCESS_MSG, "/var/tmp", constants.TMP_FREE_DISK_IN_GB)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100), "", ""),
		*srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "/(root volume)"), fmt.Sprintf(constants.SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100), "", ""),
	}

	mockSystemResourceInfo := &systemresource.MockSystemResourceInfoImpl{
		GetNumberOfCPUFunc: func() int {
			return constants.MIN_CPU_COUNT
		},
		GetCPUSpeedFunc: func() (float64, error) {
			return constants.MIN_CPU_SPEED + 1, nil
		},
		GetMemoryFunc: func() (float64, error) {
			return float64(constants.MIN_MEMORY), nil
		},
		GetDiskSpaceInfoFunc: func(s string) (disk.UsageStat, error) {
			return disk.UsageStat{
				Total: uint64(100 * constants.GB_TO_BYTES),
				Free:  uint64((constants.HAB_FREE_DISK_BEFORE_DEP_A2 + 1) * constants.GB_TO_BYTES),
			}, nil
		},
	}

	mockFsUtils := &fileutils.MockFileSystemUtils{
		PathExistsFunc: successMockCheckDirFunc,
	}

	testCases := []testCase{
		{
			testCaseDescription: "nodeType=automate,deploymentState=pre-deploy",
			respWant: &models.ApiResult{
				Passed: true,
				Checks: validChecks,
			},
			mockSystemResource: mockSystemResourceInfo,
			mockFsUtils:        mockFsUtils,
			nodeType:           enums.NodeTypeAutomate,
			deploymentState:    enums.DeploymentStatePreDeploy,
		},
		{
			testCaseDescription: "nodeType=automate,deploymentState=postDeploy",
			respWant: &models.ApiResult{
				Passed: true,
				Checks: validChecks,
			},
			mockSystemResource: mockSystemResourceInfo,
			mockFsUtils:        mockFsUtils,
			nodeType:           enums.NodeTypeAutomate,
			deploymentState:    enums.DeploymentStatePostDeploy,
		},
		{
			testCaseDescription: "nodeType=automate,deploymentState=preDeploy",
			respWant: &models.ApiResult{
				Passed: false,
				Checks: validChecks,
			},
			mockSystemResource: mockSystemResourceInfo,
			mockFsUtils:        mockFsUtils,
			nodeType:           enums.NodeTypeAutomate,
			deploymentState:    enums.DeploymentStatePostDeploy,
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {

			if testCase.deploymentState == constants.POST_DEPLOY {
				testCase.respWant.Checks[3] = *srv.GetChecksModel(true, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(constants.SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_A2)+fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_A2_IN_PER*100), "", "")
				testCase.mockSystemResource.GetDiskSpaceInfoFunc = func(s string) (disk.UsageStat, error) {
					return disk.UsageStat{
						Total: uint64(100 * constants.GB_TO_BYTES),
						Free:  uint64(30 * constants.GB_TO_BYTES),
					}, nil
				}
			}

			if !testCase.respWant.Passed {
				testCase.respWant.Checks[0] = *srv.GetChecksModel(false, constants.CPU_COUNT_CHECK_TITLE, "", fmt.Sprintf("CPU count is %v", constants.MIN_CPU_COUNT-1), fmt.Sprintf("CPU count should be >=%v", constants.MIN_CPU_COUNT))
				testCase.mockSystemResource.GetNumberOfCPUFunc = func() int {
					return constants.MIN_CPU_COUNT - 1
				}
			}

			srv.SystemResourceInfo = testCase.mockSystemResource
			srv.Fileutils = testCase.mockFsUtils
			respGet := srv.GetSystemResourcesForDeployment(testCase.nodeType, testCase.deploymentState)
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}
