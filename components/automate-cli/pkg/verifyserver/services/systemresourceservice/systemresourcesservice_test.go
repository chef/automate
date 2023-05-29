package systemresourceservice

import (
	"errors"
	"fmt"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/systemresource"
	"github.com/stretchr/testify/assert"
)

type testCase struct {
	testCaseDescription string
	nodeType            constants.NodeType
	respWant            *models.Checks
	mockSystemResource  systemresource.SystemResourceInfo
	mockFileUtils       fileutils.FileUtils
}

const (
	DISK_SPACE_CALC_ERR = "error occured while fetching total and/or free disk space"
)

func TestGetCpuCountCheck(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})

	testCases := []testCase{
		{
			testCaseDescription: "Cpu count is expected",
			respWant:            srv.GetChecksModel(true, CPU_COUNT_CHECK_TITLE, fmt.Sprintf("CPU count is >=%v", constants.MIN_CPU_COUNT), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetNumberOfCPUFunc: func() int {
					return constants.MIN_CPU_COUNT
				},
			},
		},
		{
			testCaseDescription: "Cpu count is not expected",
			respWant:            srv.GetChecksModel(false, CPU_COUNT_CHECK_TITLE, "", fmt.Sprintf("CPU count is %v", constants.MIN_CPU_COUNT-1), fmt.Sprintf("CPU count should be >=%v", constants.MIN_CPU_COUNT)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetNumberOfCPUFunc: func() int {
					return constants.MIN_CPU_COUNT - 1
				},
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.systemResourceInfo = testCase.mockSystemResource
			respGet := srv.CheckCpuCount()
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetCpuSpeedCheck(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})
	testCases := []testCase{
		{
			testCaseDescription: "CPU speed is expected",
			respWant:            srv.GetChecksModel(true, CPU_SPEED_CHECK_TITLE, fmt.Sprintf("CPU speed should be >=%vGHz", constants.MIN_CPU_SPEED), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetCPUSpeedFunc: func() (float64, error) {
					return constants.MIN_CPU_SPEED + 1, nil
				},
			},
		},
		{
			testCaseDescription: "CPU speed is not expected",
			respWant:            srv.GetChecksModel(false, CPU_SPEED_CHECK_TITLE, "", fmt.Sprintf("CPU speed is %vGHz", constants.MIN_CPU_SPEED-1), fmt.Sprintf("CPU speed should be >=%vGHz", constants.MIN_CPU_SPEED)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetCPUSpeedFunc: func() (float64, error) {
					return constants.MIN_CPU_SPEED - 1, nil
				},
			},
		},
		{
			testCaseDescription: "System is not running on supported platform",
			respWant:            srv.GetChecksModel(false, CPU_SPEED_CHECK_TITLE, "", errors.New("error occured while fetching cpu speed").Error(), RESOLUTION_MSG),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetCPUSpeedFunc: func() (float64, error) {
					return 0, errors.New("error occured while fetching cpu speed")
				},
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.systemResourceInfo = testCase.mockSystemResource
			respGet := srv.CheckCpuSpeed()
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetMemorySizeCheck(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})
	testCases := []testCase{
		{
			testCaseDescription: "Memory value is expected",
			respWant:            srv.GetChecksModel(true, MEMORY_SIZE_CHECK_TITLE, fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetMemoryFunc: func() (float64, error) {
					return float64(constants.MIN_MEMORY), nil
				},
			},
		},
		{
			testCaseDescription: "Memory value is not expected",
			respWant:            srv.GetChecksModel(false, MEMORY_SIZE_CHECK_TITLE, "", fmt.Sprintf("Memory is %0.2fGB", float64(constants.MIN_MEMORY-2)), fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetMemoryFunc: func() (float64, error) {
					return float64(constants.MIN_MEMORY - 2), nil
				},
			},
		},
		{
			testCaseDescription: "Error while reading memory size",
			respWant:            srv.GetChecksModel(false, MEMORY_SIZE_CHECK_TITLE, "", "error occured while reading memory size", RESOLUTION_MSG),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetMemoryFunc: func() (float64, error) {
					return float64(0), errors.New("error occured while reading memory size")
				},
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.systemResourceInfo = testCase.mockSystemResource
			respGet := srv.CheckMemorySize()
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetFreeDiskSpaceCheckOfDir(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})
	successMockCheckDirFunc := func(s string) (bool, error) {
		return true, nil
	}

	testCasesTmpSpaceCheck := []testCase{
		{
			testCaseDescription: "Checking free disk in /tmp",
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Temp"), fmt.Sprintf(SUCCESS_MSG, "/tmp", constants.TMP_FREE_DISK_IN_GB)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, 11, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in /tmp",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Temp"), "", fmt.Sprintf(ERROR_MSG, "/tmp", float64(9)), fmt.Sprintf(SUCCESS_MSG, "/tmp", constants.TMP_FREE_DISK_IN_GB)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, 9, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in /tmp",
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Temp"), fmt.Sprintf(SUCCESS_MSG, "/tmp", constants.TMP_FREE_DISK_IN_GB)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 300, 16, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in /tmp",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Temp"), "", fmt.Sprintf(ERROR_MSG, "/tmp", float64(14)), fmt.Sprintf(SUCCESS_MSG, "/tmp", constants.TMP_FREE_DISK_IN_GB)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 300, 14, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in /tmp | error",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Temp"), "", "error occured while fetching tmp disk space", RESOLUTION_MSG),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 0, 0, errors.New("error occured while fetching tmp disk space")
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
	}

	for _, testCase := range testCasesTmpSpaceCheck {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.systemResourceInfo = testCase.mockSystemResource
			srv.fileutils = testCase.mockFileUtils
			respGet := srv.CheckFreeDiskSpaceOfDir("/tmp", constants.TMP_FREE_DISK_IN_PER, constants.TMP_FREE_DISK_IN_GB, "Temp")
			assert.Equal(t, testCase.respWant, respGet)
		})
	}

	testCasesRootSpaceCheck := []testCase{
		{
			testCaseDescription: "Checking free disk in / (root)",
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "/(root volume)"), fmt.Sprintf(SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, 21, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in / (root)",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "/(root volume)"), "", fmt.Sprintf(ERROR_MSG, "/(root volume)", float64(19)), fmt.Sprintf(SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, 19, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in / (root)",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "/(root volume)"), "", fmt.Sprintf(ERROR_MSG, "/(root volume)", float64(25)), fmt.Sprintf(SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 200, 25, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in / (root)",
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "/(root volume)"), fmt.Sprintf(SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 200, 42, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
	}

	for _, testCase := range testCasesRootSpaceCheck {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.systemResourceInfo = testCase.mockSystemResource
			srv.fileutils = testCase.mockFileUtils
			respGet := srv.CheckFreeDiskSpaceOfDir("/", constants.ROOT_FREE_DISK_IN_PER, constants.ROOT_FREE_DISK_IN_GB, "/(root volume)")
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetHabFreeSpaceCheckPreDeployment(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})
	successMockCheckDirFunc := func(s string) (bool, error) {
		return true, nil
	}

	testCases := []testCase{
		{
			testCaseDescription: "Pre-deployment hab free space check | automate",
			nodeType:            constants.NodeTypeAutomate,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_A2), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_BEFORE_DEP_A2 + 1, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | chef-serve",
			nodeType:            constants.NodeTypeChefServer,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_CS), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_BEFORE_DEP_CS + 1, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | postgresql",
			nodeType:            constants.NodeTypePostgresql,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_PG), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_BEFORE_DEP_PG + 1, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | opensearch",
			nodeType:            constants.NodeTypeOpensearch,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_OS), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_BEFORE_DEP_OS + 1, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | bastion",
			nodeType:            constants.NodeTypeBastion,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_BASTION), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_BEFORE_DEP_BASTION + 1, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | automate",
			nodeType:            constants.NodeTypeAutomate,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", fmt.Sprintf(ERROR_MSG, "/hab", float64(constants.HAB_FREE_DISK_BEFORE_DEP_A2-1)), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_A2)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_BEFORE_DEP_A2 - 1, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | error case",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", DISK_SPACE_CALC_ERR, RESOLUTION_MSG),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return -1, -1, errors.New(DISK_SPACE_CALC_ERR)
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.systemResourceInfo = testCase.mockSystemResource
			srv.fileutils = testCase.mockFileUtils
			respGet := srv.CheckHabFreeSpacePreDeployment(testCase.nodeType)
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetHabFreeSpaceCheckPostDeployment(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})
	successMockCheckDirFunc := func(s string) (bool, error) {
		return true, nil
	}

	testCases := []testCase{
		{
			testCaseDescription: "Post-deployment hab free space check | automate",
			nodeType:            constants.NodeTypeAutomate,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_A2)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_A2_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_AFTER_DEP_A2 + 1, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | automate",
			nodeType:            constants.NodeTypeAutomate,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", fmt.Sprintf(ERROR_MSG, "/hab", float64(14)), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_A2)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_A2_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 300, 14, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | chef-server",
			nodeType:            constants.NodeTypeChefServer,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", fmt.Sprintf(ERROR_MSG, "/hab", float64(constants.HAB_FREE_DISK_AFTER_DEP_CS-1)), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_CS)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_CS_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_AFTER_DEP_CS - 1, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | opensearch",
			nodeType:            constants.NodeTypeOpensearch,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_OS)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_OS_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_AFTER_DEP_OS + 1, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | opensearch",
			nodeType:            constants.NodeTypeOpensearch,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", fmt.Sprintf(ERROR_MSG, "/hab", float64(100)), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_OS)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_OS_IN_PER*100)),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 1000, 100, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | postgresql",
			nodeType:            constants.NodeTypePostgresql,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_PG)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_PG_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 1000, 1000 * constants.HAB_FREE_DISK_AFTER_DEP_PG_IN_PER, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | bastion",
			nodeType:            constants.NodeTypeBastion,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_BASTION)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_BASTION_IN_PER*100), "", ""),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 300, 300 * constants.HAB_FREE_DISK_AFTER_DEP_BASTION_IN_PER, nil
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | error case",
			nodeType:            constants.NodeTypeAutomate,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", "error occured while fetching disk space info", RESOLUTION_MSG),
			mockSystemResource: &systemresource.MockSystemResourceInfoImpl{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 0, 0, errors.New("error occured while fetching disk space info")
				},
			},
			mockFileUtils: &fileutils.MockFileSystemUtils{
				PathExistsFunc: successMockCheckDirFunc,
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.systemResourceInfo = testCase.mockSystemResource
			srv.fileutils = testCase.mockFileUtils
			respGet := srv.CheckHabFreeSpacePostDeployment(testCase.nodeType)
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetSystemResourcesForDeployment(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &systemresource.MockSystemResourceInfoImpl{}, &fileutils.MockFileSystemUtils{})
	successMockCheckDirFunc := func(s string) (bool, error) {
		return true, nil
	}

	type testCase struct {
		testCaseDescription string
		respWant            *models.ApiResult
		mockSystemResource  *systemresource.MockSystemResourceInfoImpl
		mockFsUtils         fileutils.FileUtils
		nodeType            constants.NodeType
		deploymentState     constants.DeploymentState
	}

	validChecks := []models.Checks{
		*srv.GetChecksModel(true, CPU_COUNT_CHECK_TITLE, fmt.Sprintf("CPU count is >=%v", constants.MIN_CPU_COUNT), "", ""),
		*srv.GetChecksModel(true, CPU_SPEED_CHECK_TITLE, fmt.Sprintf("CPU speed should be >=%vGHz", constants.MIN_CPU_SPEED), "", ""),
		*srv.GetChecksModel(true, MEMORY_SIZE_CHECK_TITLE, fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY), "", ""),
		*srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_A2), "", ""),
		*srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Temp"), fmt.Sprintf(SUCCESS_MSG, "/tmp", constants.TMP_FREE_DISK_IN_GB)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100), "", ""),
		*srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "/(root volume)"), fmt.Sprintf(SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100), "", ""),
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
		GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
			return 100, constants.HAB_FREE_DISK_BEFORE_DEP_A2 + 1, nil
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
			nodeType:           constants.NodeTypeAutomate,
			deploymentState:    constants.DeploymentStatePreDeploy,
		},
		{
			testCaseDescription: "nodeType=automate,deploymentState=postDeploy",
			respWant: &models.ApiResult{
				Passed: true,
				Checks: validChecks,
			},
			mockSystemResource: mockSystemResourceInfo,
			mockFsUtils:        mockFsUtils,
			nodeType:           constants.NodeTypeAutomate,
			deploymentState:    constants.DeploymentStatePostDeploy,
		},
		{
			testCaseDescription: "nodeType=automate,deploymentState=preDeploy",
			respWant: &models.ApiResult{
				Passed: false,
				Checks: validChecks,
			},
			mockSystemResource: mockSystemResourceInfo,
			mockFsUtils:        mockFsUtils,
			nodeType:           constants.NodeTypeAutomate,
			deploymentState:    constants.DeploymentStatePostDeploy,
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {

			if testCase.deploymentState == constants.POST_DEPLOY {
				testCase.respWant.Checks[3] = *srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_A2)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_A2_IN_PER*100), "", "")
				testCase.mockSystemResource.GetDiskSpaceInfoFunc = func(s string) (float64, float64, error) {
					return 100, 30, nil
				}
			}

			if !testCase.respWant.Passed {
				testCase.respWant.Checks[0] = *srv.GetChecksModel(false, CPU_COUNT_CHECK_TITLE, "", fmt.Sprintf("CPU count is %v", constants.MIN_CPU_COUNT-1), fmt.Sprintf("CPU count should be >=%v", constants.MIN_CPU_COUNT))
				testCase.mockSystemResource.GetNumberOfCPUFunc = func() int {
					return constants.MIN_CPU_COUNT - 1
				}
			}

			srv.systemResourceInfo = testCase.mockSystemResource
			srv.fileutils = testCase.mockFsUtils
			respGet := srv.GetSystemResourcesForDeployment(testCase.nodeType, testCase.deploymentState)
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}
