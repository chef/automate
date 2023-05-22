package systemresourceservice

import (
	"errors"
	"fmt"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

type testCase struct {
	testCaseDescription string
	nodeType            string
	respWant            *models.Checks
	mockOsFsUtil        IGetOsAndFileSystemInfo
}

func TestGetCpuCountCheck(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &MockGetOsAndFileSystemInfo{})

	testCases := []testCase{
		{
			testCaseDescription: "Cpu count is expected",
			respWant: &models.Checks{
				Title:         CPU_COUNT_CHECK_TITLE,
				Passed:        true,
				SuccessMsg:    fmt.Sprintf("CPU count is >=%v", constants.MIN_CPU_COUNT),
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetNumberOfCPUFunc: func() int {
					return constants.MIN_CPU_COUNT
				},
			},
		},
		{
			testCaseDescription: "Cpu count is not expected",
			respWant: &models.Checks{
				Title:         CPU_COUNT_CHECK_TITLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf("CPU count is %v", constants.MIN_CPU_COUNT-1),
				ResolutionMsg: fmt.Sprintf("CPU count should be >=%v", constants.MIN_CPU_COUNT),
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetNumberOfCPUFunc: func() int {
					return constants.MIN_CPU_COUNT - 1
				},
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.GetOsAndFileSystemInfo = testCase.mockOsFsUtil
			respGet := srv.GetCpuCountCheck()
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetCpuSpeedCheck(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &MockGetOsAndFileSystemInfo{})
	testCases := []testCase{
		{
			testCaseDescription: "CPU speed is expected",
			respWant: &models.Checks{
				Title:         CPU_SPEED_CHECK_TITLE,
				Passed:        true,
				SuccessMsg:    fmt.Sprintf("CPU speed should be >=%vGHz", constants.MIN_CPU_SPEED),
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetCPUSpeedFunc: func(s string) (float64, error) {
					return constants.MIN_CPU_SPEED + 1, nil
				},
			},
		},
		{
			testCaseDescription: "CPU speed is not expected",
			respWant: &models.Checks{
				Title:         CPU_SPEED_CHECK_TITLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf("CPU speed is %vGHz", constants.MIN_CPU_SPEED-1),
				ResolutionMsg: fmt.Sprintf("CPU speed should be >=%vGHz", constants.MIN_CPU_SPEED),
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetCPUSpeedFunc: func(s string) (float64, error) {
					return constants.MIN_CPU_SPEED - 1, nil
				},
			},
		},
		{
			testCaseDescription: "System is not running on supported platform",
			respWant: &models.Checks{
				Title:         CPU_SPEED_CHECK_TITLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      errors.New("open " + CPU_INFO_FILE + ": no such file or directory").Error(),
				ResolutionMsg: "Please run system on supported platform",
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetCPUSpeedFunc: func(s string) (float64, error) {
					return 0, errors.New("open " + s + ": no such file or directory")
				},
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.GetOsAndFileSystemInfo = testCase.mockOsFsUtil
			respGet := srv.GetCpuSpeedCheck(CPU_INFO_FILE)
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetMemorySizeCheck(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &MockGetOsAndFileSystemInfo{})
	testCases := []testCase{
		{
			testCaseDescription: "Memory value is expected",
			respWant: &models.Checks{
				Title:         MEMORY_SIZE_CHECK_TITLE,
				Passed:        true,
				SuccessMsg:    fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY),
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetMemoryFunc: func() (float64, error) {
					return float64(constants.MIN_MEMORY), nil
				},
			},
		},
		{
			testCaseDescription: "Memory value is not expected",
			respWant: &models.Checks{
				Title:         MEMORY_SIZE_CHECK_TITLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf("Memory is %0.2fGB", float64(constants.MIN_MEMORY-2)),
				ResolutionMsg: fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY),
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetMemoryFunc: func() (float64, error) {
					return float64(constants.MIN_MEMORY - 2), nil
				},
			},
		},
		{
			testCaseDescription: "Getting error while reading cpuInfo file",
			respWant: &models.Checks{
				Title:         MEMORY_SIZE_CHECK_TITLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      "Failed to read /proc/meminfo",
				ResolutionMsg: "Please run system on supported platform",
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetMemoryFunc: func() (float64, error) {
					return float64(0), errors.New("Failed to read /proc/meminfo")
				},
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.GetOsAndFileSystemInfo = testCase.mockOsFsUtil
			respGet := srv.GetMemorySizeCheck()
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetFreeDiskSpaceCheckOfDir(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &MockGetOsAndFileSystemInfo{})
	successMockCheckDirFunc := func(s string) (bool, error) {
		return true, nil
	}

	testCasesTmpSpaceCheck := []testCase{
		{
			testCaseDescription: "Checking free disk in /tmp",
			respWant: &models.Checks{
				Title:         fmt.Sprintf(FREE_SPACE_CHECK, "Temp"),
				Passed:        true,
				SuccessMsg:    fmt.Sprintf(SUCCESS_MSG, "/tmp", constants.TMP_FREE_DISK_IN_GB) + fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100),
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, 11, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in /tmp",
			respWant: &models.Checks{
				Title:         fmt.Sprintf(FREE_SPACE_CHECK, "Temp"),
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf(ERROR_MSG, "/tmp", float64(9)),
				ResolutionMsg: fmt.Sprintf(SUCCESS_MSG, "/tmp", constants.TMP_FREE_DISK_IN_GB) + fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100),
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, 9, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in /tmp",
			respWant: &models.Checks{
				Title:         fmt.Sprintf(FREE_SPACE_CHECK, "Temp"),
				Passed:        true,
				SuccessMsg:    fmt.Sprintf(SUCCESS_MSG, "/tmp", constants.TMP_FREE_DISK_IN_GB) + fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100),
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 300, 16, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in /tmp",
			respWant: &models.Checks{
				Title:         fmt.Sprintf(FREE_SPACE_CHECK, "Temp"),
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf(ERROR_MSG, "/tmp", float64(14)),
				ResolutionMsg: fmt.Sprintf(SUCCESS_MSG, "/tmp", constants.TMP_FREE_DISK_IN_GB) + fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.TMP_FREE_DISK_IN_PER*100),
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 300, 14, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
	}

	for _, testCase := range testCasesTmpSpaceCheck {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.GetOsAndFileSystemInfo = testCase.mockOsFsUtil
			respGet := srv.GetFreeDiskSpaceCheckOfDir("/tmp", constants.TMP_FREE_DISK_IN_PER, constants.TMP_FREE_DISK_IN_GB, "Temp")
			assert.Equal(t, testCase.respWant, respGet)
		})
	}

	testCasesRootSpaceCheck := []testCase{
		{
			testCaseDescription: "Checking free disk in / (root)",
			respWant: &models.Checks{
				Title:         fmt.Sprintf(FREE_SPACE_CHECK, "/(root volume)"),
				Passed:        true,
				SuccessMsg:    fmt.Sprintf(SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB) + fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100),
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, 21, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in / (root)",
			respWant: &models.Checks{
				Title:         fmt.Sprintf(FREE_SPACE_CHECK, "/(root volume)"),
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf(ERROR_MSG, "/(root volume)", float64(19)),
				ResolutionMsg: fmt.Sprintf(SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB) + fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100),
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, 19, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in / (root)",
			respWant: &models.Checks{
				Title:         fmt.Sprintf(FREE_SPACE_CHECK, "/(root volume)"),
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      fmt.Sprintf(ERROR_MSG, "/(root volume)", float64(25)),
				ResolutionMsg: fmt.Sprintf(SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB) + fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100),
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 200, 25, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Checking free disk in / (root)",
			respWant: &models.Checks{
				Title:         fmt.Sprintf(FREE_SPACE_CHECK, "/(root volume)"),
				Passed:        true,
				SuccessMsg:    fmt.Sprintf(SUCCESS_MSG, "/(root volume)", constants.ROOT_FREE_DISK_IN_GB) + fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.ROOT_FREE_DISK_IN_PER*100),
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 200, 42, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
	}

	for _, testCase := range testCasesRootSpaceCheck {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.GetOsAndFileSystemInfo = testCase.mockOsFsUtil
			respGet := srv.GetFreeDiskSpaceCheckOfDir("/", constants.ROOT_FREE_DISK_IN_PER, constants.ROOT_FREE_DISK_IN_GB, "/(root volume)")
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetHabFreeSpaceCheckPreDeployment(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &MockGetOsAndFileSystemInfo{})
	successMockCheckDirFunc := func(s string) (bool, error) {
		return true, nil
	}

	testCases := []testCase{
		{
			testCaseDescription: "Pre-deployment hab free space check | automate",
			nodeType:            constants.AUTOMATE,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_A2), "", ""),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_BEFORE_DEP_A2 + 1, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | chef-serve",
			nodeType:            constants.CHEF_INFRA_SERVER,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_CS), "", ""),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_BEFORE_DEP_CS + 1, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | postgresql",
			nodeType:            constants.POSTGRESQL,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_PG), "", ""),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_BEFORE_DEP_PG + 1, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | opensearch",
			nodeType:            constants.OPENSEARCH,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_OS), "", ""),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_BEFORE_DEP_OS + 1, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | bastion",
			nodeType:            constants.BASTION,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_BASTION), "", ""),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_BEFORE_DEP_BASTION + 1, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | automate",
			nodeType:            constants.AUTOMATE,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", fmt.Sprintf(ERROR_MSG, "/hab", float64(constants.HAB_FREE_DISK_BEFORE_DEP_A2-1)), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_BEFORE_DEP_A2)),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_BEFORE_DEP_A2 - 1, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | error case",
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", DISK_SPACE_CALC_ERR, "Please run system on supported platform"),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return -1, -1, errors.New(DISK_SPACE_CALC_ERR)
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Pre-deployment hab free space check | wrong query",
			nodeType:            "invalid",
			respWant:            nil,
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 0, 0, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.GetOsAndFileSystemInfo = testCase.mockOsFsUtil
			respGet, _ := srv.GetHabFreeSpaceCheckPreDeployment(testCase.nodeType)
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}

func TestGetHabFreeSpaceCheckPostDeployment(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	srv := NewSystemResourceService(log, &MockGetOsAndFileSystemInfo{})
	successMockCheckDirFunc := func(s string) (bool, error) {
		return true, nil
	}

	testCases := []testCase{
		{
			testCaseDescription: "Post-deployment hab free space check | automate",
			nodeType:            constants.AUTOMATE,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_A2)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_A2_IN_PER*100), "", ""),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_AFTER_DEP_A2 + 1, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | automate",
			nodeType:            constants.AUTOMATE,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", fmt.Sprintf(ERROR_MSG, "/hab", float64(14)), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_A2)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_A2_IN_PER*100)),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 300, 14, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | chef-server",
			nodeType:            constants.CHEF_INFRA_SERVER,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", fmt.Sprintf(ERROR_MSG, "/hab", float64(constants.HAB_FREE_DISK_AFTER_DEP_CS-1)), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_CS)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_CS_IN_PER*100)),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_AFTER_DEP_CS - 1, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | opensearch",
			nodeType:            constants.OPENSEARCH,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_OS)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_OS_IN_PER*100), "", ""),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 100, constants.HAB_FREE_DISK_AFTER_DEP_OS + 1, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | opensearch",
			nodeType:            constants.OPENSEARCH,
			respWant:            srv.GetChecksModel(false, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), "", fmt.Sprintf(ERROR_MSG, "/hab", float64(100)), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_OS)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_OS_IN_PER*100)),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 1000, 100, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | postgresql",
			nodeType:            constants.POSTGRESQL,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_PG)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_PG_IN_PER*100), "", ""),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 1000, 1000 * constants.HAB_FREE_DISK_AFTER_DEP_PG_IN_PER, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
		{
			testCaseDescription: "Post-deployment hab free space check | bastion",
			nodeType:            constants.BASTION,
			respWant:            srv.GetChecksModel(true, fmt.Sprintf(FREE_SPACE_CHECK, "Hab"), fmt.Sprintf(SUCCESS_MSG, "/hab", constants.HAB_FREE_DISK_AFTER_DEP_BASTION)+fmt.Sprintf(SUCCESS_MSG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_BASTION_IN_PER*100), "", ""),
			mockOsFsUtil: &MockGetOsAndFileSystemInfo{
				GetDiskSpaceInfoFunc: func(s string) (float64, float64, error) {
					return 300, 300 * constants.HAB_FREE_DISK_AFTER_DEP_BASTION_IN_PER, nil
				},
				GetCheckPathExistsFunc: successMockCheckDirFunc,
			},
		},
	}

	for _, testCase := range testCases {
		t.Run(testCase.testCaseDescription, func(t *testing.T) {
			srv.GetOsAndFileSystemInfo = testCase.mockOsFsUtil
			respGet, _ := srv.GetHabFreeSpaceCheckPostDeployment(testCase.nodeType)
			assert.Equal(t, testCase.respWant, respGet)
		})
	}
}
