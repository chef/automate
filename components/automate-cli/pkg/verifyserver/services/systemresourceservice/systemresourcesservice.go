package systemresourceservice

import (
	"fmt"
	"math"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/enums"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/systemresource"
	"github.com/shirou/gopsutil/disk"
)

type SystemResourcesService interface {
	GetSystemResourcesForDeployment(enums.NodeType, enums.DeploymentState) *models.ApiResult
}

type SystemResourcesServiceImpl struct {
	logger             logger.Logger
	SystemResourceInfo systemresource.SystemResourceInfo
	Fileutils          fileutils.FileUtils
}

func NewSystemResourceService(log logger.Logger, sysResInfo systemresource.SystemResourceInfo, fileUtils fileutils.FileUtils) *SystemResourcesServiceImpl {
	return &SystemResourcesServiceImpl{
		logger:             log,
		SystemResourceInfo: sysResInfo,
		Fileutils:          fileUtils,
	}
}

func (srs *SystemResourcesServiceImpl) GetSystemResourcesForDeployment(nodeType enums.NodeType, deploymentState enums.DeploymentState) *models.ApiResult {

	srsResponse := &models.ApiResult{
		Passed: true,
		Checks: []models.Checks{},
	}

	cpuCountCheck := srs.CheckCpuCount()
	srsResponse.Checks = append(srsResponse.Checks, *cpuCountCheck)

	cpuSpeedCheck := srs.CheckCpuSpeed()
	srsResponse.Checks = append(srsResponse.Checks, *cpuSpeedCheck)

	memorySizeCheck := srs.CheckMemorySize()
	srsResponse.Checks = append(srsResponse.Checks, *memorySizeCheck)

	habFreeSpaceCheck := srs.CheckHabFreeSpace(nodeType, deploymentState)
	srsResponse.Checks = append(srsResponse.Checks, *habFreeSpaceCheck)

	tmpFreeSpaceCheck := srs.CheckFreeDiskSpaceOfDir("/var/tmp", constants.TMP_FREE_DISK_IN_PER, constants.TMP_FREE_DISK_IN_GB, "Temp")
	srsResponse.Checks = append(srsResponse.Checks, *tmpFreeSpaceCheck)

	rootFreeSpaceCheck := srs.CheckFreeDiskSpaceOfDir("/", constants.ROOT_FREE_DISK_IN_PER, constants.ROOT_FREE_DISK_IN_GB, "/(root volume)")
	srsResponse.Checks = append(srsResponse.Checks, *rootFreeSpaceCheck)

	if !cpuCountCheck.Passed ||
		!cpuSpeedCheck.Passed ||
		!memorySizeCheck.Passed ||
		!habFreeSpaceCheck.Passed ||
		!tmpFreeSpaceCheck.Passed ||
		!rootFreeSpaceCheck.Passed {
		srsResponse.Passed = false
	}

	return srsResponse
}

func (srs *SystemResourcesServiceImpl) CheckCpuCount() *models.Checks {
	srs.logger.Debug("CPU count check is running")
	cpuCount := srs.SystemResourceInfo.GetNumberOfCPU()
	srs.logger.Debug("CPU count is : ", cpuCount)

	if cpuCount >= constants.MIN_CPU_COUNT {
		successMsg := fmt.Sprintf("CPU count is >=%v", constants.MIN_CPU_COUNT)
		return srs.GetChecksModel(true, constants.CPU_COUNT_CHECK_TITLE, successMsg, "", "")
	}

	errorMsg := fmt.Sprintf("CPU count is %v", cpuCount)
	resolutionMsg := fmt.Sprintf("CPU count should be >=%v", constants.MIN_CPU_COUNT)
	return srs.GetChecksModel(false, constants.CPU_COUNT_CHECK_TITLE, "", errorMsg, resolutionMsg)
}

func (srs *SystemResourcesServiceImpl) CheckCpuSpeed() *models.Checks {
	srs.logger.Debug("CPU speed check is running")
	cpuSpeed, err := srs.SystemResourceInfo.GetCPUSpeed()

	if err != nil {
		srs.logger.Error("Error occured while getting cpu speed :", err)
		return srs.GetChecksModel(false, constants.CPU_SPEED_CHECK_TITLE, "", err.Error(), constants.RESOLUTION_MSG)
	}
	srs.logger.Debug("CPU speed is : ", cpuSpeed)
	if cpuSpeed >= constants.MIN_CPU_SPEED {
		successMsg := fmt.Sprintf("CPU speed should be >=%vGHz", constants.MIN_CPU_SPEED)
		return srs.GetChecksModel(true, constants.CPU_SPEED_CHECK_TITLE, successMsg, "", "")
	}
	errorMsg := fmt.Sprintf("CPU speed is %vGHz", cpuSpeed)
	resolutionMsg := fmt.Sprintf("CPU speed should be >=%vGHz", constants.MIN_CPU_SPEED)
	return srs.GetChecksModel(false, constants.CPU_SPEED_CHECK_TITLE, "", errorMsg, resolutionMsg)
}

func (srs *SystemResourcesServiceImpl) CheckMemorySize() *models.Checks {
	srs.logger.Debug("Memory size check is running")
	memoryInGB, err := srs.SystemResourceInfo.GetMemory()
	srs.logger.Debug("Current memory of system is :", memoryInGB)

	if err != nil {
		srs.logger.Error("Error occured while getting memory information : ", err.Error())
		return srs.GetChecksModel(false, constants.MEMORY_SIZE_CHECK_TITLE, "", err.Error(), constants.RESOLUTION_MSG)
	}
	srs.logger.Debugf("Current memory of system is :%vGB", memoryInGB)

	if memoryInGB >= constants.MIN_MEMORY {
		successMsg := fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY)
		return srs.GetChecksModel(true, constants.MEMORY_SIZE_CHECK_TITLE, successMsg, "", "")
	}
	errorMsg := fmt.Sprintf("Memory is %.2fGB", memoryInGB)
	resolutionMsg := fmt.Sprintf("Memory should be >=%vGB", constants.MIN_MEMORY)
	return srs.GetChecksModel(false, constants.MEMORY_SIZE_CHECK_TITLE, "", errorMsg, resolutionMsg)
}

func (srs *SystemResourcesServiceImpl) CheckHabFreeSpace(nodeType enums.NodeType, deploymentState enums.DeploymentState) *models.Checks {

	switch deploymentState {
	case enums.DeploymentStatePreDeploy:
		return srs.CheckHabFreeSpacePreDeployment(nodeType)
	case enums.DeploymentStatePostDeploy:
		return srs.CheckHabFreeSpacePostDeployment(nodeType)
	}
	return &models.Checks{}
}

func (srs *SystemResourcesServiceImpl) CheckHabFreeSpacePreDeployment(nodeType enums.NodeType) *models.Checks {
	srs.logger.Debug("Hab free space check is running for node_type : ", nodeType, " and deployment_state :", enums.DeploymentStatePreDeploy)

	currentFreeSpaceInGB, err := srs.GetFreeDiskSpaceOfGivenDir("/hab")

	var resp *models.Checks
	if err != nil {
		srs.logger.Error("Unable to determine free space of /hab :", err)
		resp = srs.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), "", err.Error(), constants.RESOLUTION_MSG)
		return resp
	}
	srs.logger.Debugf("The current total free space in hab : %0.2fGB", currentFreeSpaceInGB)

	switch nodeType {
	case enums.NodeTypeAutomate:
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(constants.HAB_FREE_DISK_BEFORE_DEP_A2, currentFreeSpaceInGB, 0, constants.HAB_FREE_DISK_BEFORE_DEP_A2, constants.PRE_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case enums.NodeTypeChefServer:
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(constants.HAB_FREE_DISK_BEFORE_DEP_CS, currentFreeSpaceInGB, 0, constants.HAB_FREE_DISK_BEFORE_DEP_CS, constants.PRE_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case enums.NodeTypeOpensearch:
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(constants.HAB_FREE_DISK_BEFORE_DEP_OS, currentFreeSpaceInGB, 0, constants.HAB_FREE_DISK_BEFORE_DEP_OS, constants.PRE_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case enums.NodeTypePostgresql:
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(constants.HAB_FREE_DISK_BEFORE_DEP_PG, currentFreeSpaceInGB, 0, constants.HAB_FREE_DISK_BEFORE_DEP_PG, constants.PRE_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case enums.NodeTypeBastion:
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(constants.HAB_FREE_DISK_BEFORE_DEP_BASTION, currentFreeSpaceInGB, 0, constants.HAB_FREE_DISK_BEFORE_DEP_BASTION, constants.PRE_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	}
	return resp
}

func (srs *SystemResourcesServiceImpl) CheckHabFreeSpacePostDeployment(nodeType enums.NodeType) *models.Checks {
	srs.logger.Debugf("Hab free space check is running for node_type : %s  and deployment_state : %s", nodeType, enums.DeploymentStatePostDeploy)
	currentFreeSpaceInGB, err := srs.GetFreeDiskSpaceOfGivenDir("/hab")
	var resp *models.Checks
	if err != nil {
		srs.logger.Error("Unable to determine free space of /hab :", err.Error())
		resp = srs.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), "", err.Error(), constants.RESOLUTION_MSG)
		return resp
	}
	srs.logger.Debugf("current free space in /hab after deployment :%0.2fGB", currentFreeSpaceInGB)

	totalSpaceInGBInHab, err := srs.GetTotalSpaceOfGivenDir("/hab")
	if err != nil {
		srs.logger.Error("Unable to retrive total space of /hab :", err.Error())
		resp = srs.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, "/hab"), "", err.Error(), constants.RESOLUTION_MSG)
		return resp
	}
	srs.logger.Debugf("current total space in /hab : %0.2f", totalSpaceInGBInHab)

	switch nodeType {
	case enums.NodeTypeAutomate:
		expectedFreeSpace := srs.GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInGBInHab, constants.HAB_FREE_DISK_AFTER_DEP_A2_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_A2)
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpace, currentFreeSpaceInGB, constants.HAB_FREE_DISK_AFTER_DEP_A2_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_A2, constants.POST_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case enums.NodeTypeChefServer:
		expectedFreeSpace := srs.GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInGBInHab, constants.HAB_FREE_DISK_AFTER_DEP_CS_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_CS)
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpace, currentFreeSpaceInGB, constants.HAB_FREE_DISK_AFTER_DEP_CS_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_CS, constants.POST_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case enums.NodeTypeOpensearch:
		expectedFreeSpace := srs.GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInGBInHab, constants.HAB_FREE_DISK_AFTER_DEP_OS_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_OS)
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpace, currentFreeSpaceInGB, constants.HAB_FREE_DISK_AFTER_DEP_OS_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_OS, constants.POST_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case enums.NodeTypePostgresql:
		expectedFreeSpace := srs.GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInGBInHab, constants.HAB_FREE_DISK_AFTER_DEP_PG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_PG)
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpace, currentFreeSpaceInGB, constants.HAB_FREE_DISK_AFTER_DEP_PG_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_PG, constants.POST_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	case enums.NodeTypeBastion:
		expectedFreeSpace := srs.GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInGBInHab, constants.HAB_FREE_DISK_AFTER_DEP_BASTION_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_BASTION)
		passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpace, currentFreeSpaceInGB, constants.HAB_FREE_DISK_AFTER_DEP_BASTION_IN_PER, constants.HAB_FREE_DISK_AFTER_DEP_BASTION, constants.POST_DEPLOY, "/hab")
		resp = srs.GetChecksModel(passed, fmt.Sprintf(constants.FREE_SPACE_CHECK, "Hab"), successMsg, errorMsg, resolutionMsg)
	}
	return resp
}

func (srs *SystemResourcesServiceImpl) CheckFreeDiskSpaceOfDir(dirPath string, freeDiskSpaceWantInPer, freeDiskSpaceWantInGB float64, checkTitle string) *models.Checks {
	srs.logger.Debugf("free disk space check is running for %s directory", dirPath)

	currentFreeSpaceInDir, err := srs.GetFreeDiskSpaceOfGivenDir(dirPath)
	if err != nil {
		srs.logger.Errorf("Unable to determine free space of %s error=%v", dirPath, err)
		return srs.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, checkTitle), "", err.Error(), constants.RESOLUTION_MSG)
	}
	srs.logger.Debugf("Current free disk space in %s is %0.2fGB", dirPath, currentFreeSpaceInDir)
	var totalSpaceInHab float64
	totalSpaceInHab, err = srs.GetTotalSpaceOfGivenDir("/hab")

	if err != nil {
		srs.logger.Error("Unable to determine total space of /hab :", err)
		return srs.GetChecksModel(false, fmt.Sprintf(constants.FREE_SPACE_CHECK, checkTitle), "", err.Error(), constants.RESOLUTION_MSG)
	}
	srs.logger.Debugf("Current total space in /hab : %0.2fGB", totalSpaceInHab)

	expectedFreeSpaceValue := srs.GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInHab, freeDiskSpaceWantInPer, freeDiskSpaceWantInGB)
	srs.logger.Debugf("Expected free space calculated for %s is %0.2fGB", dirPath, expectedFreeSpaceValue)

	passed, successMsg, errorMsg, resolutionMsg := srs.GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpaceValue, currentFreeSpaceInDir, freeDiskSpaceWantInPer, freeDiskSpaceWantInGB, constants.POST_DEPLOY, dirPath)
	return srs.GetChecksModel(passed, fmt.Sprintf(constants.FREE_SPACE_CHECK, checkTitle), successMsg, errorMsg, resolutionMsg)
}

func (srs *SystemResourcesServiceImpl) GetCheckModelValuesDetailsForStorageChecks(expectedFreeSpaceCalculated, currentFreeSpace, expectedFreeSpaceWantInPer, expectedFreeSpaceWantInGB float64, deploymentState, dirName string) (bool, string, string, string) {

	if dirName == "/" {
		dirName = "/(root volume)"
	}

	successMsg := ""
	resolutionMsg := ""
	errorMsg := ""
	passed := false
	if currentFreeSpace >= expectedFreeSpaceCalculated {
		passed = true
		successMsg = fmt.Sprintf(constants.SUCCESS_MSG, dirName, expectedFreeSpaceWantInGB)
		errorMsg = ""
		resolutionMsg = ""
	} else {
		successMsg = ""
		errorMsg = fmt.Sprintf(constants.ERROR_MSG, dirName, currentFreeSpace)
		resolutionMsg = fmt.Sprintf(constants.SUCCESS_MSG, dirName, expectedFreeSpaceWantInGB)
	}

	if deploymentState == constants.POST_DEPLOY {
		if passed {
			successMsg = successMsg + fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, expectedFreeSpaceWantInPer*100)
			errorMsg = ""
			resolutionMsg = ""
		} else {
			successMsg = ""
			resolutionMsg = resolutionMsg + fmt.Sprintf(constants.SUCCESS_MSG_IN_PER, expectedFreeSpaceWantInPer*100)
		}
	}
	return passed, successMsg, errorMsg, resolutionMsg
}

func (srs *SystemResourcesServiceImpl) GetExpectedFreeSpaceValueAfterDeploy(totalSpaceInGB, freeSpaceWantInPercentage, freeSpaceWantInGB float64) float64 {
	percentageValueOfTotalSpace := totalSpaceInGB * freeSpaceWantInPercentage
	return math.Max(percentageValueOfTotalSpace, float64(freeSpaceWantInGB))
}

func (srs *SystemResourcesServiceImpl) GetChecksModel(passed bool, checkTitle, successMsg, errorMsg, resolutionMsg string) *models.Checks {
	return &models.Checks{
		Title:         checkTitle,
		Passed:        passed,
		SuccessMsg:    successMsg,
		ErrorMsg:      errorMsg,
		ResolutionMsg: resolutionMsg,
	}
}

func (srs *SystemResourcesServiceImpl) GetFreeDiskSpaceOfGivenDir(dirPath string) (float64, error) {
	usage, err := srs.GetUsage(dirPath)

	if err != nil {
		return 0, err
	}
	return float64(usage.Free) / (1024 * 1024 * 1024), nil
}

func (srs *SystemResourcesServiceImpl) GetTotalSpaceOfGivenDir(dirPath string) (float64, error) {
	usage, err := srs.GetUsage(dirPath)
	if err != nil {
		return 0, err
	}
	return float64(usage.Total) / (1024 * 1024 * 1024), nil
}

func (srs *SystemResourcesServiceImpl) GetUsage(dirPath string) (disk.UsageStat, error) {
	isPathExist, err := srs.Fileutils.PathExists(dirPath)

	if err != nil {
		return disk.UsageStat{}, err
	}

	if !isPathExist {
		dirPath = "/"
	}

	usage, err := srs.SystemResourceInfo.GetDiskSpaceInfo(dirPath)
	if err != nil {
		return disk.UsageStat{}, err
	}
	return usage, nil
}
