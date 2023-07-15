package systemresourcechecktrigger

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/lib/logger"
)

type SystemResourceCheck struct {
	log  logger.Logger
	port string
}

func NewSystemResourceCheck(log logger.Logger, port string) *SystemResourceCheck {
	return &SystemResourceCheck{
		log:  log,
		port: port,
	}
}

func (src *SystemResourceCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	// Check for config.HardWare if empty of nil
	if config.Hardware == nil {
		return trigger.HardwareNil(constants.SYSTEM_RESOURCES, "Missing instance counts and instance IPs", true, true, true)
	}

	return trigger.RunCheck(config, src.log, src.port, constants.SYSTEM_RESOURCE_CHECK_API_PATH, config.DeploymentState)
}

func (ss *SystemResourceCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := make(map[string]map[string][]int)
	return nodeTypePortMap
}
