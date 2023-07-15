package systemuserchecktrigger

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/lib/logger"
)

type SystemUserCheck struct {
	log  logger.Logger
	port string
}

func NewSystemUserCheck(log logger.Logger, port string) *SystemUserCheck {
	return &SystemUserCheck{
		log:  log,
		port: port,
	}
}

func (suc *SystemUserCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	// Check for config.HardWare if empty of nil
	if config.Hardware == nil {
		return trigger.HardwareNil(constants.SYSTEM_USER, "Missing instance counts and instance IPs", true, true, true)
	}

	return trigger.RunCheck(config, suc.log, suc.port, constants.SYSTEM_USER_CHECK_API_PATH, "")
}

func (ss *SystemUserCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := make(map[string]map[string][]int)
	return nodeTypePortMap
}
