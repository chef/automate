package softwareversionchecktrigger

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/lib/logger"
)

type SoftwareVersionCheck struct {
	log  logger.Logger
	port string
}

func NewSoftwareVersionCheck(log logger.Logger, port string) *SoftwareVersionCheck {
	return &SoftwareVersionCheck{
		log:  log,
		port: port,
	}
}

func (svc *SoftwareVersionCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	// Check for config.HardWare if empty of nil
	if config.Hardware == nil {
		return trigger.HardwareNil(constants.SOFTWARE_VERSIONS, constants.SKIP_MISSING_HARDWARE_MESSAGE, true, true, true)
	}

	return trigger.RunCheck(config, svc.log, svc.port, constants.SOFTWARE_VERSION_CHECK_API_PATH, "")
}

func (ss *SoftwareVersionCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := make(map[string]map[string][]int)
	return nodeTypePortMap
}
