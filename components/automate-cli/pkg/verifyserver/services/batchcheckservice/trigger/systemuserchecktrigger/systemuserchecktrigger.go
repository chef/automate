package systemuserchecktrigger

import (
	"net/http"

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

func (suc *SystemUserCheck) Run(config models.Config) []models.CheckTriggerResponse {
	return trigger.RunCheck(config, suc.log, suc.port, constants.SYSTEM_USER_CHECK_API_PATH, "", http.MethodGet, nil)
}
