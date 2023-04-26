package handlers

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/checks"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/handlers/status"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
)

type Groups struct {
	Status status.IStatus
	Checks checks.IChecks
}

func NewHandlersGroup(logger logger.ILogger) *Groups {
	return &Groups{
		Status: status.NewHandler(logger),
		Checks: checks.NewHandler(logger),
	}
}
