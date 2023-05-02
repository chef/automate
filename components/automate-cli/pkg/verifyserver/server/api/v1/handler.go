package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/statusservice"
)

type Handler struct {
	Logger        logger.ILogger
	StatusService statusservice.IStatusService
	BatchCheckService batchcheckservice.IBatchCheckService
}

func NewHandler(logger logger.ILogger) *Handler {
	return &Handler{Logger: logger}
}

func (h *Handler) AddStatusService(ss statusservice.IStatusService) *Handler {
	h.StatusService = ss
	return h
}

func (h *Handler) AddBatchCheckService(ss  batchcheckservice.IBatchCheckService) *Handler {
	h.BatchCheckService = ss
	return h
}
