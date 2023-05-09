package v1

import (
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/softwareversionservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/statusservice"
)

type Handler struct {
	Logger                 logger.Logger
	StatusService          statusservice.IStatusService
	BatchCheckService      batchcheckservice.IBatchCheckService
	SoftwareVersionService softwareversionservice.ISoftwareVersionService
}

func NewHandler(logger logger.Logger) *Handler {
	return &Handler{Logger: logger}
}

func (h *Handler) AddStatusService(ss statusservice.IStatusService) *Handler {
	h.StatusService = ss
	return h
}

func (h *Handler) AddBatchCheckService(bc batchcheckservice.IBatchCheckService) *Handler {
	h.BatchCheckService = bc
	return h
}

func (h *Handler) AddSoftwareVersionService(sv softwareversionservice.ISoftwareVersionService) *Handler {
	h.SoftwareVersionService = sv
	return h
}
