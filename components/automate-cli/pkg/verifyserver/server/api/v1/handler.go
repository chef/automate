package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/nfsmountservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/statusservice"
	"github.com/chef/automate/lib/logger"
)

type Handler struct {
	Logger            logger.Logger
	StatusService     statusservice.IStatusService
	BatchCheckService batchcheckservice.IBatchCheckService
	NFSMountService   nfsmountservice.INFSService
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

func (h *Handler) AddNFSMountService(nm nfsmountservice.INFSService) *Handler {
	h.NFSMountService = nm
	return h
}
