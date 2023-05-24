package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/hardwareresourcecount"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/nfsmountservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/opensearchbackupservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/portreachableservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/s3configservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/softwareversionservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/startmockserverservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/statusservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/stopmockserverservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/externalpostgresqlservice"
	"github.com/chef/automate/lib/logger"
)

type Handler struct {
	Logger                       logger.Logger
	StatusService                statusservice.IStatusService
	BatchCheckService            batchcheckservice.IBatchCheckService
	NFSMountService              nfsmountservice.INFSService
	MockServersService           startmockserverservice.IStartMockServersService
	HardwareResourceCountService hardwareresourcecount.IHardwareResourceCountService
	SoftwareVersionService       softwareversionservice.ISoftwareVersionService
	S3ConfigService              s3configservice.IS3Config
	StopMockServersService       stopmockserverservice.IStopMockServerService
	OSBackupService              opensearchbackupservice.IOSS3BackupService
	PortReachableService         portreachableservice.IPortReachableService
	ExternalPostgresqlService    externalpostgresqlservice.ISExternalPostgresqlService
}

func NewHandler(logger logger.Logger) *Handler {
	return &Handler{Logger: logger}
}

func (h *Handler) AddStatusService(ss statusservice.IStatusService) *Handler {
	h.StatusService = ss
	return h
}

func (h *Handler) AddMockServerServices(mss startmockserverservice.IStartMockServersService) *Handler {
	h.MockServersService = mss
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

func (h *Handler) AddSoftwareVersionService(sv softwareversionservice.ISoftwareVersionService) *Handler {
	h.SoftwareVersionService = sv
	return h
}

func (h *Handler) AddHardwareResourceCountService(hrc hardwareresourcecount.IHardwareResourceCountService) *Handler {
	h.HardwareResourceCountService = hrc
	return h
}
func (h *Handler) AddS3ConfigService(ss s3configservice.IS3Config) *Handler {
	h.S3ConfigService = ss
	return h
}

func (h *Handler) AddStopMockServerService(sm stopmockserverservice.IStopMockServerService) *Handler {
	h.StopMockServersService = sm
	return h
}

func (h *Handler) AddOSS3BackupService(ss opensearchbackupservice.IOSS3BackupService) *Handler {
	h.OSBackupService = ss
	return h
}

func (h *Handler) AddPortReachableService(pr portreachableservice.IPortReachableService) *Handler {
	h.PortReachableService = pr
	return h
}

func (h *Handler) AddExternalPostgresqlService(pg externalpostgresqlservice.ISExternalPostgresqlService) *Handler {
	h.ExternalPostgresqlService = pg
	return h
}
