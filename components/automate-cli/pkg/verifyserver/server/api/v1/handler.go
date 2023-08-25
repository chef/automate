package v1

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/certificatevalidation"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/externalopensearchservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/externalpostgresqlservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/firewallservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/fqdnservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/gcpcloudstorageservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/hardwareresourcecount"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/mockserverservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/nfsmountservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/opensearchbackupservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/portreachableservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/s3configservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/softwareversionservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/sshusercheckservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/statusservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/systemresourceservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/systemuserservice"
	"github.com/chef/automate/lib/logger"
)

type Handler struct {
	Logger                       logger.Logger
	StatusService                statusservice.IStatusService
	BatchCheckService            batchcheckservice.IBatchCheckService
	NFSMountService              nfsmountservice.NFSService
	MockServersService           mockserverservice.MockServersService
	HardwareResourceCountService hardwareresourcecount.IHardwareResourceCountService
	SoftwareVersionService       softwareversionservice.ISoftwareVersionService
	S3ConfigService              s3configservice.IS3Config
	OSBackupService              opensearchbackupservice.IOSS3BackupService
	PortReachableService         portreachableservice.IPortReachableService
	ExternalPostgresqlService    externalpostgresqlservice.ExternalPostgresqlService
	SystemUserService            systemuserservice.SystemUserService
	SystemResourceService        systemresourceservice.SystemResourcesService
	ExternalOpensearchService    externalopensearchservice.IExternalOpensearchService
	FqdnService                  fqdnservice.IFqdnService
	FirewallService              firewallservice.IFirewallService
	ValidateCertificateService   certificatevalidation.IValidateCertificateService
	SshUserCheckService          sshusercheckservice.SshUserCheckService
	GCPConfigService             gcpcloudstorageservice.GCPCloudStorageConfig
}

func NewHandler(logger logger.Logger) *Handler {
	return &Handler{Logger: logger}
}

func (h *Handler) AddStatusService(ss statusservice.IStatusService) *Handler {
	h.StatusService = ss
	return h
}

func (h *Handler) AddMockServerService(mss mockserverservice.MockServersService) *Handler {
	h.MockServersService = mss
	return h
}

func (h *Handler) AddBatchCheckService(bc batchcheckservice.IBatchCheckService) *Handler {
	h.BatchCheckService = bc
	return h
}

func (h *Handler) AddNFSMountService(nm nfsmountservice.NFSService) *Handler {
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

func (h *Handler) AddSystemUserService(su systemuserservice.SystemUserService) *Handler {
	h.SystemUserService = su
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

func (h *Handler) AddFqdnService(fqdn fqdnservice.IFqdnService) *Handler {
	h.FqdnService = fqdn
	return h
}

func (h *Handler) AddExternalPostgresqlService(pg externalpostgresqlservice.ExternalPostgresqlService) *Handler {
	h.ExternalPostgresqlService = pg
	return h
}

func (h *Handler) AddSystemResourceService(srs systemresourceservice.SystemResourcesService) *Handler {
	h.SystemResourceService = srs
	return h
}

func (h *Handler) AddExternalOpensearchService(eos externalopensearchservice.IExternalOpensearchService) *Handler {
	h.ExternalOpensearchService = eos
	return h
}

func (h *Handler) AddFirewallService(fw firewallservice.IFirewallService) *Handler {
	h.FirewallService = fw
	return h
}

func (h *Handler) AddCertificateValidation(vc certificatevalidation.IValidateCertificateService) *Handler {
	h.ValidateCertificateService = vc
	return h
}

func (h *Handler) AddSshUserCheckService(ssu sshusercheckservice.SshUserCheckService) *Handler {
	h.SshUserCheckService = ssu
	return h
}
