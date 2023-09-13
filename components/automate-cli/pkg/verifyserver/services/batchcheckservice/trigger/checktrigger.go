package trigger

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type CheckTrigger struct {
	HardwareResourceCountCheck    ICheck
	SshUserAccessCheck            ICheck
	CertificateCheck              ICheck
	ExternalOpensearchCheck       ICheck
	ExternalPostgresCheck         ICheck
	FirewallCheck                 ICheck
	FqdnCheck                     ICheck
	NfsBackupConfigCheck          ICheck
	OpensearchS3BucketAccessCheck ICheck
	S3BackupConfigCheck           ICheck
	SoftwareVersionCheck          ICheck
	SystemResourceCheck           ICheck
	SystemUserCheck               ICheck
	GCPBackupConfigCheck          ICheck
}
type ICheck interface {
	Run(config *models.Config) []models.CheckTriggerResponse
	GetPortsForMockServer() map[string]map[string][]int
}

func NewCheckTrigger(hrc, sshC, cert, eop, epc, fc, fqdn, nfs, os3, s3b, svc, src, suc, gcpb ICheck) CheckTrigger {
	return CheckTrigger{
		HardwareResourceCountCheck:    hrc,
		SshUserAccessCheck:            sshC,
		CertificateCheck:              cert,
		ExternalOpensearchCheck:       eop,
		ExternalPostgresCheck:         epc,
		FirewallCheck:                 fc,
		FqdnCheck:                     fqdn,
		NfsBackupConfigCheck:          nfs,
		OpensearchS3BucketAccessCheck: os3,
		S3BackupConfigCheck:           s3b,
		SoftwareVersionCheck:          svc,
		SystemResourceCheck:           src,
		SystemUserCheck:               suc,
		GCPBackupConfigCheck:          gcpb,
	}
}
