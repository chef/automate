package trigger

type ICheckTrigger interface {
	HardwareResourceCountCheck()
	SshUserAccessCheck()
	CertificateCheck()
	SystemResourceCheck()
	ExternalOpensearchCheck()
	ExternalPostgresCheck()
	FirewallCheck()
	FqdnCheck()
	NfsBackupConfigCheck()
	S3BackupConfigCheck()
	SoftwareVersionCheck()
	SystemUserCheck()
}

type CheckTrigger struct {
}

func NewCheckTrigger() ICheckTrigger {
	return &CheckTrigger{}
}