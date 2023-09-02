package server

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
)

func (vs *VerifyServer) SetupRoutes() {
	// Status
	vs.App.Get("/status", vs.Handler.GetStatus)

	apiGroup := vs.App.Group("/api")
	apiV1Group := apiGroup.Group("/v1")
	apiChecksGroup := apiV1Group.Group("/checks")
	apiChecksGroup.Post("/fqdn", vs.Handler.CheckFqdn)
	apiChecksGroup.Post("/batch-checks", vs.Handler.BatchCheck)
	apiChecksGroup.Post("/nfs-mount", vs.Handler.NFSMount)
	apiChecksGroup.Post("/hardware-resource-count", vs.Handler.HardwareResourceCount)
	apiChecksGroup.Get("/software-versions", vs.Handler.CheckSoftwareVersion)
	apiChecksGroup.Post("/s3-config", vs.Handler.GetS3Config)
	apiChecksGroup.Post("/port-reachable", vs.Handler.PortReachable)
	apiChecksGroup.Post("/external-postgresql", vs.Handler.CheckExternalPostgresql)
	apiChecksGroup.Post("/certificate", vs.Handler.ValidateCertificate)
	apiChecksGroup.Post("/ssh-users", vs.Handler.CheckSshUser)
	apiChecksGroup.Get("/system-user", vs.Handler.CheckSystemUser)
	apiChecksGroup.Get("/system-resource", vs.Handler.GetSystemResource)
	apiChecksGroup.Post("/external-opensearch", vs.Handler.ExternalOpensearch)
	apiChecksGroup.Post("/firewall", vs.Handler.FirewallCheck)
	apiChecksGroup.Post("/aws-opensearch-s3-bucket-access", vs.Handler.CheckOSBackupS3)
	apiChecksGroup.Post("/gcp-cloud-storage-config", vs.Handler.GetGCPCloudStorageConfig)

	apiStartGroup := apiV1Group.Group("/start")
	apiStartGroup.Post("/mock-server", vs.Handler.StartMockServer)

	apiStopGroup := apiV1Group.Group("/stop")
	apiStopGroup.Post("/mock-server", vs.Handler.StopMockServer)

	apiFetchGroup := apiV1Group.Group("/fetch")
	apiFetchGroup.Post("/nfs-mount-loc", vs.Handler.NFSMountLoc)

	fiberutils.LogResgisteredRoutes(vs.App.Stack(), vs.Log)
}
