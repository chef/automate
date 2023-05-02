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
	apiChecksGroup.Get("/fqdn", vs.Handler.CheckFqdn)
	apiChecksGroup.Post("/batch-checks", vs.Handler.BatchCheck)
	apiChecksGroup.Post("/nfs-mount", vs.Handler.NFSMount)
	apiChecksGroup.Post("/hardware-resource-count", vs.Handler.HardwareResourceCount)
	apiChecksGroup.Get("/software-versions", vs.Handler.CheckSoftwareVersion)
	apiChecksGroup.Get("/s3-config", vs.Handler.GetS3Config)

	apiStartGroup := apiV1Group.Group("/start")
	apiStartGroup.Post("/mock-server", vs.Handler.StartMockServer)

	fiberutils.LogResgisteredRoutes(vs.App.Stack(), vs.Log)
}
