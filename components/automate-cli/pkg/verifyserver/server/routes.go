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

	fiberutils.LogResgisteredRoutes(vs.App.Stack(), vs.Log)
}
