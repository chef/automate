package v1_test

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
	v1 "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server/api/v1"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/systemresourceservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
)

func SetupMockSystemResourceService() *systemresourceservice.MockSystemResourcesService {
	return &systemresourceservice.MockSystemResourcesService{
		GetSystemResourcesForDeploymentFunc: func(s1, s2 string) (*models.ApiResult, error) {
			//TODO
			return &models.ApiResult{}, nil
		},
	}
}

func SetupSystemResourceServiceHandler(srs systemresourceservice.ISystemResourcesService) (*fiber.App, error) {
	log, err := logger.NewLogger("text", "debug")
	if err != nil {
		return nil, err
	}

	fconf := &fiber.Settings{
		ServerHeader: server.SERVICE,
		ErrorHandler: fiberutils.CustomErrorHandler,
	}
	app := fiber.New(fconf)
	handler := v1.NewHandler(log).AddSystemResourceService(srs)

	vs := &server.VerifyServer{
		Port:    server.DEFAULT_PORT,
		Log:     log,
		App:     app,
		Handler: handler,
	}
	vs.SetupRoutes()
	return vs.App, nil
}
