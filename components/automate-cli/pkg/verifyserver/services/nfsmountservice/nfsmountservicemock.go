package nfsmountservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/gofiber/fiber"
)

type MockNFSMountService struct {
	GetNFSMountDetailsFunc func() (*[]*models.NFSMountResponse, *fiber.Error)
}

func (mnm *MockNFSMountService) GetServices() (*[]*models.NFSMountResponse, *fiber.Error) {
	return mnm.GetNFSMountDetailsFunc()
}
