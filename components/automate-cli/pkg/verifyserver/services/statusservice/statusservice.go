package statusservice

import (
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
)

type IStatusService interface {
	GetServices() []models.ServiceDetails
}

type StatusService struct{}

func NewStatusService() IStatusService {
	return &StatusService{}
}

func (ss *StatusService) GetServices() []models.ServiceDetails {
	return []models.ServiceDetails{}
}
