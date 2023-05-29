package externalpostgresqlservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockExternalPostgresqlService struct {
	GetPgConnectionFunc func(*models.ExternalPgRequest) (*models.ExternalPgResponse, error)
}

func (mss *MockExternalPostgresqlService) GetPgConnection(req *models.ExternalPgRequest) (*models.ExternalPgResponse, error) {
	return mss.GetPgConnectionFunc(req)
}
