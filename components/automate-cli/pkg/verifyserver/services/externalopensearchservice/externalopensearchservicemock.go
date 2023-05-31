package externalopensearchservice

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"

type MockExternalOpensearchService struct {
	GetExternalOpensearchDetailsFunc func(reqBody models.ExternalOS, port int) models.ExternalOpensearchResponse
}

func (meos *MockExternalOpensearchService) GetExternalOpensearchDetails(reqBody models.ExternalOS, port int) models.ExternalOpensearchResponse {
	return meos.GetExternalOpensearchDetailsFunc(reqBody, port)
}
