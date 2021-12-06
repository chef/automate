package server_test

import (
	"testing"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	infra_proxy "github.com/chef/automate/api/interservice/infra_proxy/service"
	"github.com/golang/mock/gomock"
)

func TestGetAutomateInfraServerOrgs(t *testing.T) {
	//ctx := context.Background()
	// _, _, _, close, _, _, infraMockClient := test.SetupInfraProxyService(ctx, t)
	infraMockClient := infra_proxy.NewMockInfraProxyServiceClient(gomock.NewController(t))

	//cl := infra_proxy.NewInfraProxyServiceClient(conn)
	//defer close()

	req := request.AutomateInfraServerOrgs{
		ServerId: "infra-server",
		WebuiKey: "",
	}
	res := []response.AutomateInfraServerOrg{{
		Id:           "test",
		Name:         "test",
		AdminUser:    "pivotal",
		CredentialId: "",
		ServerId:     "pivotal",
		Projects:     nil,
	}}
	orgReq := &request.DeleteOrg{
		Id:       res[0].Id,
		ServerId: res[0].ServerId,
	}

	t.Run("GetAutomateInfraServerOrgs", func(t *testing.T) {
		infraMockClient.EXPECT().GetAutomateInfraServerOrgs(gomock.Any(), &req, gomock.Any()).Return(res, nil)
		infraMockClient.EXPECT().DeleteOrg(gomock.Any(), orgReq, gomock.Any())
	})
}
