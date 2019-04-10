package server

import (
	"testing"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/stretchr/testify/assert"
)

func TestIncludeInListIfNotAlreadyThere(t *testing.T) {

	configMgmtNodes := make(map[string]*api.InventoryNode)
	configMgmtNodes["1234"] = &api.InventoryNode{Name: "1234"}
	configMgmtNodes["9876"] = &api.InventoryNode{Name: "9876"}

	complianceNodes := make([]*reporting.Node, 0)
	complianceNodes = append(complianceNodes, &reporting.Node{Id: "5678"})

	list := includeIfNotAlreadyInList(configMgmtNodes, complianceNodes)
	assert.Equal(t, []*api.InventoryNode{{Name: "5678", Status: "active"}}, list)

	complianceNodes = append(complianceNodes, []*reporting.Node{{Id: "1234"}, {Id: "4567"}}...)

	list = includeIfNotAlreadyInList(configMgmtNodes, complianceNodes)
	assert.Equal(t, []*api.InventoryNode{{Name: "5678", Status: "active"}, {Name: "4567", Status: "active"}}, list)
}
