package pgdb_test

import (
	"github.com/chef/automate/components/nodemanager-service/api/manager"
)

func (suite *NodeManagersAndNodesDBSuite) TestUpdateNodeManagerStatus() {
	//s := &secrets.Secret{Type: "aws"}
	// add one manager
	//secretId, err := suite.Database.AddSecret(s)
	//if err != nil {
	//suite.FailNow(err.Error())
	//}
	// TODO @afiune Mock the secrets-service
	secretId := "12345678901234567890123456789012"
	mgr := manager.NodeManager{Name: "tester", Type: "aws-ec2", CredentialId: secretId}
	id, err := suite.Database.AddNodeManager(&mgr, "12345678")
	if err != nil {
		suite.FailNow(err.Error())
	}

	err = suite.Database.UpdateManagerStatus(id, "unreachable")
	if err != nil {
		suite.FailNow(err.Error())
	}

	readMgr, err := suite.Database.GetNodeManager(id)
	err = suite.Database.UpdateManagerStatus(id, "unreachable")
	if err != nil {
		suite.FailNow(err.Error())
	}
	suite.Equal(readMgr.Status, "unreachable")
}
