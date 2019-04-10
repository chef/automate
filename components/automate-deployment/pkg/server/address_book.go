package server

import (
	"fmt"

	"github.com/sirupsen/logrus"
)

func (s *server) AddressForService(name string) string {
	// TODO(multinode) 2018-04-27: Eventually we will need to pull
	// this from the target if the service lives on another
	// machine.
	host := "127.0.0.1"
	var port int32
	switch name {
	case "authn-service":
		port = s.deployment.Config.GetAuthN().GetV1().GetSys().GetService().GetPort().GetValue()
	case "authz-service":
		port = s.deployment.Config.GetAuthZ().GetV1().GetSys().GetService().GetPort().GetValue()
	case "backup-gateway":
		port = s.deployment.Config.GetBackupGateway().GetV1().GetSys().GetService().GetPort().GetValue()
	case "compliance-service":
		port = s.deployment.Config.GetCompliance().GetV1().GetSys().GetService().GetPort().GetValue()
	case "config-mgmt-service":
		port = s.deployment.Config.GetConfigMgmt().GetV1().GetSys().GetService().GetPort().GetValue()
	case "ingest-service":
		port = s.deployment.Config.GetIngest().GetV1().GetSys().GetService().GetPort().GetValue()
	case "license-control-service":
		port = s.deployment.Config.GetLicenseControl().GetV1().GetSys().GetService().GetPort().GetValue()
	case "local-user-service":
		port = s.deployment.Config.GetLocalUser().GetV1().GetSys().GetService().GetPort().GetValue()
	case "teams-service":
		port = s.deployment.Config.GetTeams().GetV1().GetSys().GetService().GetPort().GetValue()
	default:
		logrus.Warnf("AddressForService called for unknown service %s", name)
		return ""
	}
	return fmt.Sprintf("%s:%d", host, port)
}
