package client

import (
	"os"

	"github.com/sirupsen/logrus"

	dc "github.com/chef/automate/api/config/deployment"
)

// SetProxyEnvironment sets the http_proxy and no_proxy environment
// variables based on the given configuration request.
func SetProxyEnvironment(config *dc.ConfigRequest) {
	proxyConnectionString := config.GetV1().GetSys().GetProxy().GetConnectionString().GetValue()
	if proxyConnectionString != "" {
		logrus.Infof("Setting environment variable https_proxy=%s", proxyConnectionString)
		os.Setenv("https_proxy", proxyConnectionString)
	}

	noProxyString := config.GetV1().GetSys().GetProxy().GetNoProxyString().GetValue()
	if noProxyString != "" {
		logrus.Infof("Setting environment variable no_proxy=%s", noProxyString)
		os.Setenv("no_proxy", noProxyString)
	}
}
