package airgap

import (
	"github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/lib/io/fileutils"
)

func AirgapInUse() bool {
	offlineMode, _ := fileutils.PathExists(deployment.AirgapManifestPath)
	return offlineMode
}
