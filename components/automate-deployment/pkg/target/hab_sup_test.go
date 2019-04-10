package target

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

type mockHabClient struct {
	svcInfo habapi.ServiceInfo
	err     error
}

func (m mockHabClient) ServiceInfo(ctx context.Context, serviceName string, serviceGroup string) (habapi.ServiceInfo, error) {
	return m.svcInfo, m.err
}

func TestHabSupHappyPath(t *testing.T) {
	cleanup := func() {
		defaultProcMount = "/proc"
		defaultLauncherPidFile = "/hab/sup/default/LOCK"
	}
	defer cleanup()

	defaultProcMount = "testdata/find-sup-happy/proc"
	defaultLauncherPidFile = "testdata/find-sup-happy/hablock"

	m := mockHabClient{
		svcInfo: habapi.ServiceInfo{
			Sys: habapi.SysInfo{
				Version: "0.63.0/20180914145954",
			},
		},
	}

	habSup := LocalHabSup(m)
	launcherPid, err := habSup.LauncherPid()
	require.NoError(t, err)
	assert.Equal(t, 13425, launcherPid)
	supPid, err := habSup.SupPid()
	require.NoError(t, err)
	assert.Equal(t, 13433, supPid)
	supPkg, err := habSup.SupPkg()
	require.NoError(t, err)
	assert.Equal(t, habpkg.NewFQ("core", "hab-sup", "0.63.0", "20180914145954"), supPkg)
}
