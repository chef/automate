package converge_test

import (
	"context"
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"github.com/stretchr/testify/require"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/converge"
	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/version"
)

var (
	testHabBinPkg      = habpkg.NewFQ("core", "hab", "0.55.0", "20180101010101")
	testHabSupPkg      = habpkg.NewFQ("core", "hab-sup", "0.55.0", "20180101010101")
	testHabLauncherPkg = habpkg.NewFQ("core", "hab-launcher", "8479", "20180917175912")

	testAutomateUnitFileContent = []byte("# Test Automate Unit File Content")
	testSupervisorState         = converge.NewSupervisorState(testHabSupPkg, testHabBinPkg, testHabLauncherPkg, "")
	testDeployedServicesMap     = map[string]target.DeployedService{}
)

type NullHabCache struct{}

func (n *NullHabCache) TDepsForPackage(habpkg.VersionedPackage) ([]habpkg.HabPkg, error) {
	return []habpkg.HabPkg{}, nil
}
func (n *NullHabCache) ListAllPackages() ([]habpkg.HabPkg, error) {
	return []habpkg.HabPkg{}, nil
}
func (n *NullHabCache) Delete(habpkg.HabPkg) error {
	return nil
}

func NewMockTarget(noDeploymentService ...bool) *target.MockTarget {
	if len(noDeploymentService) > 1 {
		panic("Cant have more than one input")
	}

	s := &mockHabSup{}
	s.On("SupPkg").Return(testHabSupPkg, nil)

	m := &target.MockTarget{}
	m.On("DeployedServices", mock.Anything).Maybe().Return(testDeployedServicesMap, nil)
	m.On("HabSup").Maybe().Return(s)
	m.On("HabCache").Maybe().Return(&NullHabCache{})
	version.BuildTime = "20180327165341"
	dsPkg := deploymentServicePackage()
	if len(noDeploymentService) == 0 {
		AlreadyInstalled(m, &dsPkg)
		NoSelfReconfigurePending(m)
		NoSupervisorUpgradeRequired(m)
		WithUserToml(m, &dsPkg, "")
	}

	return m
}

func NoSupervisorUpgradeRequired(m *target.MockTarget) *target.MockTarget {
	m.On("SystemdRunning").Return(true, nil)
	AlreadyInstalled(m, &testHabBinPkg)
	AlreadyInstalled(m, &testHabSupPkg)
	AlreadyInstalled(m, &testHabLauncherPkg)
	m.On("GetSymlinkedHabSup").Return(testHabSupPkg, nil)
	m.On("GetAutomateUnitFile").Return(testAutomateUnitFileContent, nil)
	m.On("RenderAutomateUnitFile", "", testHabBinPkg, testHabLauncherPkg).Return(string(testAutomateUnitFileContent), nil)
	m.On("SystemdReloadRequired").Return(false, nil)
	m.On("HabSupRestartRequired", testHabSupPkg).Return(false, nil)
	m.On("BinlinkPackage", &testHabBinPkg, "hab").Return("", nil)
	return m
}

type mockHabSup struct {
	mock.Mock
}

func (m *mockHabSup) SupPkg() (habpkg.HabPkg, error) {
	args := m.Called()
	return args.Get(0).(habpkg.HabPkg), args.Error(1)
}
func (m *mockHabSup) SupPid() (int, error) {
	args := m.Called()
	return args.Get(0).(int), args.Error(1)
}
func (m *mockHabSup) LauncherPid() (int, error) {
	args := m.Called()
	return args.Get(0).(int), args.Error(1)
}
func (m *mockHabSup) Hup(c context.Context) error {
	args := m.Called(c)
	return args.Error(0)
}

func NoSelfReconfigurePending(m *target.MockTarget) {
	m.On("GetDeploymentServiceReconfigurePending").Return(false, nil)
}

func AlreadyInstalled(m *target.MockTarget, pkg habpkg.VersionedPackage) {
	if p, ok := pkg.(*habpkg.HabPkg); ok {
		m.On("IsInstalled", p).Return(true, nil)
		return
	}

	if p, ok := pkg.(*habpkg.Hart); ok {
		m.On("IsInstalled", p).Return(true, nil)
		return
	}
	m.On("IsInstalled", &pkg).Return(true, nil)
}

func NotInstalled(m *target.MockTarget, pkg habpkg.VersionedPackage) {
	if p, ok := pkg.(*habpkg.HabPkg); ok {
		m.On("IsInstalled", p).Return(false, nil)
		return
	}

	if p, ok := pkg.(*habpkg.Hart); ok {
		m.On("IsInstalled", p).Return(false, nil)
		return
	}
	m.On("IsInstalled", &pkg).Return(false, nil)
}

func WithUserToml(m *target.MockTarget, pkg habpkg.VersionedPackage, currentUserToml string) {
	m.On("GetUserToml", pkg).Return(currentUserToml, nil)
}

func WithDeployedService(m *target.MockTarget, pkg habpkg.HabPkg, updateStrategy string) {
	for k := range testDeployedServicesMap {
		delete(testDeployedServicesMap, k)
	}
	testDeployedServicesMap[pkg.Name()] = target.DeployedService{
		Pkg:            pkg,
		UpdateStrategy: updateStrategy,
	}
}

func ExpectHabAPIClientWithResponse(m *target.MockTarget, response string) *httptest.Server {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintln(w, response)
	}))
	c := habapi.New(ts.URL)
	m.On("HabAPIClient").Return(c)
	return ts
}

func ExpectSetUserToml(m *target.MockTarget, pkg habpkg.VersionedPackage, config string) {
	m.On("SetUserToml", pkg.Name(), config).Return(nil)
}

func ExpectInstallHart(m *target.MockTarget, pkg *habpkg.Hart) {
	m.On("InstallService", pkg, "").Return(nil)
}

func ExpectInstallFromDepot(m *target.MockTarget, pkg habpkg.HabPkg) {
	m.On("InstallService", &pkg, "").Return(nil)
}

func ExpectUnloadService(m *target.MockTarget, pkg habpkg.HabPkg) {
	m.On("UnloadService", &pkg).Return(nil)
}

func ExpectLoadDeploymentService(m *target.MockTarget, pkg habpkg.VersionedPackage) {
	m.On("LoadDeploymentService", pkg).Return(nil)
}

func ExpectServiceReload(m *target.MockTarget, pkg habpkg.VersionedPackage) {
	m.
		On("UnloadService", pkg).
		Return(nil).
		On("LoadService", pkg, mock.Anything, mock.Anything).
		Return(nil)
}

type mockEventSink struct{}

func (*mockEventSink) Sink(converge.Event) {}

func noopEventSink() *mockEventSink {
	return &mockEventSink{}
}

func deploymentServicePackage() habpkg.HabPkg {
	return habpkg.NewFQ("chef", "deployment-service", "0.1.0", "20180327165341")
}

func deploymentService() converge.Service {
	p := deploymentServicePackage()
	return converge.Service{
		Name:          "deployment-service",
		ConvergeState: converge.Running(&p),
	}
}

func withServices(services ...converge.Service) []converge.Service {
	newServices := make([]converge.Service, len(services)+1)
	copy(newServices, services)
	newServices = append(newServices, deploymentService())

	return newServices
}

func TestCompiler(t *testing.T) {
	t.Run("the plan makes no modifications when no services are specified or running", func(t *testing.T) {
		target := NewMockTarget()

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(converge.NewDesiredState(
			converge.Topology{
				target: withServices(),
			},
			testSupervisorState,
			[]habpkg.HabPkg{},
			"conservative",
		))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		assert.NoError(t, err)

		target.AssertExpectations(t)
	})

}

func TestCompilerInstallPhaseDepot(t *testing.T) {
	pkg := habpkg.NewFQ("origin", "foo", "0.1.0", "20180131221115")
	desiredState := func(target target.Target) converge.DesiredState {
		return converge.NewDesiredState(converge.Topology{
			target: withServices(
				converge.Service{
					Name:          pkg.Name(),
					ConvergeState: converge.Installed(&pkg),
				},
			),
		},
			testSupervisorState,
			[]habpkg.HabPkg{},
			"conservative",
		)
	}

	t.Run("an unconstrained version doesn't install anything new if a package is already installed", func(t *testing.T) {
		target := NewMockTarget()
		AlreadyInstalled(target, &pkg)

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(desiredState(target))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		assert.NoError(t, err)

		target.AssertExpectations(t)
	})

	t.Run("an unconstrained version installs the latest package in a channel if one is not already installed", func(t *testing.T) {
		target := NewMockTarget()
		NotInstalled(target, &pkg)
		ExpectInstallFromDepot(target, pkg)

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(desiredState(target))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		assert.NoError(t, err)

		target.AssertExpectations(t)
	})
}

func TestCompilerInstallPhaseHart(t *testing.T) {
	pkg, _ := habpkg.HartFromPath("/src/results/origin-foo-0.1.0-20180131221115-x86_64-linux.hart")
	pkg.WithOrigin("origin")
	pkg.WithName("foo")
	desiredState := func(target target.Target) converge.DesiredState {
		return converge.NewDesiredState(converge.Topology{
			target: withServices(
				converge.Service{
					Name:          pkg.Name(),
					ConvergeState: converge.Installed(&pkg),
				},
			),
		}, testSupervisorState,
			[]habpkg.HabPkg{},
			"conservative",
		)
	}

	t.Run("a hart version constraint doesn't install anything new if a package is already installed", func(t *testing.T) {
		target := NewMockTarget()
		AlreadyInstalled(target, &pkg)

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(desiredState(target))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		assert.NoError(t, err)

		target.AssertExpectations(t)
	})

	t.Run("a hart version installs the package if it is not already installed", func(t *testing.T) {
		target := NewMockTarget()
		NotInstalled(target, &pkg)
		ExpectInstallHart(target, &pkg)

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(desiredState(target))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		assert.NoError(t, err)
		target.AssertExpectations(t)
	})
}

func TestCompilerRunningDepot(t *testing.T) {
	pkg := habpkg.NewFQ("origin", "foo", "0.1.0", "20180131221115")
	desiredState := func(target target.Target) converge.DesiredState {
		return converge.NewDesiredState(converge.Topology{
			target: withServices(
				converge.Service{
					Name:          pkg.Name(),
					ConvergeState: converge.Running(&pkg, converge.UserTOML("userToml")),
				},
			),
		},
			testSupervisorState,
			[]habpkg.HabPkg{},
			"conservative",
		)
	}
	t.Run("an unconstrained version doesn't install anything new if a package is already installed", func(t *testing.T) {
		t.Run("modifications are not made running service", func(t *testing.T) {
			target := NewMockTarget()
			WithDeployedService(target, pkg, "none")
			AlreadyInstalled(target, &pkg)
			WithUserToml(target, &pkg, "userToml")

			compiler := converge.NewPhaseOrderedCompiler()
			plan, err := compiler.Compile(desiredState(target))
			require.NoError(t, err)
			err = plan.Execute(noopEventSink())
			assert.NoError(t, err)

			target.AssertExpectations(t)
		})

		t.Run("a running desired state also configures", func(t *testing.T) {
			target := NewMockTarget()
			WithDeployedService(target, pkg, "none")
			AlreadyInstalled(target, &pkg)
			WithUserToml(target, &pkg, "")
			ExpectSetUserToml(target, &pkg, "userToml")

			compiler := converge.NewPhaseOrderedCompiler()
			plan, err := compiler.Compile(desiredState(target))
			require.NoError(t, err)
			err = plan.Execute(noopEventSink())
			assert.NoError(t, err)

			target.AssertExpectations(t)
		})

		t.Run("The service is unloaded and loaded if it had an update strategy of at-once", func(t *testing.T) {
			// Service update strategy at one point was controlled by Habitat. Hab no longer has that responsibility
			target := NewMockTarget()
			WithDeployedService(target, pkg, "at-once")
			// Mock unload check
			ts := ExpectHabAPIClientWithResponse(target, `[]`)
			defer ts.Close()
			AlreadyInstalled(target, &pkg)
			WithUserToml(target, &pkg, "userToml")
			ExpectServiceReload(target, &pkg)

			compiler := converge.NewPhaseOrderedCompiler()
			plan, err := compiler.Compile(desiredState(target))
			require.NoError(t, err)
			err = plan.Execute(noopEventSink())
			assert.NoError(t, err)

			target.AssertExpectations(t)

		})
	})

	t.Run("The service is installed and reloaded when the version changes", func(t *testing.T) {
		pkgCurrent := habpkg.NewFQ("origin", "foo", "0.1.0", "20180131220000")
		target := NewMockTarget()
		WithDeployedService(target, pkgCurrent, "none")
		// Mock unload check
		ts := ExpectHabAPIClientWithResponse(target, `[]`)
		defer ts.Close()
		NotInstalled(target, &pkg)
		WithUserToml(target, &pkg, "userToml")
		ExpectInstallFromDepot(target, pkg)
		ExpectServiceReload(target, &pkg)

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(desiredState(target))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		assert.NoError(t, err)

		target.AssertExpectations(t)
	})

}

func TestCompilerRunningHart(t *testing.T) {
	pkg, _ := habpkg.HartFromPath("/src/results/origin-foo-0.1.0-20180131221115-x86_64-linux.hart")
	pkg.WithOrigin("origin")
	pkg.WithName("foo")
	desiredState := func(target target.Target) converge.DesiredState {
		return converge.NewDesiredState(converge.Topology{
			target: withServices(
				converge.Service{
					Name:          pkg.Name(),
					ConvergeState: converge.Running(&pkg, converge.UserTOML("userToml")),
				},
			),
		}, testSupervisorState,
			[]habpkg.HabPkg{},
			"conservative",
		)
	}
	t.Run("a hart constraint doesn't install the package if it is already installed", func(t *testing.T) {
		t.Run("modifications are not made when running service is at desired version", func(t *testing.T) {
			target := NewMockTarget()
			WithDeployedService(target, habpkg.NewFQ("origin", "foo", "0.1.0", "20180131221115"), "none")
			AlreadyInstalled(target, &pkg)
			WithUserToml(target, &pkg, "userToml")

			compiler := converge.NewPhaseOrderedCompiler()
			plan, err := compiler.Compile(desiredState(target))
			require.NoError(t, err)
			err = plan.Execute(noopEventSink())
			assert.NoError(t, err)

			target.AssertExpectations(t)
		})
	})

	t.Run("The service is installed and reloaded when the version changes", func(t *testing.T) {
		pkgCurrent := habpkg.NewFQ("origin", "foo", "0.1.0", "20180131220000")
		target := NewMockTarget()
		WithDeployedService(target, pkgCurrent, "none")
		// Mock unload check
		ts := ExpectHabAPIClientWithResponse(target, `[]`)
		defer ts.Close()
		NotInstalled(target, &pkg)
		WithUserToml(target, &pkg, "userToml")
		ExpectInstallHart(target, &pkg)
		ExpectServiceReload(target, &pkg)

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(desiredState(target))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		assert.NoError(t, err)

		target.AssertExpectations(t)
	})

}

func TestCompilerCleanup(t *testing.T) {
	pkg := habpkg.NewFQ("origin", "foo", "0.1.0", "20180131221115")
	t.Run("the plan unloads services when no services are expected", func(t *testing.T) {
		target := NewMockTarget()
		WithDeployedService(target, pkg, "none")
		ExpectUnloadService(target, pkg)
		ts := ExpectHabAPIClientWithResponse(target, "[]")
		defer ts.Client()

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(converge.NewDesiredState(converge.Topology{
			target: withServices(),
		}, testSupervisorState,
			[]habpkg.HabPkg{},
			"conservative",
		))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		assert.NoError(t, err)

		target.AssertExpectations(t)
	})

	t.Run("skipped services are not cleaned up", func(t *testing.T) {
		target := NewMockTarget()
		WithDeployedService(target, pkg, "none")

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(converge.NewDesiredState(converge.Topology{
			target: withServices(
				converge.Service{
					Name:          pkg.Name(),
					ConvergeState: converge.Skip(),
				},
			),
		}, testSupervisorState,
			[]habpkg.HabPkg{},
			"conservative",
		))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		assert.NoError(t, err)

		target.AssertExpectations(t)
	})

	t.Run("running services with a desired state of installed are unloaded", func(t *testing.T) {
		target := NewMockTarget()
		WithDeployedService(target, pkg, "none")
		AlreadyInstalled(target, &pkg)
		ExpectUnloadService(target, pkg)
		ts := ExpectHabAPIClientWithResponse(target, "[]")
		defer ts.Client()

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(converge.NewDesiredState(converge.Topology{
			target: withServices(
				converge.Service{
					Name:          pkg.Name(),
					ConvergeState: converge.Installed(&pkg),
				},
			),
		}, testSupervisorState,
			[]habpkg.HabPkg{},
			"conservative",
		))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		assert.NoError(t, err)

		target.AssertExpectations(t)
	})
}

func TestSelfUpgradeHart(t *testing.T) {
	dsHartPath := "/src/results/chef-deployment-service-0.1.0-20180131221115-x86_64-linux.hart"
	dsPkg, _ := habpkg.HartFromPath(dsHartPath)
	dsPkg.WithOrigin("chef")
	dsPkg.WithName("deployment-service")
	pkg := habpkg.NewFQ("origin", "foo", "0.1.0", "20180131221115")
	desiredState := func(target target.Target) converge.DesiredState {
		return converge.NewDesiredState(converge.Topology{
			target: []converge.Service{
				{
					Name:          dsPkg.Name(),
					ConvergeState: converge.Running(&dsPkg),
				},
				{
					Name:          pkg.Name(),
					ConvergeState: converge.Running(&pkg, converge.UserTOML("userToml")),
				},
			},
		}, testSupervisorState,
			[]habpkg.HabPkg{},
			"conservative",
		)
	}
	t.Run("pending self upgrades make no further changes", func(t *testing.T) {
		target := NewMockTarget(true)
		NotInstalled(target, &dsPkg)
		AlreadyInstalled(target, &pkg)
		ExpectInstallHart(target, &dsPkg)
		ExpectLoadDeploymentService(target, &dsPkg)

		oldVersion := version.BuildTime
		version.BuildTime = "20180131220000"
		defer func() { version.BuildTime = oldVersion }()

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(desiredState(target))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		assert.Equal(t, api.ErrSelfUpgradePending, err)

		target.AssertExpectations(t)
	})
}

func TestSelfUpgradeDepot(t *testing.T) {
	dsPkg := habpkg.NewFQ("chef", "deployment-service", "0.1.0", "20180131221115")
	pkg := habpkg.NewFQ("origin", "foo", "0.1.0", "20180131221115")
	desiredState := func(target target.Target) converge.DesiredState {
		return converge.NewDesiredState(converge.Topology{
			target: []converge.Service{
				{
					Name:          dsPkg.Name(),
					ConvergeState: converge.Running(&dsPkg),
				},
				{
					Name:          pkg.Name(),
					ConvergeState: converge.Running(&pkg, converge.UserTOML("userToml")),
				},
			},
		}, testSupervisorState,
			[]habpkg.HabPkg{},
			"conservative",
		)
	}
	t.Run("pending self upgrades make no further changes", func(t *testing.T) {
		target := NewMockTarget(true)
		NotInstalled(target, &dsPkg)
		AlreadyInstalled(target, &pkg)
		ExpectInstallFromDepot(target, dsPkg)
		ExpectLoadDeploymentService(target, &dsPkg)

		oldVersion := version.BuildTime
		version.BuildTime = "20180131220000"
		defer func() { version.BuildTime = oldVersion }()

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(desiredState(target))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		assert.Equal(t, api.ErrSelfUpgradePending, err)

		target.AssertExpectations(t)
	})
	t.Run("will not downgrade the deployment-service", func(t *testing.T) {
		currentDsPkg := habpkg.NewFQ("chef", "deployment-service", "0.1.0", "20181231235959")
		target := NewMockTarget(true)
		WithDeployedService(target, currentDsPkg, "none")
		NotInstalled(target, &dsPkg)
		AlreadyInstalled(target, &pkg)
		ExpectInstallFromDepot(target, dsPkg) // TODO: we also might not want to install from depot
		// do not expect a load deployment service

		compiler := converge.NewPhaseOrderedCompiler()
		plan, err := compiler.Compile(desiredState(target))
		require.NoError(t, err)
		err = plan.Execute(noopEventSink())
		require.Error(t, err)

		target.AssertExpectations(t)
	})
}
