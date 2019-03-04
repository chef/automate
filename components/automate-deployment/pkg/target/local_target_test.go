// Copyright © 2017 Chef Software

package target

import (
	"bytes"
	"context"
	"fmt"
	"io/ioutil"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"os/user"
	"path/filepath"
	"syscall"
	"testing"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"github.com/stretchr/testify/require"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/parser"
	"github.com/chef/automate/lib/platform/command"
)

type mockUserLookupProvider struct {
	mock.Mock
}

func (m *mockUserLookupProvider) Reset() {
	m.Mock = mock.Mock{}
}

func (m *mockUserLookupProvider) Lookup(username string) (*user.User, error) {
	args := m.Called(username)
	if args.Get(0) == nil {
		return nil, args.Error(1)
	}
	return args.Get(0).(*user.User), args.Error(1)
}
func (m *mockUserLookupProvider) LookupGroup(groupname string) (*user.Group, error) {
	args := m.Called(groupname)
	if args.Get(0) == nil {
		return nil, args.Error(1)
	}

	return args.Get(0).(*user.Group), args.Error(1)
}
func (m *mockUserLookupProvider) LookupGroupId(gid string) (*user.Group, error) {
	args := m.Called(gid)
	if args.Get(0) == nil {
		return nil, args.Error(1)
	}
	return args.Get(0).(*user.Group), args.Error(1)
}

type recallingTempFileProvider struct {
	files []*os.File
}

func (p *recallingTempFileProvider) NextFileName() (string, error) {
	f, err := ioutil.TempFile("", "test-temp-file")
	if p.files == nil {
		p.files = make([]*os.File, 0, 2)
	}
	p.files = append(p.files, f)
	return f.Name(), err
}

func (p *recallingTempFileProvider) TempFile(dir, prefix string) (*os.File, error) {
	if len(p.files) > 0 {
		f := p.files[0]
		p.files = p.files[1:]
		return f, nil
	}

	return ioutil.TempFile(dir, prefix)
}

var testTempFileProvider = &recallingTempFileProvider{}

type execMock struct {
	expectedCommand command.ExpectedCommand
	output          string
	err             error
}

func expectHabCommand(cmd string, args ...string) command.ExpectedCommand {
	timeout := HabTimeoutDefault
	if len(args) > 1 && args[1] == "install" {
		timeout = HabTimeoutInstallPackage
	}
	return command.ExpectedCommand{
		Cmd:     cmd,
		Timeout: timeout,
		Env:     []string{"HAB_NOCOLORING=true", "HAB_NONINTERACTIVE=true"},
		Args:    args,
	}
}

func expectCommand(cmd string, args ...string) command.ExpectedCommand {
	return command.ExpectedCommand{
		Cmd:  cmd,
		Args: args,
	}
}

func newMock(expected command.ExpectedCommand, output string, err error) execMock {
	return execMock{
		expectedCommand: expected,
		output:          output,
		err:             err,
	}
}

func setupTestLocalTarget(t *testing.T) (*LocalTarget, *command.MockExecutor, func(t *testing.T)) {
	mockExec := command.NewMockExecutor(t)
	tgt := NewLocalTarget(false)
	defaultTempFileProvider = testTempFileProvider
	tgt.Executor = mockExec
	tgt.HabCmd = NewHabCmd(mockExec, false)
	tgt.HabBackoff = 1 * time.Millisecond

	return tgt, mockExec, func(t *testing.T) {
		mockExec.AssertAllCalled()
	}
}

func TestServiceConfigureFails(t *testing.T) {
	h := NewLocalTarget(false)
	h.HabBaseDir = "/dev/null/foobar"

	err := h.SetUserToml("foo", "")
	assert.Error(t, err)
}

// TODO ideally we will more fully test this function
func TestServiceConfigureSucceeds(t *testing.T) {
	testDir, _ := ioutil.TempDir("", "TestServiceConfigureFails")
	defer os.Remove(testDir)

	h := NewLocalTarget(false)
	h.HabBaseDir = testDir

	config := "config"
	err := h.SetUserToml("foo", config)
	assert.Nil(t, err)

	c, err := ioutil.ReadFile(filepath.Join(testDir, "user", "foo", "config", "user.toml"))
	assert.Nil(t, err)
	assert.Equal(t, config, string(c))
}

func TestLocalTarget_StartService(t *testing.T) {
	tests := []struct {
		name     string
		origin   string
		svcName  string
		binds    []string
		bindMode string
		wantErr  bool
		mocks    []execMock
	}{

		{"fails on hab start", "origin", "name", nil, "", true, []execMock{
			newMock(expectHabCommand("hab", "svc", "load", "--force", "origin/name", "--strategy", "none"), "", errors.New("")),
		}},
		{"succeeds with no binds", "origin", "name", nil, "", false, []execMock{
			newMock(expectHabCommand("hab", "svc", "load", "--force", "origin/name", "--strategy", "none"), "success", nil),
		}},
		{"succeeds with multiple binds", "origin", "svc-with-binds", []string{"service1:service1.default", "service2:service2.default"}, "strict",
			false, []execMock{

				newMock(expectHabCommand("hab", "svc", "load", "--force", "origin/svc-with-binds", "--strategy", "none",
					"--bind", "service1:service1.default",
					"--bind", "service2:service2.default",
					"--binding-mode", "strict"), "success", nil),
			}},
		{"succeeds with multiple binds and relaxed bind mode", "origin", "svc-with-relaxed-binds", []string{"service3:service3.default", "service4:service4.default"}, "relaxed",
			false, []execMock{
				newMock(expectHabCommand("hab", "svc", "load", "--force", "origin/svc-with-relaxed-binds", "--strategy", "none",
					"--bind", "service3:service3.default",
					"--bind", "service4:service4.default",
					"--binding-mode", "relaxed"), "success", nil),
			}},
		{"succeeds with no binds and persistent", "origin", "name", nil, "", false, []execMock{
			newMock(expectHabCommand("hab", "svc", "load", "--force", "origin/name", "--strategy", "none"), "success", nil),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			testSvc := habpkg.New(tt.origin, tt.svcName)

			h, mockExec, finish := setupTestLocalTarget(t)

			for _, mock := range tt.mocks {
				mockExec.Expect("CombinedOutput", mock.expectedCommand).Return(mock.output, mock.err).Once()
			}

			if err := h.LoadService(&testSvc, Binds(tt.binds), BindMode(tt.bindMode)); (err != nil) != tt.wantErr {
				t.Errorf("LocalTarget.StartService() error = %v, wantErr %v", err, tt.wantErr)
			}

			finish(t)
		})
	}
}

func TestLocalTarget_Status(t *testing.T) {
	type args struct {
		serviceName string
	}
	tests := []struct {
		name       string
		args       args
		statusCode int
		want       api.ServiceState
	}{
		{"service health is ok if health check returns 200", args{"redis"}, 200, api.ServiceState{Name: "redis", State: api.ServiceState_OK}},
		{"service health is down if health check returns 404", args{"redis"}, 404, api.ServiceState{Name: "redis", State: api.ServiceState_DOWN}},
		{"service health is critical if health check returns 500", args{"redis"}, 500, api.ServiceState{Name: "redis", State: api.ServiceState_UNKNOWN}},
		{"service health is unhealthy if health check returns 503", args{"redis"}, 503, api.ServiceState{Name: "redis", State: api.ServiceState_CRITICAL}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ts := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path == "/services" {
					w.Write([]byte(`[{"pkg": {"origin": "core", "name": "redis" }, "process": {"pid": 100} }]`))
				} else {
					w.WriteHeader(tt.statusCode)
				}
			}))
			defer ts.Close()
			customURL := "127.0.0.1:9631"
			l, err := net.Listen("tcp", customURL)
			require.NoError(t, err)
			ts.Listener = l
			ts.Start()
			defer ts.Close()

			tgt := NewLocalTarget(false)
			expectedStatus := &api.ServiceStatus{}
			expectedStatus.Add(&tt.want)
			got := tgt.Status(context.Background(), []string{tt.args.serviceName})
			assert.Len(t, got.Services, 1)
			assert.Equal(t, tt.want.GetName(), got.Services[0].GetName())
			assert.Equal(t, uint64(100), got.Services[0].GetPid())
			assert.Equal(t, tt.want.GetState(), got.Services[0].GetState())
		})
	}
}

func Test_startHabSupFromSystemd(t *testing.T) {
	writer := cli.NewWriter(ioutil.Discard, ioutil.Discard, bytes.NewBuffer(nil))
	reloadCmd := expectCommand("systemctl", "daemon-reload")
	enableCmd := expectCommand("systemctl", "enable", "chef-automate.service")
	startCmd := expectCommand("systemctl", "start", "chef-automate.service")

	habStatusRetryWait = 0 * time.Second

	tests := []struct {
		name    string
		wantErr bool
		mocks   []execMock
	}{
		{"returns error when systemctl cannot reload the daemon", true, []execMock{
			newMock(reloadCmd, "", errors.New("")),
		}},
		{"returns error when systemctl cannot enable our service", true, []execMock{
			newMock(reloadCmd, "", nil),
			newMock(enableCmd, "", errors.New("")),
		}},
		{"returns error when systemctl cannot enable our service", true, []execMock{
			newMock(reloadCmd, "", nil),
			newMock(enableCmd, "", nil),
			newMock(startCmd, "", errors.New("")),
		}},
		{"returns nil when systemctl starts our service", false, []execMock{
			newMock(reloadCmd, "", nil),
			newMock(enableCmd, "", nil),
			newMock(startCmd, "", nil),
		}},
	}
	for _, tt := range tests {
		tgt, mockExec, finish := setupTestLocalTarget(t)

		t.Run(tt.name, func(t *testing.T) {
			for _, mock := range tt.mocks {
				mockExec.Expect("CombinedOutput", mock.expectedCommand).Return(mock.output, mock.err).Once()
			}
			if err := tgt.startHabSupFromSystemd(writer); (err != nil) != tt.wantErr {
				t.Errorf("startHabSupFromSystemd() error = %v, wantErr %v", err, tt.wantErr)
			}
		})

		finish(t)
	}
}

func mockManifest(data []byte) *manifest.A2 {
	m, err := parser.ManifestFromBytes(data)
	if err != nil {
		panic("Error reading manifest fixture")
	}
	return m
}

func habResponseFixture(name string) []byte {
	data, err := ioutil.ReadFile(fmt.Sprintf("../../testdata/hab_responses/%s.json", name))
	if err != nil {
		logrus.WithError(err).Fatalf("Failed reading test fixture data for %s", name)
	}
	return data
}

func TestInstallHabitat(t *testing.T) {
	writer := cli.NewWriter(ioutil.Discard, ioutil.Discard, bytes.NewBuffer(nil))
	manifest := mockManifest(habResponseFixture("manifest"))
	hasHabCmd := expectCommand("bash", "-c", "command -v hab")
	habHabInstall := expectHabCommand("hab", "pkg", "install", "core/hab/0.54.0/20180221022026")
	habBinlink := expectHabCommand("hab", "pkg", "binlink", "--force", "core/hab/0.54.0/20180221022026", "hab")

	mockScriptInstall := func(t *testing.T, mockExec *command.MockExecutor, tempDir string, installError error) {
		filename, err := testTempFileProvider.NextFileName()
		require.NoError(t, err)
		mockExec.Expect("CombinedOutput", command.ExpectedCommand{
			Env:  []string{fmt.Sprintf("TMPDIR=%s/tmp", tempDir)},
			Cmd:  "bash",
			Args: []string{filename, "-v", "0.54.0/20180221022026"},
		}).Return("", installError).Once()
	}

	tests := []struct {
		name    string
		wantErr bool
		mockFun func(*testing.T, *command.MockExecutor, string)
	}{
		{"installs hab via the install script if hab can't be found", false,
			func(t *testing.T, mockExec *command.MockExecutor, tempDir string) {
				mockExec.Expect("CombinedOutput", hasHabCmd).Return("", errors.New("")).Once()
				mockScriptInstall(t, mockExec, tempDir, nil)
				mockExec.Expect("CombinedOutput", habBinlink).Return("", nil).Once()
			},
		},
		{"installs hab via the hab if hab is found", false,
			func(t *testing.T, mockExec *command.MockExecutor, tempDir string) {
				mockExec.Expect("CombinedOutput", hasHabCmd).Return("/bin/hab", nil).Once()
				mockExec.Expect("CombinedOutput", habHabInstall).Return("", nil).Once()
				mockExec.Expect("CombinedOutput", habBinlink).Return("", nil).Once()
			},
		},
		{"returns error when hab script install fails", true,
			func(t *testing.T, mockExec *command.MockExecutor, tempDir string) {
				mockExec.Expect("CombinedOutput", hasHabCmd).Return("", errors.New("")).Once()
				mockScriptInstall(t, mockExec, tempDir, errors.New("oops"))
			},
		},
		{"returns error when hab hab install fails", true,
			func(t *testing.T, mockExec *command.MockExecutor, tempDir string) {
				mockExec.Expect("CombinedOutput", hasHabCmd).Return("/bin/hab", nil).Once()
				mockExec.Expect("CombinedOutput", habHabInstall).Return("", errors.New("oops")).Once()
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tgt, mockExec, finish := setupTestLocalTarget(t)

			tempDir, _ := ioutil.TempDir("", "TestInstallHab")
			defer os.Remove(tempDir)
			tgt.HabBaseDir = tempDir

			tt.mockFun(t, mockExec, tempDir)
			if err := tgt.InstallHabitat(manifest, writer); (err != nil) != tt.wantErr {
				t.Errorf("ensureHab() error = %v, wantErr %v", err, tt.wantErr)
			}
			finish(t)
		})
	}
}

func Test_installHabComponents(t *testing.T) {
	writer := cli.NewWriter(ioutil.Discard, ioutil.Discard, bytes.NewBuffer(nil))
	manifest := mockManifest(habResponseFixture("manifest"))
	installHabSupCmd := expectHabCommand("hab", "pkg", "install", "core/hab-sup/0.54.0/20180221023448")
	installHabLauncherCmd := expectHabCommand("hab", "pkg", "install", "core/hab-launcher/7241/20180321162126")

	tests := []struct {
		name    string
		wantErr bool
		mocks   []execMock
	}{
		{"returns nil when hab component installs are successful", false, []execMock{
			newMock(installHabSupCmd, "", nil),
			newMock(installHabLauncherCmd, "", nil),
		}},
		{"returns error when hab-sup install fails", true, []execMock{
			newMock(installHabSupCmd, "", errors.New("")),
		}},
		{"returns error when hab-launcher install fails", true, []execMock{
			newMock(installHabSupCmd, "", nil),
			newMock(installHabLauncherCmd, "", errors.New("")),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tgt, mockExec, finish := setupTestLocalTarget(t)

			for _, mock := range tt.mocks {
				mockExec.Expect("CombinedOutput", mock.expectedCommand).Return(mock.output, mock.err).Once()
			}
			if err := tgt.installHabComponents(manifest, writer); (err != nil) != tt.wantErr {
				t.Errorf("ensureHab() error = %v, wantErr %v", err, tt.wantErr)
			}
			finish(t)
		})
	}
}

func TestStop(t *testing.T) {
	stopCmd := expectCommand("hab", "sup", "term")
	tests := []struct {
		name    string
		wantErr bool
		mocks   []execMock
	}{
		{"returns error when systemctl cannot stop services", true, []execMock{
			newMock(stopCmd, "", errors.New("")),
		}},
		{"returns nil when systemctl stops services", false, []execMock{
			newMock(stopCmd, "", nil),
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			h, mockExec, finish := setupTestLocalTarget(t)
			for _, mock := range tt.mocks {
				mockExec.Expect("Start", mock.expectedCommand).Return(mock.err).Once()
			}
			if err := h.Stop(); (err != nil) != tt.wantErr {
				t.Errorf("Stop() error = %v, wantErr %v", err, tt.wantErr)
			}
			finish(t)
		})
	}
}

func ensureHabUserRealTests(t *testing.T) {
	writer := cli.NewWriter(ioutil.Discard, ioutil.Discard, bytes.NewBuffer(nil))
	oldHabUser := habUser
	oldHabGroup := habGroup
	habUser = fmt.Sprintf("test-hab-user-%d", os.Getpid())
	habGroup = habUser
	defer func() {
		habUser = oldHabUser
		habGroup = oldHabGroup
	}()

	makeTarget := func() (*LocalTarget, *command.MockExecutor) {
		mockExec := command.NewMockExecutor(t, command.Passthrough(command.NewExecExecutor()))
		tgt := NewLocalTarget(false)
		tgt.Executor = mockExec
		return tgt, mockExec
	}

	t.Run("it does nothing if user and group both already exist", func(t *testing.T) {
		tgt, mockExec := makeTarget()
		err := command.Run("useradd", command.Args("-U", habUser))
		require.NoError(t, err)
		defer func() {
			err := command.Run("userdel", command.Args(habUser))
			require.NoError(t, err, "test user cleanup")
			_ = command.Run("groupdel", command.Args(habGroup))
		}()

		err = tgt.EnsureHabUser(writer)
		require.NoError(t, err)
		mockExec.AssertAllCalled()
	})

	t.Run("if the group and user doesn't exist, it creates both using the -U flags", func(t *testing.T) {
		tgt, mockExec := makeTarget()
		defer func() {
			err := command.Run("userdel", command.Args(habUser))
			require.NoError(t, err, "test user cleanup")
			_ = command.Run("groupdel", command.Args(habGroup))
		}()
		mockExec.Expect("CombinedOutput", command.ExpectedCommand{
			Cmd:  "useradd",
			Args: []string{habUser, "-U"},
		}).Return("", nil)
		err := tgt.EnsureHabUser(writer)
		require.NoError(t, err)

		mockExec.AssertAllCalled()
	})

	t.Run("if the group exists and the user doesn't exist, it creates the user using the -g flags", func(t *testing.T) {
		tgt, mockExec := makeTarget()
		err := command.Run("groupadd", command.Args(habGroup))
		require.NoError(t, err, "test user cleanup")

		defer func() {
			err := command.Run("userdel", command.Args(habUser))
			require.NoError(t, err, "test user cleanup")
			_ = command.Run("groupdel", command.Args(habGroup))
		}()

		mockExec.Expect("CombinedOutput", command.ExpectedCommand{
			Cmd:  "useradd",
			Args: []string{habUser, "-g", habGroup},
		}).Return("", nil)
		err = tgt.EnsureHabUser(writer)
		require.NoError(t, err)

		mockExec.AssertAllCalled()
	})

	t.Run("if the user exists but the group doesn't, it returns an error", func(t *testing.T) {
		tgt, mockExec := makeTarget()
		tmpGroup := fmt.Sprintf("not-%s", habGroup)
		err := command.Run("groupadd", command.Args(tmpGroup))
		require.NoError(t, err, "group create")

		err = command.Run("useradd", command.Args(habUser, "-g", tmpGroup))
		require.NoError(t, err)

		defer func() {
			err := command.Run("userdel", command.Args(habUser))
			require.NoError(t, err, "test user cleanup")
			_ = command.Run("groupdel", command.Args(tmpGroup))
		}()
		err = tgt.EnsureHabUser(writer)
		assert.Error(t, err)
		mockExec.AssertAllCalled()
	})
}

func ensureHabUserMockedTests(t *testing.T) {
	outbuf := new(bytes.Buffer)
	writer := cli.NewWriter(outbuf, outbuf, bytes.NewBuffer(nil))
	mockUserLookup := new(mockUserLookupProvider)
	oldUserLookupProvider := defaultUserLookupProvider
	defaultUserLookupProvider = mockUserLookup
	defer func() { defaultUserLookupProvider = oldUserLookupProvider }()

	makeTarget := func() (*LocalTarget, *command.MockExecutor) {
		mockExec := command.NewMockExecutor(t)
		tgt := NewLocalTarget(false)
		tgt.Executor = mockExec
		return tgt, mockExec
	}

	t.Run("if the group and user doesn't exist, it creates both using the -U flags", func(t *testing.T) {
		tgt, mockExec := makeTarget()
		mockUserLookup.On("Lookup", habUser).Return(nil, user.UnknownUserError(habUser))
		mockUserLookup.On("LookupGroup", habGroup).Return(nil, user.UnknownGroupError(habGroup))
		mockExec.Expect("CombinedOutput", command.ExpectedCommand{
			Cmd:  "useradd",
			Args: []string{habUser, "-U"},
		}).Return("", nil)
		err := tgt.EnsureHabUser(writer)
		t.Log(outbuf.String())
		require.NoError(t, err)

		mockExec.AssertAllCalled()
		mockUserLookup.AssertExpectations(t)
		mockUserLookup.Reset()
	})

	t.Run("it does nothing if user and group both already exist", func(t *testing.T) {
		tgt, mockExec := makeTarget()
		mockUserLookup.On("Lookup", habUser).Return(&user.User{}, nil)
		mockUserLookup.On("LookupGroup", habGroup).Return(&user.Group{}, nil)
		err := tgt.EnsureHabUser(writer)
		require.NoError(t, err)
		mockExec.AssertAllCalled()
		mockUserLookup.AssertExpectations(t)
		mockUserLookup.Reset()
	})

	t.Run("if the group exists and the user doesn't exist, it creates the user using the -g flags", func(t *testing.T) {
		tgt, mockExec := makeTarget()
		mockUserLookup.On("Lookup", habUser).Return(nil, user.UnknownUserError(habUser))
		mockUserLookup.On("LookupGroup", habGroup).Return(&user.Group{}, nil)
		mockExec.Expect("CombinedOutput", command.ExpectedCommand{
			Cmd:  "useradd",
			Args: []string{habUser, "-g", habGroup},
		}).Return("", nil)
		err := tgt.EnsureHabUser(writer)
		require.NoError(t, err)

		mockExec.AssertAllCalled()
		mockUserLookup.AssertExpectations(t)
		mockUserLookup.Reset()
	})

	t.Run("if the user exists but the group doesn't, it returns an error", func(t *testing.T) {
		tgt, mockExec := makeTarget()
		mockUserLookup.On("Lookup", habUser).Return(&user.User{Gid: "test-gid"}, nil)
		mockUserLookup.On("LookupGroupId", "test-gid").Return(&user.Group{}, nil)
		mockUserLookup.On("LookupGroup", habGroup).Return(nil, user.UnknownGroupError(habGroup))
		err := tgt.EnsureHabUser(writer)
		assert.Error(t, err)
		mockExec.AssertAllCalled()
		mockUserLookup.AssertExpectations(t)
		mockUserLookup.Reset()
	})

}

func Test_EnsureHabUser(t *testing.T) {
	realTestEnvVar := os.Getenv("REAL_USERADD_TESTS")
	if realTestEnvVar == "true" {
		t.Log("REAL_USERADD_TESTS = true, testing with actual commands.")
		ensureHabUserRealTests(t)
	} else {
		t.Log("REAL_USERADD_TESTS != true, ensureHabUser tests will use mocking only")
		ensureHabUserMockedTests(t)
	}
}

func TestLocalTarget_InstallService(t *testing.T) {
	testSvc := habpkg.New("origin", "name")
	installCmd := expectHabCommand("hab", "pkg", "install", "origin/name", "--channel", "stable")

	tests := []struct {
		name    string
		wantErr bool
		mocks   []execMock
	}{
		{"uses provided channel", false, []execMock{
			newMock(installCmd, "", nil),
		}},
		{"errors if install fails", true, []execMock{
			newMock(installCmd, "", errors.New("install failed")),
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			h, mockExec, finish := setupTestLocalTarget(t)

			for _, mock := range tt.mocks {
				mockExec.Expect("CombinedOutput", mock.expectedCommand).Return(mock.output, mock.err).Once()
			}
			if err := h.InstallService(&testSvc, "stable"); (err != nil) != tt.wantErr {
				t.Errorf("LocalTarget.InstallService() error = %v, wantErr %v", err, tt.wantErr)
			}
			finish(t)
		})
	}
}

func TestRemoveService(t *testing.T) {
	tgt, mockExec, finish := setupTestLocalTarget(t)
	tempDir, _ := ioutil.TempDir("", "TestRemoveService")
	tgt.HabBaseDir = tempDir

	defer func() {
		os.RemoveAll(tempDir)
		finish(t)
	}()

	testPkgDir := filepath.Join(tempDir, "pkgs", "origin", "name")
	os.MkdirAll(testPkgDir, os.ModePerm)

	mockExec.Expect("CombinedOutput", expectHabCommand("hab", "svc", "unload", "origin/name")).Return("", nil).Once()

	svc := habpkg.New("origin", "name")
	err := tgt.RemoveService(&svc)
	_, statErr := os.Stat(testPkgDir)

	assert.Nil(t, err)
	assert.Equal(t, true, os.IsNotExist(statErr))
}

func TestRemoveServiceSupUnloadFail(t *testing.T) {
	tgt, mockExec, finish := setupTestLocalTarget(t)
	defer finish(t)

	mockExec.Expect("CombinedOutput", expectHabCommand("hab", "svc", "unload", "origin/name")).Return("some error output", errors.New("command failed")).Once()
	svc := habpkg.New("origin", "name")

	err := tgt.RemoveService(&svc)
	assert.Error(t, err)
}

func TestRemoveServicePkgDeleteFails(t *testing.T) {
	if syscall.Geteuid() == 0 {
		t.Skip("Test running as EUID 0, skipping for now as this is an uncalled function anyway")
	}
	tgt, mockExec, finish := setupTestLocalTarget(t)
	defer finish(t)

	tempDir, _ := ioutil.TempDir("", "TestRemoveService")
	tgt.HabBaseDir = tempDir

	defer os.RemoveAll(tempDir)

	testPkgDir := filepath.Join(tempDir, "pkgs", "origin", "name")
	os.MkdirAll(testPkgDir, 0400)

	mockExec.Expect("CombinedOutput", expectHabCommand("hab", "svc", "unload", "origin/name")).Return("", nil).Once()

	svc := habpkg.New("origin", "name")
	err := tgt.RemoveService(&svc)
	assert.Error(t, err)
}

func TestRemoveServiceBadIdent(t *testing.T) {
	pkg := habpkg.New("", "")
	tgt := NewLocalTarget(false)
	err := tgt.RemoveService(&pkg)
	assert.Error(t, err)
	assert.Equal(t, err.Error(), "cannot remove service with invalid identifier /")
}

func TestBindsLValues(t *testing.T) {
	//TODO(jaym) this is temporary until we create a Bind type
	t.Run("nil input returns an empty array", func(t *testing.T) {
		assert.Equal(t, []string{}, bindsLValues(nil))
	})

	t.Run("No service group", func(t *testing.T) {
		assert.Equal(t, []string{"foo"}, bindsLValues([]string{"foo"}))
	})

	t.Run("Has service group", func(t *testing.T) {
		assert.Equal(t, []string{"bar"}, bindsLValues([]string{"bar:baz"}))
	})

	t.Run("Has empty service group", func(t *testing.T) {
		assert.Equal(t, []string{"baz"}, bindsLValues([]string{"baz:"}))
	})
}

func TestGetUserToml(t *testing.T) {
	pkg := habpkg.New("bar", "foo")

	t.Run("returns an empty string if the file does not exist", func(t *testing.T) {
		tempDir, _ := ioutil.TempDir("", "TestServiceConfigureFails")
		defer os.Remove(tempDir)

		h := NewLocalTarget(false)
		h.HabBaseDir = tempDir
		userToml, err := h.GetUserToml(&pkg)
		assert.NoError(t, err)
		assert.Empty(t, userToml)
	})

	t.Run("returns the user toml if it exists", func(t *testing.T) {
		tempDir, _ := ioutil.TempDir("", "TestServiceConfigureFails")
		defer os.Remove(tempDir)

		h := NewLocalTarget(false)
		h.HabBaseDir = tempDir

		config := "config"
		err := h.SetUserToml(pkg.Name(), config)
		assert.Nil(t, err)

		userToml, err := h.GetUserToml(&pkg)
		assert.Nil(t, err)
		assert.Equal(t, config, userToml)
	})
}
