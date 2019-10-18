package preflight_test

import (
	"fmt"
	"os"
	"os/user"
	"strings"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/preflight"
	"github.com/chef/automate/lib/proc"
)

type fileOrError struct {
	data []byte
	err  error
}

type availDiskOrError struct {
	avail uint64
	err   error
}

type pathOrError struct {
	path string
	err  error
}

type userOrError struct {
	user *user.User
	err  error
}

type symlinkOrError struct {
	isSymlink bool
	err       error
}

type mockTestProbe struct {
	t                *testing.T
	files            map[string]fileOrError
	symlinks         map[string]symlinkOrError
	availableDisk    map[string]availDiskOrError
	lookupPath       map[string]pathOrError
	lookupUser       map[string]userOrError
	httpConnectivity map[string]bool
	euid             int

	successes []string
	failures  []string
	summaries []string
}

func NewMockTestProbe(t *testing.T) *mockTestProbe {
	return &mockTestProbe{
		t:                t,
		files:            make(map[string]fileOrError),
		availableDisk:    make(map[string]availDiskOrError),
		lookupPath:       make(map[string]pathOrError),
		lookupUser:       make(map[string]userOrError),
		httpConnectivity: make(map[string]bool),
		symlinks:         make(map[string]symlinkOrError),
		euid:             -1,
	}
}
func (m *mockTestProbe) ReportSuccess(s string) { m.successes = append(m.successes, s) }
func (m *mockTestProbe) ReportFailure(s string) { m.failures = append(m.failures, s) }
func (m *mockTestProbe) ReportSummary(s string) { m.summaries = append(m.summaries, s) }

func (m *mockTestProbe) File(path string) ([]byte, error) {
	d, exists := m.files[path]
	if !exists {
		require.FailNow(m.t, fmt.Sprintf("received unexpected call testProbe.File(\"%s\")", path))
	}
	return d.data, d.err
}

func (m *mockTestProbe) IsSymlink(path string) (bool, error) {
	if symlink, exists := m.symlinks[path]; exists {
		return symlink.isSymlink, symlink.err
	}
	if file, exists := m.files[path]; exists {
		if file.err == nil {
			return false, nil
		}
		return false, file.err
	}
	require.FailNow(m.t, fmt.Sprintf("received unexpected call to testProbe.IsSymlink(\"%s\")", path))
	return false, nil
}

func (m *mockTestProbe) Euid() int { return m.euid }
func (m *mockTestProbe) AvailableDiskSpace(path string) (uint64, error) {
	d, exists := m.availableDisk[path]
	if !exists {
		require.FailNow(m.t, fmt.Sprintf("received unexpected call to testProbe.AvailableDiskSpace(\"%s\")", path))
	}
	return d.avail, d.err
}

func (m *mockTestProbe) LookPath(file string) (string, error) {
	d, exists := m.lookupPath[file]
	if !exists {
		require.FailNow(m.t, fmt.Sprintf("received unexpected call to testProbe.LookPath(\"%s\")", file))
	}
	return d.path, d.err
}

func (m *mockTestProbe) LookupUser(username string) (*user.User, error) {
	d, exists := m.lookupUser[username]
	if !exists {
		require.FailNow(m.t, fmt.Sprintf("received unexpected call to testProbe.LookupUser(\"%s\")", username))
	}
	return d.user, d.err
}

func (m *mockTestProbe) HTTPConnectivity(url string) error {
	connected, exists := m.httpConnectivity[url]
	if !exists {
		require.FailNow(m.t, fmt.Sprintf("received unexpected call to testProbe.HttpConnectivity(\"%s\")", url))
	}
	if connected {
		return nil
	}
	return errors.New("Cant reach url")
}

func (m *mockTestProbe) WithFileContents(path string, data []byte) {
	m.files[path] = fileOrError{
		data: data,
	}
}

func (m *mockTestProbe) WithFileNotFound(path string) {
	_, err := os.Open("thisfilewillnevereverexist")
	require.Error(m.t, err)
	m.files[path] = fileOrError{
		err: err,
	}
	m.symlinks[path] = symlinkOrError{
		err: err,
	}
}

func (m *mockTestProbe) WithCommandFound(cmd string) {
	m.lookupPath[cmd] = pathOrError{
		path: cmd,
	}
}

func (m *mockTestProbe) WithCommandNotFound(cmd string) {
	m.lookupPath[cmd] = pathOrError{
		err: errors.New("command not found"),
	}
}

func (m *mockTestProbe) WithUserFound(username string) {
	m.lookupUser[username] = userOrError{
		user: &user.User{
			Uid:      "1000",
			Gid:      "1000",
			Username: username,
			Name:     username,
			HomeDir:  "",
		},
	}
}

func (m *mockTestProbe) WithUserNotFound(username string) {
	m.lookupUser[username] = userOrError{
		err: user.UnknownUserError(username),
	}
}

func (m *mockTestProbe) WithAvailableDiskSpace(path string, avail uint64) {
	m.availableDisk[path] = availDiskOrError{
		avail: avail,
	}
}

func (m *mockTestProbe) WithoutMount(path string) {
	m.availableDisk[path] = availDiskOrError{
		err: errors.New("mount not found"),
	}
}

func (m *mockTestProbe) ConnectedTo(url string) {
	m.httpConnectivity[url] = true
}

func (m *mockTestProbe) ConnectionSevered(url string) {
	m.httpConnectivity[url] = false
}

func (m *mockTestProbe) WithEuid(euid int) {
	m.euid = euid
}

func (m *mockTestProbe) WithSymlink(path string) {
	m.symlinks[path] = symlinkOrError{isSymlink: true}
}

func (m *mockTestProbe) AssertSuccess() {
	assert.Len(m.t, m.failures, 0)
	assert.True(m.t, len(m.successes) > 0, "success not reported")
}

func (m *mockTestProbe) AssertFailure() {
	assert.Len(m.t, m.successes, 0)
	assert.True(m.t, len(m.failures) > 0, "failures not reported")
}

func (m *mockTestProbe) AssertSummary() {
	assert.True(m.t, len(m.summaries) > 0, "summaries not reported")
}

func TestKernelVersionCheck(t *testing.T) {
	check := preflight.KernelVersionCheck(proc.KernelVersion{
		Major: 3,
		Minor: 2,
	})

	t.Run("succeeds if major version is greater than", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/sys/kernel/osrelease", []byte("4.18.16.a-1-foo\n"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})

	t.Run("succeeds if major version is equal minor version is greater than", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/sys/kernel/osrelease", []byte("3.10.0-892.14.4.el7.x86_64\n"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})

	t.Run("succeeds if major version is equal minor version is equal", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/sys/kernel/osrelease", []byte("3.2.0-892.14.4.el7.x86_64\n"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})

	t.Run("fails if major version is equal minor version is less than", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/sys/kernel/osrelease", []byte("3.1.4-892.14.4.el7.x86_64\n"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertFailure()
	})

	t.Run("errors if not parsable", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/sys/kernel/osrelease", []byte("a.b.c-d\n"))
		err := check.TestFunc(probe)
		require.Error(t, err)
	})
}

func TestMinimumMemoryCheck(t *testing.T) {
	minimumRamKiloByte := 2837488
	check := preflight.MinimumRAMCheck(minimumRamKiloByte)
	t.Run("passes when there are gobs of memory", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/meminfo", []byte(`
MemTotal:       32837488 kB
MemFree:        16203960 kB
MemAvailable:   26506912 kB
Buffers:         1135164 kB
Cached:          9366264 kB`))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})

	t.Run("passes when there are minimum available", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/meminfo", []byte(fmt.Sprintf(`
MemTotal:       %d kB`, minimumRamKiloByte)))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})

	t.Run("fails when there are less than minimum available", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/meminfo", []byte(fmt.Sprintf(`
MemTotal:       %d kB`, minimumRamKiloByte-1)))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertFailure()
	})

	t.Run("errors when MemTotal is not reported", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/meminfo", []byte(`
Foo:       32837488 kB`))
		err := check.TestFunc(probe)
		require.Error(t, err)
	})
}

func TestA2DeployedCheck(t *testing.T) {
	t.Run("when no port check is provided", func(t *testing.T) {
		check := preflight.IsA2DeployedCheck(nil)

		t.Run("fails if the deployment-service user.toml is on disk", func(t *testing.T) {
			probe := NewMockTestProbe(t)
			probe.WithFileContents("/hab/user/deployment-service/config/user.toml", []byte(""))
			err := check.TestFunc(probe)
			require.NoError(t, err)
			probe.AssertFailure()
			probe.AssertSummary()
		})

		t.Run("succeeds if the deployment-service user.toml is not on disk", func(t *testing.T) {
			probe := NewMockTestProbe(t)
			probe.WithFileNotFound("/hab/user/deployment-service/config/user.toml")
			err := check.TestFunc(probe)
			require.NoError(t, err)
			probe.AssertSuccess()
		})
	})

	t.Run("when a port check is provided", func(t *testing.T) {
		t.Run("does not run if a2 is already deployed", func(t *testing.T) {
			check := preflight.IsA2DeployedCheck(&preflight.Check{
				TestFunc: func(preflight.TestProbe) error {
					assert.Fail(t, "port check should not get called if a2 is already deployed")
					return nil
				},
			})

			probe := NewMockTestProbe(t)
			probe.WithFileContents("/hab/user/deployment-service/config/user.toml", []byte(""))
			err := check.TestFunc(probe)
			require.NoError(t, err)
			probe.AssertFailure()
			probe.AssertSummary()
		})

		t.Run("succeeds if the deployment-service user.toml is not on disk", func(t *testing.T) {
			called := false
			check := preflight.IsA2DeployedCheck(&preflight.Check{
				TestFunc: func(preflight.TestProbe) error {
					called = true
					return nil
				},
			})
			probe := NewMockTestProbe(t)
			probe.WithFileNotFound("/hab/user/deployment-service/config/user.toml")
			err := check.TestFunc(probe)
			require.NoError(t, err)
			probe.AssertSuccess()
			assert.True(t, called, "expected port check to be called")
		})
	})

}

func TestCLIInBinCheck(t *testing.T) {
	check := preflight.CLIInBin()

	t.Run("succeeds if /bin/chef-automate does not exist", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileNotFound("/bin/chef-automate")
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})

	t.Run("succeeds if /bin/chef-automate is a symlink", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithSymlink("/bin/chef-automate")
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})

	t.Run("fails if /bin/chef-automate is not a symlink", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/bin/chef-automate", []byte(""))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertFailure()
		probe.AssertSummary()
	})

}

func TestUseraddCheck(t *testing.T) {
	check := preflight.HasUseraddCheck()

	t.Run("check has correct name", func(t *testing.T) {
		assert.Equal(t, "has_cmd_useradd", check.Name)
	})

	t.Run("fails if useradd is not found", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithCommandNotFound("useradd")
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertFailure()
	})

	t.Run("succeeds if useradd is found", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithCommandFound("useradd")
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})
}

func TestNobodyCheck(t *testing.T) {
	check := preflight.HasNobodyCheck()

	t.Run("check has correct name", func(t *testing.T) {
		assert.Equal(t, "has_user_nobody", check.Name)
	})

	t.Run("fails if nobody is not found", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithUserNotFound("nobody")
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertFailure()
	})

	t.Run("succeeds if nobody is found", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithUserFound("nobody")
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})
}

func TestIsSystemdCheck(t *testing.T) {
	check := preflight.IsSystemdCheck()

	t.Run("fails if /proc/1/comm does not start with systemd", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/1/comm", []byte("not systemd"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertFailure()
	})

	t.Run("succeeds if /proc/1/comm does not start with systemd", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/1/comm", []byte("systemd\n"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})
}

func TestMinimumDiskCheck(t *testing.T) {
	minimumDisk := uint64(1 << 30)
	check := preflight.MinimumDiskCheck(minimumDisk)

	t.Run("when the hab mount is available", func(t *testing.T) {
		t.Run("fails if there is not enough disk space availabe", func(t *testing.T) {
			probe := NewMockTestProbe(t)
			probe.WithAvailableDiskSpace("/hab", minimumDisk-1)
			err := check.TestFunc(probe)
			require.NoError(t, err)
			probe.AssertFailure()
		})
		t.Run("succeeds if there is enough disk space availabe", func(t *testing.T) {
			probe := NewMockTestProbe(t)
			probe.WithAvailableDiskSpace("/hab", minimumDisk)
			err := check.TestFunc(probe)
			require.NoError(t, err)
			probe.AssertSuccess()
		})
	})

	t.Run("when the hab mount is not available", func(t *testing.T) {
		t.Run("fails if / does not have enough disk space", func(t *testing.T) {
			probe := NewMockTestProbe(t)
			probe.WithoutMount("/hab")
			probe.WithAvailableDiskSpace("/", minimumDisk-1)
			err := check.TestFunc(probe)
			require.NoError(t, err)
			probe.AssertFailure()
		})
		t.Run("succeeds if / has enough disk space", func(t *testing.T) {
			probe := NewMockTestProbe(t)
			probe.WithoutMount("/hab")
			probe.WithAvailableDiskSpace("/", minimumDisk)
			err := check.TestFunc(probe)
			require.NoError(t, err)
			probe.AssertSuccess()
		})
	})

}

func TestRootUserRequiredCheck(t *testing.T) {
	check := preflight.RootUserRequiredCheck()

	t.Run("fails if not running as root", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithEuid(1000)
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertFailure()
	})

	t.Run("succeeds if running as root", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.WithEuid(0)
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})
}

func TestSysctlCheck(t *testing.T) {
	setupTestProbe := func(t *testing.T) *mockTestProbe {
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/sys/fs/file-max", []byte("64000\n"))
		probe.WithFileContents("/proc/sys/vm/max_map_count", []byte("262144\n"))
		probe.WithFileContents("/proc/sys/vm/dirty_ratio", []byte("30\n"))
		probe.WithFileContents("/proc/sys/vm/dirty_background_ratio", []byte("35\n"))
		probe.WithFileContents("/proc/sys/vm/dirty_expire_centisecs", []byte("30000\n"))

		return probe
	}
	check := preflight.DefaultSysctlCheck()

	t.Run("succeeds when all is good", func(t *testing.T) {
		probe := setupTestProbe(t)
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
		assert.Len(t, probe.summaries, 0, "there should be no summaries")
	})

	t.Run("fs.file-max with large value", func(t *testing.T) {
		probe := setupTestProbe(t)
		probe.WithFileContents("/proc/sys/fs/file-max", []byte("3253750"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
		assert.Len(t, probe.summaries, 0, "there should be no summaries")
	})

	t.Run("fails when fs.file-max is below 64000", func(t *testing.T) {
		probe := setupTestProbe(t)
		probe.WithFileContents("/proc/sys/fs/file-max", []byte("63999\n"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		assert.Len(t, probe.failures, 1, "there should be one failure")
		assert.True(t, len(probe.successes) > 0)
		probe.AssertSummary()
	})

	t.Run("fails when vm.max_map_count is below 262144", func(t *testing.T) {
		probe := setupTestProbe(t)
		probe.WithFileContents("/proc/sys/vm/max_map_count", []byte("262143"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		assert.Len(t, probe.failures, 1, "there should be one failure")
		assert.True(t, len(probe.successes) > 0)
		probe.AssertSummary()
	})

	t.Run("fails when vm.dirty_ratio is below 4", func(t *testing.T) {
		probe := setupTestProbe(t)
		probe.WithFileContents("/proc/sys/vm/dirty_ratio", []byte("4"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		assert.Len(t, probe.failures, 1, "there should be one failure")
		assert.True(t, len(probe.successes) > 0)
		probe.AssertSummary()
	})

	t.Run("fails when vm.dirty_ratio is above 30", func(t *testing.T) {
		probe := setupTestProbe(t)
		probe.WithFileContents("/proc/sys/vm/dirty_ratio", []byte("31"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		assert.Len(t, probe.failures, 1, "there should be one failure")
		assert.True(t, len(probe.successes) > 0)
		probe.AssertSummary()
	})

	t.Run("fails when vm.dirty_ratio is below 10", func(t *testing.T) {
		probe := setupTestProbe(t)
		probe.WithFileContents("/proc/sys/vm/dirty_background_ratio", []byte("9"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		assert.Len(t, probe.failures, 1, "there should be one failure")
		assert.True(t, len(probe.successes) > 0)
		probe.AssertSummary()
	})

	t.Run("fails when vm.dirty_background_ratio is above 60", func(t *testing.T) {
		probe := setupTestProbe(t)
		probe.WithFileContents("/proc/sys/vm/dirty_background_ratio", []byte("61"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		assert.Len(t, probe.failures, 1, "there should be one failure")
		assert.True(t, len(probe.successes) > 0)
		probe.AssertSummary()
	})

	t.Run("fails when vm.dirty_expire_centisecs is below 10000", func(t *testing.T) {
		probe := setupTestProbe(t)
		probe.WithFileContents("/proc/sys/vm/dirty_expire_centisecs", []byte("9999"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		assert.Len(t, probe.failures, 1, "there should be one failure")
		assert.True(t, len(probe.successes) > 0)
		probe.AssertSummary()
	})

	t.Run("fails when vm.dirty_expire_centisecs is above 30000", func(t *testing.T) {
		probe := setupTestProbe(t)
		probe.WithFileContents("/proc/sys/vm/dirty_expire_centisecs", []byte("30001"))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		assert.Len(t, probe.failures, 1, "there should be one failure")
		assert.True(t, len(probe.successes) > 0)
		probe.AssertSummary()
	})
}

func TestConnectivityCheck(t *testing.T) {
	urls := []string{
		"http://example.com",
		"http://example.org",
	}
	check := preflight.ConnectivityCheck(urls)

	t.Run("fails when one of the urls errors", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.ConnectedTo("http://example.com")
		probe.ConnectionSevered("http://example.org")
		err := check.TestFunc(probe)
		require.NoError(t, err)
		assert.Len(t, probe.failures, 1, "there should be one failure")
		assert.Len(t, probe.successes, 1, "there should be one success")
		probe.AssertSummary()
	})

	t.Run("fails when both of the urls errors", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.ConnectionSevered("http://example.com")
		probe.ConnectionSevered("http://example.org")
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertFailure()
		probe.AssertSummary()
	})

	t.Run("succeeds when both of the urls are accessible", func(t *testing.T) {
		probe := NewMockTestProbe(t)
		probe.ConnectedTo("http://example.com")
		probe.ConnectedTo("http://example.org")
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})
}

func TestNetstatCheck(t *testing.T) {
	check := preflight.PortCheck([]int{10141})
	t.Run("succeeds when ports are not used", func(t *testing.T) {
		ipv4Txt := strings.TrimSpace(`
sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
1: 00000000:2797 00000000:0000 0A 00000000:00000000 00:00000000 00000000    42        0 9064307 1 0000000000000000 99 0 0 10 0
2: 7501A8C0:BB08 7501A8C0:2790 01 00000000:00000000 00:00000000 00000000    42        0 10309977 1 0000000000000000 20 3 31 10 -1`)
		ipv6Txt := strings.TrimSpace(`
sl  local_address                         remote_address                        st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
0: 00000000000000000000000001000000:0277 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 12089 1 0000000000000000 99 0 0 10 0
1: 00000000000000000000000000000000:0BB8 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000   988        0 15314 1 0000000000000000 99 0 0 10 0`)
		probe := NewMockTestProbe(t)
		probe.WithFileContents("/proc/net/tcp", []byte(ipv4Txt))
		probe.WithFileContents("/proc/net/tcp6", []byte(ipv6Txt))
		err := check.TestFunc(probe)
		require.NoError(t, err)
		probe.AssertSuccess()
	})

	t.Run("fails port is used", func(t *testing.T) {
		t.Run("when it's in ipv4", func(t *testing.T) {
			ipv4Txt := strings.TrimSpace(`
				sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
				1: 00000000:279D 00000000:0000 0A 00000000:00000000 00:00000000 00000000    42        0 9064307 1 0000000000000000 99 0 0 10 0
				2: 7501A8C0:BB08 7501A8C0:2790 01 00000000:00000000 00:00000000 00000000    42        0 10309977 1 0000000000000000 20 3 31 10 -1`)
			ipv6Txt := strings.TrimSpace(`
				sl  local_address                         remote_address                        st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
				0: 00000000000000000000000001000000:0277 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 12089 1 0000000000000000 99 0 0 10 0
				1: 00000000000000000000000000000000:0BB8 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000   988        0 15314 1 0000000000000000 99 0 0 10 0`)
			probe := NewMockTestProbe(t)
			probe.WithFileContents("/proc/net/tcp", []byte(ipv4Txt))
			probe.WithFileContents("/proc/net/tcp6", []byte(ipv6Txt))
			err := check.TestFunc(probe)
			require.NoError(t, err)
			probe.AssertFailure()
		})

		t.Run("when it's ipv6", func(t *testing.T) {
			ipv4Txt := strings.TrimSpace(`
				sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
				1: 00000000:2797 00000000:0000 0A 00000000:00000000 00:00000000 00000000    42        0 9064307 1 0000000000000000 99 0 0 10 0
				2: 7501A8C0:BB08 7501A8C0:2790 01 00000000:00000000 00:00000000 00000000    42        0 10309977 1 0000000000000000 20 3 31 10 -1`)
			ipv6Txt := strings.TrimSpace(`
				sl  local_address                         remote_address                        st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
				0: 00000000000000000000000001000000:279D 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 12089 1 0000000000000000 99 0 0 10 0
				1: 00000000000000000000000000000000:0BB8 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000   988        0 15314 1 0000000000000000 99 0 0 10 0`)
			probe := NewMockTestProbe(t)
			probe.WithFileContents("/proc/net/tcp", []byte(ipv4Txt))
			probe.WithFileContents("/proc/net/tcp6", []byte(ipv6Txt))
			err := check.TestFunc(probe)
			require.NoError(t, err)
			probe.AssertFailure()
		})
	})
}
