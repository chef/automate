package a1upgrade

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/platform/command"
)

func TestCheckA1Installed(t *testing.T) {
	r := newMockDeliveryRunning(t)
	s := newMockDeliverySecrets(t)
	w := cli.NewWriter(ioutil.Discard, ioutil.Discard, bytes.NewBuffer(nil))
	origA1VersionManifest := A1VersionManifestPath
	defer func() { A1VersionManifestPath = origA1VersionManifest }()

	t.Run("it returns an error if the A1VersionManifest is not found on disk", func(t *testing.T) {
		A1VersionManifestPath = "/this/path/really/should/not/exist/on/your/computer.txt"
		p := NewPreflightRunner(w, r, s, false, false)
		p.checkA1Installed()
		assert.Error(t, p.err)
	})

	t.Run("it returns an error if the A1VersionManifest is not found on disk", func(t *testing.T) {
		tempfile, err := ioutil.TempFile("", "PreflightTempFile")
		require.NoError(t, err)
		defer func() {
			tempfile.Close()
			os.Remove(tempfile.Name())
		}()
		A1VersionManifestPath = tempfile.Name()
		p := NewPreflightRunner(w, r, s, false, false)
		p.checkA1Installed()
		assert.NoError(t, p.err)
	})
}

func createMockManifest(t *testing.T, version string) func() {
	tempfile, err := ioutil.TempFile("", "PreflightTempFile")
	require.NoError(t, err, "mock manifest tempfile creation")
	content := fmt.Sprintf("automate %s\n\n", version)
	_, err = tempfile.Write([]byte(content))
	require.NoError(t, err, "writing test manifest content")
	err = tempfile.Sync()
	require.NoError(t, err, "syncing test manifest content to disk")
	A1VersionManifestPath = tempfile.Name()
	return func() {
		tempfile.Close()
		os.Remove(tempfile.Name())
	}
}

func TestCheckA1MinimumVersion(t *testing.T) {
	r := newMockDeliveryRunning(t)
	s := newMockDeliverySecrets(t)
	w := cli.NewWriter(ioutil.Discard, ioutil.Discard, bytes.NewBuffer(nil))
	origA1VersionManifest := A1VersionManifestPath
	defer func() { A1VersionManifestPath = origA1VersionManifest }()

	t.Run("it returns an error if the A1VersionManifest cannot be read", func(t *testing.T) {
		A1VersionManifestPath = "/this/path/really/should/not/exist/on/your/computer.txt"
		p := NewPreflightRunner(w, r, s, false, false)
		p.checkA1MinimumVersion()
		assert.Error(t, p.err)
	})

	t.Run("it returns an error if the A1VersionManifest has too old of a version", func(t *testing.T) {
		cleanup1 := createMockManifest(t, "0.19.991")
		defer cleanup1()
		p := NewPreflightRunner(w, r, s, false, false)
		p.checkA1MinimumVersion()
		assert.Error(t, p.err)

		cleanup2 := createMockManifest(t, "1.7.341")
		defer cleanup2()
		p = NewPreflightRunner(w, r, s, false, false)
		p.checkA1MinimumVersion()
		assert.Error(t, p.err)

		cleanup3 := createMockManifest(t, "1.8.0")
		defer cleanup3()
		p = NewPreflightRunner(w, r, s, false, false)
		p.checkA1MinimumVersion()
		assert.Error(t, p.err)
	})

	t.Run("it returns an error for malformed manifests", func(t *testing.T) {
		cleanup1 := createMockManifest(t, "potato")
		defer cleanup1()
		p := NewPreflightRunner(w, r, s, false, false)
		p.checkA1MinimumVersion()
		assert.Error(t, p.err)

		cleanup2 := createMockManifest(t, "potato.2.3")
		defer cleanup2()
		p = NewPreflightRunner(w, r, s, false, false)
		p.checkA1MinimumVersion()
		assert.Error(t, p.err)

		cleanup3 := createMockManifest(t, "1.potato.3")
		defer cleanup3()
		p = NewPreflightRunner(w, r, s, false, false)
		p.checkA1MinimumVersion()
		assert.Error(t, p.err)

		cleanup4 := createMockManifest(t, "1.2.potato")
		defer cleanup4()
		p = NewPreflightRunner(w, r, s, false, false)
		p.checkA1MinimumVersion()
		assert.Error(t, p.err)

		cleanup5 := createMockManifest(t, "")
		defer cleanup5()
		p = NewPreflightRunner(w, r, s, false, false)
		p.checkA1MinimumVersion()
		assert.Error(t, p.err)
	})

	t.Run("it succeeds for a recent enough automate 1", func(t *testing.T) {
		cleanup := createMockManifest(t, "1.8.38")
		defer cleanup()
		p := NewPreflightRunner(w, r, s, false, false)
		p.checkA1MinimumVersion()
		assert.NoError(t, p.err)

		cleanup2 := createMockManifest(t, "1.9.1")
		defer cleanup2()
		p = NewPreflightRunner(w, r, s, false, false)
		p.checkA1MinimumVersion()
		assert.NoError(t, p.err)

		cleanup3 := createMockManifest(t, "2.0.0")
		defer cleanup3()
		p = NewPreflightRunner(w, r, s, false, false)
		p.checkA1MinimumVersion()
		assert.NoError(t, p.err)
	})
}

func TestCheckA1InstallUp(t *testing.T) {
	r := newMockDeliveryRunning(t)
	s := newMockDeliverySecrets(t)
	w := cli.NewWriter(ioutil.Discard, ioutil.Discard, bytes.NewBuffer(nil))

	oldDefaultCommandExecutor := defaultCommandExecutor
	defer func() { defaultCommandExecutor = oldDefaultCommandExecutor }()

	setup := func(t *testing.T) *command.MockExecutor {
		mock := command.NewMockExecutor(t)
		defaultCommandExecutor = mock
		return mock
	}

	t.Run("it succeeds if automate-ctl status succeeds", func(t *testing.T) {
		mock := setup(t)
		mock.Expect("Run", command.ExpectedCommand{
			Cmd:     "automate-ctl",
			Args:    []string{"status"},
			Timeout: automateCtlTimeout}).Return(nil)
		p := NewPreflightRunner(w, r, s, false, false)
		p.checkA1InstallUp()
		assert.NoError(t, p.err)
		mock.AssertAllCalled()
	})

	t.Run("it fails if automate-ctl status fails", func(t *testing.T) {
		mock := setup(t)
		mock.Expect("Run", command.ExpectedCommand{
			Cmd:     "automate-ctl",
			Args:    []string{"status"},
			Timeout: automateCtlTimeout}).Return(errors.New("exit 1"))
		p := NewPreflightRunner(w, r, s, false, false)
		p.checkA1InstallUp()
		assert.Error(t, p.err)
		mock.AssertAllCalled()
	})
}

func TestCheckA1InstallHealthy(t *testing.T) {
	r := newMockDeliveryRunning(t)
	s := newMockDeliverySecrets(t)
	w := cli.NewWriter(ioutil.Discard, ioutil.Discard, bytes.NewBuffer(nil))

	originalA1StatusURL := A1StatusURL
	defer func() { A1StatusURL = originalA1StatusURL }()

	t.Run("it succeeds if the status API returns a 200 response", func(t *testing.T) {
		ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusOK)
		}))
		defer ts.Close()
		A1StatusURL = ts.URL

		p := NewPreflightRunner(w, r, s, false, false)
		p.checkA1InstallHealthy()
		assert.NoError(t, p.err)
	})

	t.Run("it fails if the status API returns a non-200 response", func(t *testing.T) {
		ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(http.StatusInternalServerError)
		}))
		defer ts.Close()
		A1StatusURL = ts.URL

		p := NewPreflightRunner(w, r, s, false, false)
		p.checkA1InstallHealthy()
		assert.Error(t, p.err)
	})
}

func TestCheckExpectedDataPaths(t *testing.T) {
	r := newMockDeliveryRunning(t)
	s := newMockDeliverySecrets(t)
	w := cli.NewWriter(ioutil.Discard, ioutil.Discard, bytes.NewBuffer(nil))

	setup := func(t *testing.T) func() {
		dir1, err := ioutil.TempDir("", "PreflightTempDir")
		require.NoError(t, err)
		dir2, err := ioutil.TempDir("", "PreflightTempDir")
		require.NoError(t, err)
		dir3, err := ioutil.TempDir("", "PreflightTempDir")
		require.NoError(t, err)

		// Setup config to require the 3 tmp dirs. Shut off optional dirs
		r.Delivery.Insights.DataDirectory = dir1
		r.Delivery.Compliance.ProfilesPath = dir2
		r.Delivery.Delivery.GitRepos = dir3
		r.Delivery.Backup.Elasticsearch.Type = "s3"
		r.Delivery.Reaper.ArchiveDestination = "s3"

		return func() {
			os.RemoveAll(dir1)
			os.RemoveAll(dir2)
		}
	}

	t.Run("it doesn't error if all source directories exist", func(t *testing.T) {
		cleanup := setup(t)
		defer cleanup()

		p := NewPreflightRunner(w, r, s, false, false)
		p.checkExpectedDataPaths()
		assert.NoError(t, p.err)
	})

	t.Run("it raises an error if an expected source directory doesn't exist", func(t *testing.T) {
		p := NewPreflightRunner(w, r, s, false, false)
		p.checkExpectedDataPaths()
		assert.Error(t, p.err)
	})

	t.Run("it raises an error if a destination directory is non-empty with no sentinel", func(t *testing.T) {
		cleanup := setup(t)
		defer cleanup()

		dir4, err := ioutil.TempDir("", "PreflightTempDir")
		require.NoError(t, err)
		defer os.RemoveAll(dir4)

		defaultHabBaseDir = dir4
		esDataDir := filepath.Join(defaultHabBaseDir, "automate-elasticsearch", "data")
		err = os.MkdirAll(esDataDir, os.ModePerm)
		require.NoError(t, err)
		err = ioutil.WriteFile(filepath.Join(esDataDir, "testfile"), []byte("test context"), os.ModePerm)
		require.NoError(t, err)

		p := NewPreflightRunner(w, r, s, false, false)
		p.checkExpectedDataPaths()
		assert.Error(t, p.err)
	})

	t.Run("it doesn't raise an error if a destination directory is non-empty with done sentinel", func(t *testing.T) {
		cleanup := setup(t)
		defer cleanup()

		dir4, err := ioutil.TempDir("", "PreflightTempDir")
		require.NoError(t, err)
		defer os.RemoveAll(dir4)

		esDataDir := filepath.Join(dir4, "automate-elasticsearch", "data")
		os.MkdirAll(esDataDir, os.ModePerm)
		defaultHabBaseDir = dir4
		err = os.MkdirAll(esDataDir, os.ModePerm)
		require.NoError(t, err)

		err = ioutil.WriteFile(filepath.Join(esDataDir, "testfile"), []byte("test context"), os.ModePerm)
		require.NoError(t, err)
		err = ioutil.WriteFile(filepath.Join(filepath.Dir(esDataDir), ".data-a1-migration-move-started"), []byte("test context"), os.ModePerm)
		require.NoError(t, err)

		p := NewPreflightRunner(w, r, s, false, false)
		p.checkExpectedDataPaths()
		assert.NoError(t, p.err)
	})
}
