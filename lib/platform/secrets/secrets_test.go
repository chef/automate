package secrets_test

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"syscall"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/secrets"
)

func TestSecretNameFromString(t *testing.T) {
	t.Run("it parses chef_secrets style secrets specs", func(t *testing.T) {
		name, err := secrets.SecretNameFromString("group.secret_name")
		require.NoError(t, err)
		assert.Equal(t, secrets.SecretName{"group", "secret_name"}, name)
	})

	t.Run("it fails if too many parts are given", func(t *testing.T) {
		_, err := secrets.SecretNameFromString("group.secret_name.out")
		require.Error(t, err)
	})

	t.Run("it fails if too few parts are given", func(t *testing.T) {
		_, err := secrets.SecretNameFromString("secret_name")
		require.Error(t, err)
	})
}

func TestDiskSecretStore(t *testing.T) {
	tmpDir, err := ioutil.TempDir("", "secret-store-tests")
	defer os.RemoveAll(tmpDir)

	require.NoError(t, err, "failed to create temporary directory")
	uid := os.Getuid()
	gid := os.Getgid()
	require.NoError(t, err, "failed to look up running user")
	dataDir := filepath.Join(tmpDir, "data-dir")

	dataStore := secrets.NewDiskStore(dataDir, uid, gid)
	err = dataStore.Initialize()
	require.NoError(t, err, "failed to initialize")

	t.Run("init creates the data directory with mode 0700", func(t *testing.T) {
		exists, _ := fileutils.PathExists(dataDir)
		assert.True(t, exists)

		stat, err := os.Stat(dataDir)
		require.NoError(t, err)
		assert.Equal(t, os.FileMode(os.ModeDir|0700), stat.Mode())
	})

	t.Run("set creates a secret with correct permissions", func(t *testing.T) {
		testSecretName := secrets.SecretName{"test", "test-secret-1"}
		testSecretContent := []byte("this-is-a-secret")
		err := dataStore.SetSecret(testSecretName, testSecretContent)
		require.NoError(t, err)

		stat, err := os.Stat(filepath.Join(dataDir, "test", "test-secret-1"))
		require.NoError(t, err)
		assert.Equal(t, os.FileMode(0700), stat.Mode())
		assert.Equal(t, uint32(uid), stat.Sys().(*syscall.Stat_t).Uid)

	})

	t.Run("get and set can roundtrip a secret", func(t *testing.T) {
		testSecretName := secrets.SecretName{"test", "test-secret-1"}
		testSecretContent := []byte("this-is-a-secret")
		err := dataStore.SetSecret(testSecretName, testSecretContent)
		require.NoError(t, err)

		secretContent, err := dataStore.GetSecret(testSecretName)
		require.NoError(t, err)
		assert.Equal(t, testSecretContent, secretContent)
	})

	t.Run("Exists returns true if the secret exists", func(t *testing.T) {
		testSecretName := secrets.SecretName{"test", "test-secret-2"}
		testDNESecretName := secrets.SecretName{"test", "nothing-to-see-here"}
		testSecretContent := []byte("this-is-a-secret")
		err := dataStore.SetSecret(testSecretName, testSecretContent)
		require.NoError(t, err)

		exists, err := dataStore.Exists(testSecretName)
		require.NoError(t, err)
		assert.True(t, exists)

		exists, err = dataStore.Exists(testDNESecretName)
		require.NoError(t, err)
		assert.False(t, exists)
	})
}
