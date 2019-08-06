package storage_test

import (
	"context"
	"database/sql"
	"io/ioutil"
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/license-control-service/pkg/keys"
	"github.com/chef/automate/components/license-control-service/pkg/storage"
)

const resetDatabaseStatement = `DROP SCHEMA public CASCADE;
CREATE SCHEMA public;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO public;
`

func TestPGBackend(t *testing.T) {
	pgURL := os.Getenv("PG_URL")
	if pgURL == "" {
		t.Fatal("test requires PG_URL to be set")
	}

	skipValidLicenseTests := false
	validLicenseFile := "../../../../dev/license.jwt"
	validLicenseBytes, err := ioutil.ReadFile(validLicenseFile)
	if err != nil {
		if os.IsNotExist(err) {
			t.Log("No license file, skipping tests that require it")
			skipValidLicenseTests = true
		} else {
			t.Fatalf("Unknown error reading valid license file: %s", err.Error())
		}
	}
	validLicenseContent := strings.TrimSpace(string(validLicenseBytes))

	resetDB := func(t *testing.T) {
		db, err := sql.Open("postgres", pgURL)
		require.NoError(t, err, "open db for reset")
		defer db.Close()
		_, err = db.Exec(resetDatabaseStatement)
		require.NoError(t, err, "reset db")
	}
	t.Run("Init should succeed with non-existent legacy migration file", func(t *testing.T) {
		defer resetDB(t)
		backend := storage.NewCurrentBackend(pgURL, "../../migrations", "/definitely/should/not/exist")
		err := backend.Init(context.Background(), keys.NewLicenseParser(keys.BuiltinKeyData))
		require.NoError(t, err)
	})

	t.Run("Init should succeed with existent but corrupt legacy migration file", func(t *testing.T) {
		defer resetDB(t)
		backend := storage.NewCurrentBackend(pgURL, "../../migrations", "/dev/null")
		err := backend.Init(context.Background(), keys.NewLicenseParser(keys.BuiltinKeyData))
		require.NoError(t, err)
	})

	t.Run("Init should migrate legacy migration file", func(t *testing.T) {
		if skipValidLicenseTests {
			t.Skip("Valid license file not present, skipping tests that require it")
		}
		defer resetDB(t)
		file, err := ioutil.TempFile("./", "storage-tests-")
		defer os.Remove(file.Name())
		_, err = file.Write(validLicenseBytes)
		require.NoError(t, err, "write license file")

		backend := storage.NewCurrentBackend(pgURL, "../../migrations", file.Name())
		err = backend.Init(context.Background(), keys.NewLicenseParser(keys.BuiltinKeyData))
		require.NoError(t, err)
		license, _, err := backend.GetLicense(context.Background())
		require.NoError(t, err, "GetLicense")
		assert.Equal(t, validLicenseContent, license)
	})

	t.Run("it should be able to round-trip secrets", func(t *testing.T) {
		defer resetDB(t)
		backend := storage.NewCurrentBackend(pgURL, "../../migrations", "/definitely/should/not/exist")
		err := backend.Init(context.Background(), keys.NewLicenseParser(keys.BuiltinKeyData))
		require.NoError(t, err, "Init")

		err = backend.SetLicense(context.Background(), "test-content")
		require.NoError(t, err, "SetLicense")
		license, _, err := backend.GetLicense(context.Background())
		require.NoError(t, err, "GetLicense")
		assert.Equal(t, "test-content", license)
	})
}

func TestFileBackend(t *testing.T) {
	skipValidLicenseTests := false
	validLicenseFile := "../../../../dev/license.jwt"
	validLicenseBytes, err := ioutil.ReadFile(validLicenseFile)
	if err != nil {
		if os.IsNotExist(err) {
			t.Log("No license file, skipping tests that require it")
			skipValidLicenseTests = true
		} else {
			t.Errorf("Unknown error reading valid license file: %s", err.Error())
		}
	}
	validLicenseContent := strings.TrimSpace(string(validLicenseBytes))

	t.Run("GetLicense returns NoLicenseError if file does not exist", func(t *testing.T) {
		f := storage.NewFileBackend("/this/path/definitely/should/not/exist/i/hope")
		_, err := f.GetLicense(context.Background())
		assert.Error(t, err)
		_, ok := err.(*storage.NoLicenseError)
		assert.True(t, ok, "is a NoLicenseError")
	})
	t.Run("GetLicense returns the contents of the on-disk store if it exists", func(t *testing.T) {
		if skipValidLicenseTests {
			t.Skip("Valid license file not present, skipping tests that require it")
		}
		f := storage.NewFileBackend(validLicenseFile)
		license, err := f.GetLicense(context.Background())
		require.NoError(t, err)
		assert.Equal(t, validLicenseContent, license)
	})
	t.Run("Cleanup removes the on-disk store", func(t *testing.T) {
		file, err := ioutil.TempFile("./", "storage-tests-")
		defer os.Remove(file.Name())
		require.NoError(t, err)
		f := storage.NewFileBackend(file.Name())
		err = f.Cleanup(context.Background())
		require.NoError(t, err)
		_, err = os.Stat(file.Name())
		assert.True(t, os.IsNotExist(err), "backend file should no longer exist")
	})
	t.Run("Cleanup doesn't produce an error if run twice", func(t *testing.T) {
		file, err := ioutil.TempFile("./", "storage-tests-")
		defer os.Remove(file.Name())
		require.NoError(t, err)
		f := storage.NewFileBackend(file.Name())
		err = f.Cleanup(context.Background())
		require.NoError(t, err)
		err = f.Cleanup(context.Background())
		require.NoError(t, err)
	})
}
