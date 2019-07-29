package server_test

import (
	"context"
	"io/ioutil"
	"os"
	"regexp"
	"testing"
	"time"

	log "github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	lc "github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/components/license-control-service/pkg/keys"
	"github.com/chef/automate/components/license-control-service/pkg/server"
	"github.com/chef/automate/components/license-control-service/pkg/storage"
)

var validLicenseData []byte
var skipValidLicenseTests bool

const (
	expiredLicenseFile = "../../testdata/a2-test.license.expires.2018-05-01"
	corruptLicenseFile = "../../testdata/a2-test_corrupt.license"
	validLicenseFile   = "../../../../dev/license.jwt"
)

func testLicenseControlServer(t *testing.T) lc.LicenseControlServer {
	licenseParser := keys.NewLicenseParser(keys.BuiltinKeyData)
	backend := &storage.MemBackend{}
	err := backend.Init(context.Background(), licenseParser)
	require.NoError(t, err)
	return server.NewLicenseControlServer(context.Background(), backend, licenseParser, &server.Config{})
}

func TestMain(m *testing.M) {
	var err error
	validLicenseData, err = ioutil.ReadFile(validLicenseFile)
	if err != nil {
		if os.IsNotExist(err) {
			log.Warn("No license file, skipping tests that require it")
			skipValidLicenseTests = true
		} else {
			log.WithError(err).Fatal("Unable to read license file")
		}
	}
	e := m.Run()
	os.Exit(e)
}

func TestAppliedLicense(t *testing.T) {
	if skipValidLicenseTests {
		t.Skip("Valid license file not present, skipping tests that require it")
	}

	srv := testLicenseControlServer(t)
	srv.Update(context.Background(), &lc.UpdateRequest{LicenseData: string(validLicenseData)})

	t.Run("License() with a valid license applied", func(t *testing.T) {
		res, err := srv.License(
			context.Background(),
			&lc.LicenseRequest{},
		)
		require.NoError(t, err)
		assert.NotNil(t, res.License.Id, "License should include a license ID")
		assert.Equal(t, res.License.CustomerId, "0000000000000000", "License should be valid")
	})
	t.Run("Policy() with a valid license applied", func(t *testing.T) {
		res, err := srv.Policy(
			context.Background(),
			&lc.PolicyRequest{},
		)
		require.NoError(t, err)
		assert.NotNil(t, res.Policy.LicenseId, "Policy should include a LicenseId")
		assert.True(t, res.Policy.Valid, "Policy should be valid")
		assert.NotEqual(t, len(res.Policy.Capabilities), 0, "Policy should include >= 1 Capabilities")
		assert.NotEqual(t, len(res.Policy.Rules), 0, "Policy should include >= 1 Rules")
	})
	t.Run("Status(0 with a valid license applied)", func(t *testing.T) {
		startTime := time.Now()
		licenseIDMatcher := regexp.MustCompile(`^\w{8}-\w{4}-\w{4}-\w{4}-\w{12}$`)

		res, err := srv.Status(
			context.Background(),
			&lc.StatusRequest{},
		)
		require.NoError(t, err)

		assert.True(
			t, licenseIDMatcher.MatchString(res.LicenseId),
			"Status LicenseId is malformed",
		)

		assert.Equal(
			t, "Chef Dev", res.CustomerName,
			"Expected customer name to match fixture",
		)

		// FIXME: neither of the following asserts means anything now because the
		// tests are initialized with a license and it's not updated in this test
		// function. so we aren't actually testing to see if updating the license
		// changes ConfiguredAt.
		assert.True(
			t, (res.ConfiguredAt.Seconds >= startTime.Unix()),
			"Policy should have been reconfigured after we marked the time",
		)
		assert.True(
			t, (res.ConfiguredAt.Seconds <= time.Now().Unix()),
			"Policy should have been reconfigured before we evaluate this test",
		)

		assert.NotEqual(
			t, 0, res.LicensedPeriod.Start.GetSeconds(),
			"Licensed period start date should be non-zero",
		)
		assert.NotEqual(
			t, 0, res.LicensedPeriod.End.GetSeconds(),
			"Licensed period end date should be non-zero",
		)
		assert.True(
			t, (res.LicensedPeriod.Start.GetSeconds() < res.LicensedPeriod.End.GetSeconds()),
			"Licensed period start date should come before end date",
		)
	})
}

func TestUpdateValidLicenseWithWhiteSpace(t *testing.T) {
	if skipValidLicenseTests {
		t.Skip("Valid license file not present, skipping tests that require it")
	}

	srv := testLicenseControlServer(t)
	raw, err := ioutil.ReadFile(validLicenseFile)
	assert.NoError(t, err, "reading test license")
	license := string(raw) + " \n"

	res, err := srv.Update(
		context.Background(),
		&lc.UpdateRequest{
			LicenseData: license,
		},
	)

	require.NoError(t, err, "update error")
	assert.NotNil(t, res)
}

// func TestLoadedLicenseFromDiskNotNil(t *testing.T) {
// 	if skipValidLicenseTests {
// 		t.Skip("Valid license file not present, skipping tests that require it")
// 	}

// 	configWithLicense := cfg
// 	configWithLicense.LicenseTokenPath = validLicenseFile
// 	svr := server.NewLicenseControlServer(context.Background(), &configWithLicense)
// 	res, _ := svr.License(context.Background(), &lc.LicenseRequest{})
// 	assert.NotNil(t, res.License, "LicenseControlServer should load the license on disk")
// }

func TestUpdateInvalidLicense(t *testing.T) {
	srv := testLicenseControlServer(t)
	res, err := srv.Update(
		context.Background(),
		&lc.UpdateRequest{
			LicenseData: "invalid",
		},
	)

	assert.NotNil(t, err, "Error should not be nil when trying to update with invalid license")
	assert.Nil(t, res, "Update should return a nil response with invalid license")
}

func TestDefaultLicenseNil(t *testing.T) {
	srv := testLicenseControlServer(t)
	res, _ := srv.License(context.Background(), &lc.LicenseRequest{})
	assert.Nil(t, res.License, "LicenseControlServer should default to nil license")
}

// func TestLoadedCorruptLicenseFromDiskNil(t *testing.T) {
// 	configWithLicense := cfg
// 	configWithLicense.LicenseTokenPath = corruptLicenseFile
// 	svr := server.NewLicenseControlServer(context.Background(), &configWithLicense)
// 	res, _ := svr.License(context.Background(), &lc.LicenseRequest{})
// 	assert.Nil(t, res.License, "LicenseControlServer should not have loaded the license on disk")
// }

func TestUnforcedUpdateToExpiredLicense(t *testing.T) {
	srv := testLicenseControlServer(t)

	raw, err := ioutil.ReadFile(expiredLicenseFile)
	require.NoError(t, err, "reading test license")
	license := string(raw) + " \n"

	_, err = srv.Update(
		context.Background(),
		&lc.UpdateRequest{
			LicenseData: license,
		},
	)
	require.Error(t, err, "unforced update to an expired license should have failed")
}

func TestForcedUpdateToExpiredLicense(t *testing.T) {
	srv := testLicenseControlServer(t)
	raw, err := ioutil.ReadFile(expiredLicenseFile)
	require.NoError(t, err, "reading test license")
	license := string(raw) + " \n"

	res, err := srv.Update(
		context.Background(),
		&lc.UpdateRequest{
			Force:       true,
			LicenseData: license,
		},
	)

	require.NoError(t, err, "forced update to an expired license should have succeeded")
	assert.True(t, res.Updated, "forced update to an expired license should have succeeded")
}
