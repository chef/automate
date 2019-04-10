package gateway_test

import (
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"strconv"
	"testing"
	"text/template"

	"github.com/spf13/viper"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-gateway/gateway"
	"github.com/chef/automate/lib/tls/test/helpers"
)

// Test that the service configuration file parses into valid configuration.
func TestConfigFromViper(t *testing.T) {
	cfgTemplate := `
host = "{{.Host}}"
port = {{.Port}}
grpc_port = {{.GRPCPort}}
external_fqdn = "{{.Fqdn}}"

[tls]
  cert_path = "{{.ConfigPath}}/service.crt"
  key_path = "{{.ConfigPath}}/service.key"
  root_ca_path ="{{.ConfigPath}}/root_ca.crt"

[grpc_clients.endpoints]
  [grpc_clients.endpoints.ingest-service]
    target = "{{.IngestTarget}}"
    secure = {{.IngestSecure}}
  [grpc_clients.endpoints.config-mgmt-service]
    target = "{{.CfgMgmtTarget}}"
    secure = {{.CfgMgmtSecure}}
  [grpc_clients.endpoints.license-control-service]
    target = "{{.LicenseTarget}}"
    secure = {{.LicenseSecure}}
`
	tempDir, err := ioutil.TempDir("", "config-from-viper-test")
	require.NoError(t, err, "could not create temporary test directory")
	defer os.RemoveAll(tempDir)

	testConfig := struct {
		Host          string
		Port          int
		GRPCPort      int
		Fqdn          string
		ConfigPath    string
		IngestTarget  string
		IngestSecure  string
		CfgMgmtTarget string
		CfgMgmtSecure string
		LicenseTarget string
		LicenseSecure string
	}{
		Host:          "127.0.0.1",
		Port:          2000,
		GRPCPort:      2001,
		Fqdn:          "https://fqdn",
		ConfigPath:    tempDir,
		IngestTarget:  "192.168.1.1:10101",
		IngestSecure:  "true",
		CfgMgmtTarget: "192.168.1.1:10102",
		CfgMgmtSecure: "true",
		LicenseTarget: "192.168.1.1:10103",
		LicenseSecure: "false",
	}

	tomlPath := filepath.Join(tempDir, "config.toml")
	tmpl := template.Must(template.New("Config").Parse(cfgTemplate))
	var rendered bytes.Buffer
	err = tmpl.Execute(&rendered, testConfig)
	require.NoError(t, err, "Failed to render test config template into string")
	err = ioutil.WriteFile(tomlPath, rendered.Bytes(), 0755)
	require.NoError(t, err, fmt.Sprintf("Failed to create %s", tomlPath))

	testCopyFile(t, helpers.DevCertPath("automate-gateway"), filepath.Join(tempDir, "service.crt"))
	testCopyFile(t, helpers.DevKeyPath("automate-gateway"), filepath.Join(tempDir, "service.key"))
	testCopyFile(t, helpers.DevRootCACert(), filepath.Join(tempDir, "root_ca.crt"))

	viper.SetConfigFile(tomlPath)
	err = viper.ReadInConfig()
	require.NoError(t, err, "Failed to read in test template")
	cfg, err := gateway.ConfigFromViper()

	require.NoError(t, err, "Failed to set config from viper")
	require.NotNil(t, cfg.ServiceCerts.ServiceKeyPair)
	require.NotNil(t, cfg.ServiceCerts.RootCACert)

	assert.Equal(t, testConfig.Host, cfg.Hostname)
	assert.Equal(t, testConfig.Port, cfg.Port)
	assert.Equal(t, testConfig.GRPCPort, cfg.GRPCPort)
	assert.Equal(t, testConfig.Fqdn, cfg.ExternalFqdn)

	assert.Equal(t, testConfig.IngestTarget, cfg.GrpcClients.Endpoints["ingest-service"].Target)
	iSecure, err := strconv.ParseBool(testConfig.IngestSecure)
	require.NoError(t, err, "Failed to parse bool")
	assert.Equal(t, iSecure, cfg.GrpcClients.Endpoints["ingest-service"].Secure)

	assert.Equal(t, testConfig.CfgMgmtTarget, cfg.GrpcClients.Endpoints["config-mgmt-service"].Target)
	cSecure, err := strconv.ParseBool(testConfig.CfgMgmtSecure)
	require.NoError(t, err, "Failed to parse bool")
	assert.Equal(t, cSecure, cfg.GrpcClients.Endpoints["config-mgmt-service"].Secure)

	assert.Equal(t, testConfig.LicenseTarget, cfg.GrpcClients.Endpoints["license-control-service"].Target)
	lSecure, err := strconv.ParseBool(testConfig.LicenseSecure)
	require.NoError(t, err, "Failed to parse bool")
	assert.Equal(t, lSecure, cfg.GrpcClients.Endpoints["license-control-service"].Secure)
}

// ********* helper methods below *********

func testCopyFile(t *testing.T, src string, dst string) {
	t.Helper()
	srcFile, err := os.Open(src)
	require.NoError(t, err, "Failed to read test file")
	defer srcFile.Close()

	destFile, err := os.Create(dst)
	require.NoError(t, err, "Failed to create test file")
	defer destFile.Close()

	_, err = io.Copy(destFile, srcFile)
	require.NoError(t, err, "Failed to write test file")

	err = destFile.Sync()
	require.NoError(t, err, "Failed to write test file")
}
