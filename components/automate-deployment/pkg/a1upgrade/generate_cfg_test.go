package a1upgrade

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"reflect"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestGenerateMigrationOverrideConfig(t *testing.T) {
	tmpDir, cleanup := newTestTempDir(t, "migration-override")
	defer cleanup()

	certPath := newTestTempFile(t, tmpDir, "user.crt", "cert-value")
	keyPath := newTestTempFile(t, tmpDir, "user.key", "key-value")

	mockSSLCerts := func(r *DeliveryRunning) {
		r.Delivery.Delivery.SSLCertificates = map[string]map[string]string{
			"user-fqdn": {
				"crt": certPath,
				"key": keyPath,
			},
		}
	}

	t.Run("Sets the LDAP host configuration for Dex if user defined A1 LDAP configuration exists", func(t *testing.T) {
		r := newMockDeliveryRunning(t)
		s := newMockDeliverySecrets(t)
		mockSSLCerts(r)

		r.Delivery.Delivery.LDAPHosts = []string{"128.16.82.23"}
		r.Delivery.Delivery.LDAPPort = "6789"
		r.Delivery.Delivery.LDAPBindDN = "ldapbind@chef.io"
		r.Delivery.Delivery.LDAPBindDNPassword = "SuperSecure"
		r.Delivery.Delivery.LDAPBaseDN = "dc=admin,dc=chef,dc=io"
		r.Delivery.Delivery.LDAPMail = "mail"
		r.Delivery.Delivery.LDAPLogin = "sAMAccountName"

		config, err := generateMigrationOverrideConfig(r, s)
		require.NoError(t, err)
		assert.Equal(t, "128.16.82.23:6789", config.Dex.V1.Sys.Connectors.Ldap.Host.Value)
	})
	t.Run("Doesn't set the LDAP host configuration for Dex if default A1 LDAP configuration exists", func(t *testing.T) {
		r := newMockDeliveryRunning(t)
		s := newMockDeliverySecrets(t)
		mockSSLCerts(r)

		config, err := generateMigrationOverrideConfig(r, s)
		require.NoError(t, err)
		assert.Nil(t, config.Dex.V1.Sys.Connectors.Ldap)
	})
}

func TestGetFrontendTLSCreds(t *testing.T) {
	t.Run("User provided SSL Cert and Key exist", func(t *testing.T) {
		r := newMockDeliveryRunning(t)
		tmpDir, cleanup := newTestTempDir(t, "user-provided-ssl-cert")
		defer cleanup()

		certPath := newTestTempFile(t, tmpDir, "user.crt", "cert-value")
		keyPath := newTestTempFile(t, tmpDir, "user.key", "key-value")

		r.Delivery.Delivery.SSLCertificates = map[string]map[string]string{
			"user-fqdn": {
				"crt": certPath,
				"key": keyPath,
			},
		}

		frontendTLSCreds, err := getFrontendTLSCreds(r)
		require.NoError(t, err, "failed to generate frontend TLS creds")

		assert.Equal(t, frontendTLSCreds[0].ServerName, "user-fqdn")
		assert.Equal(t, frontendTLSCreds[0].Cert, "cert-value")
		assert.Equal(t, frontendTLSCreds[0].Key, "key-value")
	})
	t.Run("User provided SSL Cert and Key exist and uses file:// uri", func(t *testing.T) {
		r := newMockDeliveryRunning(t)
		tmpDir, cleanup := newTestTempDir(t, "user-provided-ssl-cert")
		defer cleanup()

		certPath := newTestTempFile(t, tmpDir, "user.crt", "cert-value")
		keyPath := newTestTempFile(t, tmpDir, "user.key", "key-value")

		r.Delivery.Delivery.SSLCertificates = map[string]map[string]string{
			"user-fqdn": {
				"crt": fmt.Sprintf("file://%s", certPath),
				"key": fmt.Sprintf("file://%s", keyPath),
			},
		}

		frontendTLSCreds, err := getFrontendTLSCreds(r)
		require.NoError(t, err, "failed to generate frontend TLS creds")

		assert.Equal(t, frontendTLSCreds[0].ServerName, "user-fqdn")
		assert.Equal(t, frontendTLSCreds[0].Cert, "cert-value")
		assert.Equal(t, frontendTLSCreds[0].Key, "key-value")
	})
	t.Run("User provided SSL Cert doesn't exist", func(t *testing.T) {
		r := newMockDeliveryRunning(t)
		tmpDir, cleanup := newTestTempDir(t, "user-provided-ssl-cert")
		defer cleanup()

		certPath := filepath.Join(tmpDir, "user.crt")
		keyPath := newTestTempFile(t, tmpDir, "user.key", "key-value")

		r.Delivery.Delivery.SSLCertificates = map[string]map[string]string{
			"user-fqdn": {
				"crt": certPath,
				"key": keyPath,
			},
		}

		_, err := getFrontendTLSCreds(r)
		require.Error(t, err)
	})
	t.Run("User provided SSL Key doesn't exist", func(t *testing.T) {
		r := newMockDeliveryRunning(t)
		tmpDir, cleanup := newTestTempDir(t, "user-provided-ssl-cert")
		defer cleanup()

		certPath := newTestTempFile(t, tmpDir, "user.crt", "cert-value")
		keyPath := filepath.Join(tmpDir, "user.key")

		r.Delivery.Delivery.SSLCertificates = map[string]map[string]string{
			"user-fqdn": {
				"crt": certPath,
				"key": keyPath,
			},
		}

		_, err := getFrontendTLSCreds(r)
		require.Error(t, err)
	})
	t.Run("Self-signed SSL Cert and Key exist", func(t *testing.T) {
		r := newMockDeliveryRunning(t)
		tmpDir, cleanup := newTestTempDir(t, "self-signed-ssl-cert")
		defer cleanup()
		caDir := filepath.Join(tmpDir, "ca")
		err := os.MkdirAll(caDir, 0755)
		require.NoError(t, err, "Failed to create fake nginx ca dir")

		newTestTempFile(t, caDir, "self-signed.crt", "cert-value")
		newTestTempFile(t, caDir, "self-signed.key", "key-value")

		r.Delivery.FQDN = "self-signed"
		r.Delivery.Nginx.Dir = tmpDir
		assert.Equal(t, r.Delivery.FQDN, "self-signed")

		frontendTLSCreds, err := getFrontendTLSCreds(r)
		require.NoError(t, err, "failed to load self-signed TLS creds")

		assert.Equal(t, frontendTLSCreds[0].ServerName, "self-signed")
		assert.Equal(t, frontendTLSCreds[0].Cert, "cert-value")
		assert.Equal(t, frontendTLSCreds[0].Key, "key-value")
	})
	t.Run("Self-signed SSL Cert doesn't exist", func(t *testing.T) {
		r := newMockDeliveryRunning(t)
		tmpDir, cleanup := newTestTempDir(t, "self-signed-ssl-cert")
		defer cleanup()
		caDir := filepath.Join(tmpDir, "ca")
		err := os.MkdirAll(caDir, 0755)
		require.NoError(t, err, "Failed to create fake nginx ca dir")

		newTestTempFile(t, caDir, "self-signed.key", "user.key")

		r.Delivery.FQDN = "self-signed"
		r.Delivery.Nginx.Dir = tmpDir

		_, err = getFrontendTLSCreds(r)
		require.Error(t, err)
	})
	t.Run("Self-signed SSL Key doesn't exist", func(t *testing.T) {
		r := newMockDeliveryRunning(t)
		tmpDir, cleanup := newTestTempDir(t, "self-signed-ssl-cert")
		defer cleanup()
		caDir := filepath.Join(tmpDir, "ca")
		err := os.MkdirAll(caDir, 0755)
		require.NoError(t, err, "Failed to create fake nginx ca dir")

		newTestTempFile(t, caDir, "self-signed.crt", "cert-value")

		r.Delivery.FQDN = "self-signed"
		r.Delivery.Nginx.Dir = tmpDir

		_, err = getFrontendTLSCreds(r)
		require.Error(t, err)
	})
}

func TestGetLDAPSettings(t *testing.T) {
	t.Run("With default A1 fake LDAP settings", func(t *testing.T) {
		r := newMockDeliveryRunning(t)

		ldapSettings, err := getLDAPSettings(r)

		require.NoError(t, err)
		assert.Nil(t, ldapSettings)
	})
	t.Run("With user provided LDAP settings", func(t *testing.T) {
		r := newMockDeliveryRunning(t)

		r.Delivery.Delivery.LDAPHosts = []string{"128.16.82.23"}
		r.Delivery.Delivery.LDAPPort = "6789"
		r.Delivery.Delivery.LDAPBindDN = "ldapbind@chef.io"
		r.Delivery.Delivery.LDAPBindDNPassword = "SuperSecure"
		r.Delivery.Delivery.LDAPBaseDN = "dc=admin,dc=chef,dc=io"
		r.Delivery.Delivery.LDAPMail = "mail"
		r.Delivery.Delivery.LDAPLogin = "sAMAccountName"

		ldapSettings, err := getLDAPSettings(r)

		require.NoError(t, err)
		assert.Equal(t, w.String("128.16.82.23:6789"), ldapSettings.Host)
		assert.Equal(t, w.String("ldapbind@chef.io"), ldapSettings.BindDn)
		assert.Equal(t, w.String("SuperSecure"), ldapSettings.BindPassword)
		assert.Equal(t, w.String("dc=admin,dc=chef,dc=io"), ldapSettings.BaseUserSearchDn)
		assert.Equal(t, w.String("mail"), ldapSettings.EmailAttr)
		assert.Equal(t, w.String("sAMAccountName"), ldapSettings.UserIdAttr)
		assert.Equal(t, w.String("sAMAccountName"), ldapSettings.UsernameAttr)
	})
}

func TestPostgresqlSettings(t *testing.T) {
	t.Run("With default A1 MD5AuthCIDRAddresses", func(t *testing.T) {
		r := newMockDeliveryRunning(t)

		psqlConfig, err := getPostgresSettings(r)

		require.NoError(t, err)
		assert.Equal(t, "samehost", psqlConfig.Md5AuthCidrAddresses[0])
		assert.Equal(t, 1, len(psqlConfig.Md5AuthCidrAddresses))
	})
	t.Run("With user provided A1 MD5AuthCIDRAddresses", func(t *testing.T) {
		r := newMockDeliveryRunning(t)

		userCIDRs := []string{
			"128.123.132.123/24",
			"123.213.123.123/8",
		}

		expectedCIDRs := append(userCIDRs, "samehost")

		r.Delivery.PostgreSQL.MD5AuthCIDRAddresses = userCIDRs

		psqlConfig, err := getPostgresSettings(r)
		require.NoError(t, err)

		cidrs := psqlConfig.Md5AuthCidrAddresses

		assert.True(t, reflect.DeepEqual(expectedCIDRs, cidrs), "CIDRs must match")
	})
}

func TestProxySettings(t *testing.T) {
	t.Run("With default A1 fake proxy settings", func(t *testing.T) {
		r := newMockDeliveryRunning(t)

		proxyConfig, err := getProxySettings(r)

		require.NoError(t, err)
		assert.Nil(t, proxyConfig)
	})
	t.Run("With user provided proxy settings", func(t *testing.T) {
		r := newMockDeliveryRunning(t)
		noProxy := []string{
			"127.0.0.1",
			"localhost",
			"192.168.0.0",
		}

		r.Delivery.Delivery.Proxy.Host = "proxy.megacorp.com"
		// set as a string to make sure we cast it to an int32 wrapper properly
		r.Delivery.Delivery.Proxy.Port = "3000"
		r.Delivery.Delivery.Proxy.User = "dilbert"
		r.Delivery.Delivery.Proxy.Password = "garfield"
		r.Delivery.Delivery.Proxy.NoProxy = noProxy

		proxyConfig, err := getProxySettings(r)

		require.NoError(t, err)
		assert.Equal(t, w.String("proxy.megacorp.com"), proxyConfig.Host)
		assert.Equal(t, w.Int32(3000), proxyConfig.Port)
		assert.Equal(t, w.String("dilbert"), proxyConfig.User)
		assert.Equal(t, w.String("garfield"), proxyConfig.Password)
		assert.Equal(t, noProxy, proxyConfig.NoProxy)
	})
}

func newMockDeliveryRunning(t *testing.T) *DeliveryRunning {
	config := NewA1Config()
	config.DeliveryRunningPath = fixturePath("delivery-running.json")
	err := config.LoadDeliveryRunning()
	require.NoError(t, err, "failed to load delivery.running.json")

	return config.DeliveryRunning
}

func newMockDeliverySecrets(t *testing.T) *DeliverySecrets {
	config := NewA1Config()
	config.DeliverySecretsPath = fixturePath("delivery-secrets.json")
	err := config.LoadDeliverySecrets()
	require.NoError(t, err, "failed to load delivery-secrets.json")

	return config.DeliverySecrets
}

func newTestTempDir(t *testing.T, msg string) (string, func()) {
	tempdir, err := ioutil.TempDir("", msg)
	require.NoError(t, err, "could not create temporary test directory")
	f := func() { os.RemoveAll(tempdir) }

	return tempdir, f
}

func newTestTempFile(t *testing.T, dirName, name, value string) string {
	fPath := filepath.Join(dirName, name)
	err := ioutil.WriteFile(fPath, []byte(value), 0755)
	require.NoError(t, err, fmt.Sprintf("failed to create %s", fPath))

	return fPath
}
