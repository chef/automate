package helpers

import (
	"fmt"
	"os"
	"path"
	"path/filepath"
	"runtime"
	"testing"

	"github.com/chef/automate/lib/tls/certs"
)

var devCertDir = "dev/certs"

func init() {
	_, currentFilePath, _, _ := runtime.Caller(0)
	currentDir := path.Dir(currentFilePath)
	dir, err := filepath.Abs(path.Join(currentDir, "..", "..", "..", "..", "dev", "certs"))
	if err != nil {
		fmt.Fprintf(os.Stderr, "WARNING: could not find development certs directory")
	}
	devCertDir = dir
}

func DevCertPath(serviceName string) string {
	return path.Join(devCertDir, fmt.Sprintf("%s.crt", serviceName))
}

func DevKeyPath(serviceName string) string {
	return path.Join(devCertDir, fmt.Sprintf("%s.key", serviceName))
}

func DevRootCACert() string {
	return path.Join(devCertDir, "Chef_Automate_FAKE_Dev.crt")
}

func DevRootCAKey() string {
	return path.Join(devCertDir, "Chef_Automate_FAKE_Dev.key")
}

// LoadDevCerts returns the dev service certificates meant for usage in tests
// and local development
func LoadDevCerts(t *testing.T, name string) *certs.ServiceCerts {
	t.Helper()
	cfg := certs.TLSConfig{
		CertPath:       DevCertPath(name),
		KeyPath:        DevKeyPath(name),
		RootCACertPath: DevRootCACert(),
	}

	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		t.Fatalf("Could not load certs: %v", err)
	}

	return serviceCerts
}
