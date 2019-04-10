package test

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/boltdb/bolt"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/lib/tls/test/helpers"
)

type certData struct {
	Key  string
	Cert string
}

var testCertData = map[string]certData{}

func initCerts(t *testing.T, svcName string) {
	if _, ok := testCertData[svcName]; !ok {
		c, err := ioutil.ReadFile(helpers.DevCertPath(svcName))
		require.NoError(t, err)
		k, err := ioutil.ReadFile(helpers.DevKeyPath(svcName))
		require.NoError(t, err)
		testCertData[svcName] = certData{
			Key:  string(k),
			Cert: string(c),
		}
	}
}

// Postgresql returns a sample postgresql service
func Postgresql(t *testing.T) *deployment.Service {
	pkg := habpkg.New("chef", "automate-postgresql")
	initCerts(t, "automate-postgresql")
	return &deployment.Service{
		Installable:     &pkg,
		DeploymentState: deployment.Skip,
		SSLKey:          testCertData["automate-postgresql"].Key,
		SSLCert:         testCertData["automate-postgresql"].Cert,
	}
}

// EsSideCar returns a sample es-sidecar-service
func EsSideCar(t *testing.T) *deployment.Service {
	pkg, _ := habpkg.HartFromPath("results/jaym-es-sidecar-service-1.0.0-20180214165126-x86_64-linux.hart")
	pkg.WithOrigin("jaym")
	pkg.WithName("es-sidecar-service")
	initCerts(t, "es-sidecar-service")
	return &deployment.Service{
		Installable:     &pkg,
		DeploymentState: deployment.Running,
		SSLKey:          testCertData["es-sidecar-service"].Key,
		SSLCert:         testCertData["es-sidecar-service"].Cert,
	}
}

// AutomateElasticsearch returns a sample elasticsearch service
func AutomateElasticsearch(t *testing.T) *deployment.Service {
	pkg := habpkg.New("chef", "automate-elasticsearch")
	return &deployment.Service{
		Installable:     &pkg,
		DeploymentState: deployment.Running,
		SSLKey:          testCertData["automate-elasticsearch"].Key,
		SSLCert:         testCertData["automate-elasticsearch"].Cert,
	}
}

// AutomateLoadBalancer returns a sample automate-lb service
func AutomateLoadBalancer(t *testing.T) *deployment.Service {
	pkg := habpkg.New("chef", "automate-load-balancer")
	initCerts(t, "automate-load-balancer")
	return &deployment.Service{
		Installable:     &pkg,
		DeploymentState: deployment.Removed,
		SSLKey:          testCertData["automate-load-balancer"].Key,
		SSLCert:         testCertData["automate-load-balancer"].Cert,
	}
}

// WithDatabase opens a new boltdb database and calls the provided function
func WithDatabase(t *testing.T, f func(*testing.T, *bolt.DB)) {
	path := Tempfile()
	WithDatabaseFile(t, f, path)
}

// WithDatabaseFile opens an existing boltdb database and calls the provided function
func WithDatabaseFile(t *testing.T, f func(*testing.T, *bolt.DB), path string) {
	database, err := bolt.Open(path, 0600, nil)
	if err != nil {
		t.Fatal(err)
	}
	f(t, database)
	database.Close() // nolint
	os.Remove(path)  // nolint
}

// Tempfile creates a new temporary file
func Tempfile() string {
	f, err := ioutil.TempFile("", "a2-deployment-")
	if err != nil {
		panic(err)
	}
	if err := f.Close(); err != nil {
		panic(err)
	}
	return f.Name()
}
