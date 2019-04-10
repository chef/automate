package certauthority_test

import (
	"fmt"
	"io/ioutil"
	"net"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/certauthority"
)

var testRootCert string

func init() {
	testRootBytes, err := ioutil.ReadFile("../../testdata/certificates/root.crt")
	if err != nil {
		panic(fmt.Sprintf("Cannot read test root certificate!: %s", err.Error()))
	}
	testRootCert = string(testRootBytes)
}

type TestCertBackend struct {
	Initialized      bool
	InitializeError  error
	ServiceCertError error
	ServiceCertData  *certauthority.ServiceCertData
}

func (b *TestCertBackend) Init() (string, error) {
	b.Initialized = true
	return testRootCert, nil
}

func (b *TestCertBackend) ReInit() (string, error) {
	return b.Init()
}

func (b *TestCertBackend) IsInitialized() (bool, error) {
	return b.Initialized, b.InitializeError
}

func (b *TestCertBackend) CertForService(certauthority.CertRequest) (*certauthority.ServiceCertData, error) {
	if b.ServiceCertData == nil {
		return &certauthority.ServiceCertData{
			Key:  "test key",
			Cert: "test cert",
		}, b.ServiceCertError
	}

	return b.ServiceCertData, b.ServiceCertError
}

func TestInitAuthorityStoresRootCert(t *testing.T) {
	testBackend := &TestCertBackend{}
	a := certauthority.NewCertAuthority(testBackend, "test-id")
	a.InitAuthority()
	assert.Equal(t, testRootCert, a.RootCert())
}

func TestRootCertReturnsTheRootCert(t *testing.T) {
	testBackend := &TestCertBackend{}
	a := certauthority.NewCertAuthority(testBackend, "test-id")
	a.InitAuthority()
	assert.Equal(t, testRootCert, a.RootCert())
}

func TestIsInitializedDelegatesToBackend(t *testing.T) {
	testBackend := &TestCertBackend{}
	a := certauthority.NewCertAuthority(testBackend, "test-id")

	testBackend.Initialized = true
	ret, _ := a.IsInitialized()
	assert.True(t, ret)

	testBackend.Initialized = false
	ret, _ = a.IsInitialized()
	assert.False(t, ret)

	expectedError := errors.New("someting real bad")
	testBackend.InitializeError = expectedError
	_, actualError := a.IsInitialized()
	assert.Equal(t, expectedError, actualError)

}

func TestCertDataForServiceReturnsCertData(t *testing.T) {
	testBackend := &TestCertBackend{}
	a := certauthority.NewCertAuthority(testBackend, "test-id")

	a.InitAuthority()
	certData, _ := a.CertDataForService(testRequest)
	assert.Equal(t, "test key", certData.Key)
	assert.Equal(t, "test cert", certData.Cert, "test cert")
	assert.Equal(t, testRootCert, *certData.RootCert)
}

func TestCertDataForServiceReturnsErrorIfNotInitialized(t *testing.T) {
	testBackend := &TestCertBackend{}
	a := certauthority.NewCertAuthority(testBackend, "test-id")

	_, err := a.CertDataForService(testRequest)
	assert.NotNil(t, err)
}

func TestCertDataForServiceReturnsErrorIfIsInitializedErrors(t *testing.T) {
	testBackend := &TestCertBackend{}
	testBackend.InitializeError = errors.New("test error")
	a := certauthority.NewCertAuthority(testBackend, "test-id")
	_, err := a.CertDataForService(testRequest)
	assert.Error(t, err)
}

func TestCertDataForServiceReturnsErrorIfBackendErrors(t *testing.T) {
	testBackend := &TestCertBackend{}
	testBackend.ServiceCertError = errors.New("test error")

	a := certauthority.NewCertAuthority(testBackend, "test-id")
	a.InitAuthority()
	_, err := a.CertDataForService(testRequest)
	assert.Error(t, err)
}

func TestIsSignedBy(t *testing.T) {
	goodCertBytes, err := ioutil.ReadFile("../../testdata/certificates/good.crt")
	require.NoError(t, err, "reading test data")
	goodCertStr := string(goodCertBytes)

	badCertBytes, err := ioutil.ReadFile("../../testdata/certificates/bad.crt")
	require.NoError(t, err, "reading test data")
	badCertStr := string(badCertBytes)

	testBackend := &TestCertBackend{}
	ca := certauthority.NewCertAuthority(testBackend, "test-id")
	ca.InitAuthority()

	t.Run("returns true if the given certificate was signed by the CA", func(t *testing.T) {
		goodCert, err := certauthority.PEMToCert(goodCertStr)
		require.NoError(t, err)

		assert.True(t, ca.IsSignedBy(goodCert))
	})

	t.Run("returns false if the given certificate was not signed by the CA", func(t *testing.T) {
		badCert, err := certauthority.PEMToCert(badCertStr)
		require.NoError(t, err)

		assert.False(t, ca.IsSignedBy(badCert))
	})
}

func TestValidateCertificateForRequest(t *testing.T) {
	goodCertBytes, err := ioutil.ReadFile("../../testdata/certificates/good.crt")
	require.NoError(t, err, "reading test data")
	goodCertStr := string(goodCertBytes)

	badCertBytes, err := ioutil.ReadFile("../../testdata/certificates/bad.crt")
	require.NoError(t, err, "reading test data")
	badCertStr := string(badCertBytes)

	goodCert, err := certauthority.PEMToCert(goodCertStr)
	require.NoError(t, err)
	badCert, err := certauthority.PEMToCert(badCertStr)
	require.NoError(t, err)

	expectedIPs := []net.IP{net.IPv4(127, 0, 0, 1)}
	expectedHostnames := []string{}
	goodReq := certauthority.NewCertRequest("deployment-service", expectedIPs, expectedHostnames)

	testBackend := &TestCertBackend{}

	ca := certauthority.NewCertAuthority(testBackend, "test-id")
	ca.InitAuthority()

	t.Run("returns nil if everything is cool in the pool", func(t *testing.T) {
		err := ca.ValidateCertificateForRequest(goodCert, goodReq)
		require.NoError(t, err)
	})

	t.Run("returns nil even if the user has added the service-name to DNSNames already", func(t *testing.T) {
		req := certauthority.NewCertRequest("deployment-service", expectedIPs, []string{"deployment-service"})
		err := ca.ValidateCertificateForRequest(goodCert, req)
		require.NoError(t, err)
	})

	t.Run("returns a NotSignedByCA err if the given certificate was not signed by the CA", func(t *testing.T) {
		err := ca.ValidateCertificateForRequest(badCert, goodReq)
		assert.Equal(t, certauthority.NotSignedByCA, err)
	})

	t.Run("returns a CommonNameMismatch err if the given certificate has wrong common name", func(t *testing.T) {
		req := certauthority.NewCertRequest("some-service", expectedIPs, expectedHostnames)
		err := ca.ValidateCertificateForRequest(goodCert, req)
		require.Error(t, err)
		_, ok := err.(*certauthority.CommonNameMismatch)
		assert.True(t, ok, "error should be a CommonNameMismatch")
	})

	t.Run("returns a SANIPAddrMismatch err if the given certificate has wrong IP Addr SAN entries", func(t *testing.T) {
		req := certauthority.NewCertRequest("deployment-service", []net.IP{net.IPv4(172, 0, 2, 2)}, expectedHostnames)
		err := ca.ValidateCertificateForRequest(goodCert, req)
		require.Error(t, err)
		_, ok := err.(*certauthority.SANIPAddrMismatch)
		assert.True(t, ok, "error should be a SANIPAddrMismatch")
	})

	t.Run("returns a SANIPAddrMismatch err if the given certificate has extra IP Addr SAN entries", func(t *testing.T) {
		req := certauthority.NewCertRequest("deployment-service", []net.IP{}, expectedHostnames)
		err := ca.ValidateCertificateForRequest(goodCert, req)
		require.Error(t, err)
		_, ok := err.(*certauthority.SANIPAddrMismatch)
		assert.True(t, ok, "error should be a SANIPAddrMismatch")
	})

	t.Run("returns a SANIPAddrMismatch err if the given certificate has too few IP Addr SAN entries", func(t *testing.T) {
		req := certauthority.NewCertRequest("deployment-service", append(expectedIPs, net.IPv4(172, 0, 2, 2)), expectedHostnames)
		err := ca.ValidateCertificateForRequest(goodCert, req)
		require.Error(t, err)
		_, ok := err.(*certauthority.SANIPAddrMismatch)
		assert.True(t, ok, "error should be a SANIPAddrMismatch")
	})

	t.Run("returns a SANHostnameMismatch err if the given certificate has too few DNSName SAN entries", func(t *testing.T) {
		// NOTE: deployment-service is an implicit DNSName entry
		req := certauthority.NewCertRequest("deployment-service", expectedIPs, []string{"cool.domain"})
		err := ca.ValidateCertificateForRequest(goodCert, req)
		require.Error(t, err)
		_, ok := err.(*certauthority.SANHostnameMismatch)
		assert.True(t, ok, "error should be a SANHostnameMismatch")
	})
}
