package certauthority_test

import (
	"net"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/automate-deployment/pkg/certauthority"
	"github.com/chef/automate/lib/platform/command"
)

type TestDiskStoreResult struct {
	Exist   bool
	Content string
	Error   error
}

type TestDiskStore struct {
	OpResults     []TestDiskStoreResult
	ReadHistory   []string
	ExistHistory  []string
	DeleteHistory []string
	opIdx         uint32
}

func (d *TestDiskStore) getNextResult() TestDiskStoreResult {
	res := d.OpResults[d.opIdx]
	d.opIdx++
	return res
}

func (d *TestDiskStore) AddOpResult(c string, b bool, e error) {
	newRes := TestDiskStoreResult{
		Exist:   b,
		Content: c,
		Error:   e,
	}
	d.OpResults = append(d.OpResults, newRes)
}

func (d *TestDiskStore) Exist(path string) (bool, error) {
	d.ExistHistory = append(d.ExistHistory, path)
	res := d.getNextResult()
	return res.Exist, res.Error
}

func (d *TestDiskStore) ReadFile(path string) (string, error) {
	d.ReadHistory = append(d.ReadHistory, path)
	res := d.getNextResult()
	return res.Content, res.Error
}

func (d *TestDiskStore) DeleteFile(path string) error {
	d.DeleteHistory = append(d.DeleteHistory, path)
	res := d.getNextResult()
	return res.Error
}

var testRequest = certauthority.NewCertRequest("test-service", []net.IP{net.IPv4(172, 0, 2, 1)}, []string{})

func basicTestSetup(t *testing.T) (*command.MockExecutor, *TestDiskStore, *certauthority.CertstrapBackend) {
	fileBackend := &TestDiskStore{}
	cmdBackend := command.NewMockExecutor(t)

	crtBackend := certauthority.NewCertstrapBackend("test_data_dir", "test_authority")
	crtBackend.SetFileStore(fileBackend)
	crtBackend.SetCmdExecutor(cmdBackend)
	return cmdBackend, fileBackend, crtBackend
}

func TestInitCallsCertstrap(t *testing.T) {
	cmdBackend, fileBackend, be := basicTestSetup(t)
	cmdBackend.Expect("CombinedOutput", command.ExpectedCommand{
		Cmd: "certstrap",
		Args: []string{
			"--depot-path",
			"test_data_dir",
			"init",
			"--common-name", "test_authority",
			"--passphrase", ""},
	}).Return("test command output", nil)

	fileBackend.AddOpResult("file exists", false, nil)
	fileBackend.AddOpResult("test key content", true, nil)
	be.Init()
	cmdBackend.AssertAllCalled()
}

func TestInitReturnsCertstrapError(t *testing.T) {
	cmdBackend, fileBackend, be := basicTestSetup(t)
	fileBackend.AddOpResult("file exists", false, nil)
	cmdBackend.Expect("CombinedOutput", command.ExpectedCommand{}).Return("fake certstrap output", errors.New("exit status 1")).Times(2)

	_, err := be.Init()
	assert.Equal(t,
		"certstrap init failure: fake certstrap output: exit status 1",
		err.Error())
}

func TestInitReturnsIsInitializedError(t *testing.T) {
	_, fileBackend, be := basicTestSetup(t)
	fileBackend.AddOpResult("file exists", false, errors.New("test error"))

	_, err := be.Init()
	assert.Error(t, err)
}

func TestInitReturnsKeyContent(t *testing.T) {
	cmdBackend, fileBackend, be := basicTestSetup(t)
	cmdBackend.Expect("CombinedOutput", command.ExpectedCommand{}).Return("test command output", nil).Once()
	fileBackend.AddOpResult("file exists", false, nil)
	fileBackend.AddOpResult("test key content", true, nil)

	key, _ := be.Init()
	assert.Equal(t, "test_data_dir/test_authority.crt", fileBackend.ReadHistory[0])
	assert.Equal(t, "test key content", key)
	cmdBackend.AssertAllCalled()
}

func TestInitReturnsFileErrors(t *testing.T) {
	cmdBackend, fileBackend, be := basicTestSetup(t)
	cmdBackend.Expect("CombinedOutput", command.ExpectedCommand{}).Return("test command output", nil).Once()
	fileBackend.AddOpResult("file exists", false, nil)
	fileBackend.AddOpResult("", true, errors.New("test error reason"))

	_, err := be.Init()
	assert.Equal(t,
		"unable to read certificate data: test error reason",
		err.Error())
}

func TestIsInitializedReturnsFileErrors(t *testing.T) {
	_, fileBackend, be := basicTestSetup(t)
	fileBackend.AddOpResult("", true, errors.New("test error reason"))
	_, err := be.IsInitialized()
	assert.Error(t, err)
}

func TestIsInitializedReturnsBool(t *testing.T) {
	_, fileBackend, be := basicTestSetup(t)
	fileBackend.AddOpResult("", true, nil)
	fileBackend.AddOpResult("", false, nil)

	actualResponse1, _ := be.IsInitialized()
	actualResponse2, _ := be.IsInitialized()
	assert.Equal(t, true, actualResponse1)
	assert.Equal(t, false, actualResponse2)
}

// Happy path test
func TestCertForServiceReturnsContent(t *testing.T) {
	cmdBackend, fileBackend, be := basicTestSetup(t)
	// request-cert and sign
	cmdBackend.Expect("CombinedOutput", command.ExpectedCommand{
		Cmd:  "certstrap",
		Args: []string{"--depot-path", "test_data_dir", "request-cert", "--common-name", "test-service", "--passphrase", "", "--ip", "172.0.2.1", "--domain", "test-service"},
	}).Return("test command output", nil)

	cmdBackend.Expect("CombinedOutput", command.ExpectedCommand{
		Cmd:  "certstrap",
		Args: []string{"--depot-path", "test_data_dir", "sign", "--CA", "test_authority", "test-service"},
	}).Return("test command output", nil)

	// Read key and cert
	fileBackend.AddOpResult("test key content", true, nil)
	fileBackend.AddOpResult("test cert content", true, nil)

	// delete key, csr, and cert
	fileBackend.AddOpResult("file deleted", true, nil)
	fileBackend.AddOpResult("file deleted", true, nil)
	fileBackend.AddOpResult("file deleted", true, nil)

	keyData, _ := be.CertForService(testRequest)

	cmdBackend.AssertAllCalled()
	assert.Equal(t, "test_data_dir/test-service.key", fileBackend.ReadHistory[0])
	assert.Equal(t, "test_data_dir/test-service.crt", fileBackend.ReadHistory[1])
	assert.Equal(t, "test_data_dir/test-service.crt", fileBackend.DeleteHistory[0])
	assert.Equal(t, "test_data_dir/test-service.key", fileBackend.DeleteHistory[1])
	assert.Equal(t, "test_data_dir/test-service.csr", fileBackend.DeleteHistory[2])
	assert.Equal(t, "test key content", keyData.Key)
	assert.Equal(t, "test cert content", keyData.Cert)
}

func TestCertForServiceReturnsCertstrapReqErrors(t *testing.T) {
	cmdBackend, fileBackend, be := basicTestSetup(t)
	cmdBackend.Expect("CombinedOutput", command.ExpectedCommand{}).Return("fake certstrap output", errors.New("exit status 1"))
	fileBackend.AddOpResult("file deleted", true, nil)

	_, err := be.CertForService(testRequest)
	cmdBackend.AssertAllCalled()
	assert.Equal(t,
		"certstrap request-cert failure: fake certstrap output: exit status 1",
		err.Error())
}

func TestCertForServiceReturnsCertstrapSignErrors(t *testing.T) {
	cmdBackend, fileBackend, be := basicTestSetup(t)
	cmdBackend.Expect("CombinedOutput", command.ExpectedCommand{}).Return("ok", nil).Once()
	cmdBackend.Expect("CombinedOutput", command.ExpectedCommand{}).Return("fake certstrap output", errors.New("exit status 1")).Once()

	// delete key, csr, and cert
	fileBackend.AddOpResult("file deleted", true, nil)
	fileBackend.AddOpResult("file deleted", true, nil)
	fileBackend.AddOpResult("file deleted", true, nil)

	_, err := be.CertForService(testRequest)
	cmdBackend.AssertAllCalled()
	assert.Equal(t,
		"certstrap sign failure: fake certstrap output: exit status 1",
		err.Error())
}

func TestCertForServiceReturnsKeyReadErrors(t *testing.T) {
	cmdBackend, fileBackend, be := basicTestSetup(t)
	// request-cert and sign
	cmdBackend.Expect("CombinedOutput", command.ExpectedCommand{}).Return("test command output", nil).Times(2)

	// Read key and fail
	fileBackend.AddOpResult("", true, errors.New("test key read error"))

	// delete key, csr, and cert
	fileBackend.AddOpResult("file deleted", true, nil)
	fileBackend.AddOpResult("file deleted", true, nil)
	fileBackend.AddOpResult("file deleted", true, nil)

	_, err := be.CertForService(testRequest)
	assert.Equal(t, "error reading key data from disk: test key read error", err.Error())
}

func TestCertForServiceReturnsCertReadErrors(t *testing.T) {
	cmdBackend, fileBackend, be := basicTestSetup(t)
	// request-cert and sign
	cmdBackend.Expect("CombinedOutput", command.ExpectedCommand{}).Return("test command output", nil).Times(2)

	// Read key, then read cert and fail
	fileBackend.AddOpResult("key data", true, nil)
	fileBackend.AddOpResult("", true, errors.New("test key read error"))

	// delete key, csr, and cert
	fileBackend.AddOpResult("file deleted", true, nil)
	fileBackend.AddOpResult("file deleted", true, nil)
	fileBackend.AddOpResult("file deleted", true, nil)

	_, err := be.CertForService(testRequest)
	cmdBackend.AssertAllCalled()
	assert.Equal(t, "error reading cert data from disk: test key read error", err.Error())
}

func TestCertForServiceIgnoresFileDeleteErrors(t *testing.T) {
	cmdBackend, fileBackend, be := basicTestSetup(t)
	// request-cert and sign
	cmdBackend.Expect("CombinedOutput", command.ExpectedCommand{}).Return("test command output", nil).Times(2)

	// Read key, then read cert and fail
	fileBackend.AddOpResult("test key data", true, nil)
	fileBackend.AddOpResult("test cert data", true, nil)

	// delete key, csr, and cert
	fileBackend.AddOpResult("file deleted", true, errors.New("Failed to delete"))
	fileBackend.AddOpResult("file deleted", true, errors.New("Failed to delete"))
	fileBackend.AddOpResult("file deleted", true, errors.New("Failed to delete"))

	_, err := be.CertForService(testRequest)
	cmdBackend.AssertAllCalled()
	assert.Equal(t, nil, err)
}
