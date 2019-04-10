package certauthority

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/platform/command"
)

// A FileStore manages files identified by string names.
type FileStore interface {
	// ReadFile returns the contents of the file as a string
	ReadFile(string) (string, error)
	// DeleteFile removes the file from the store
	DeleteFile(string) error
	// Exist returns true if the file exists in the store
	Exist(string) (bool, error)
}

// A DiskStore is a FileStore that uses the local filesystem. It
// assumes filenames are paths on disk. This is the implementation of
// FileStore used by default.
type DiskStore struct{}

// Exist returns true if the file exists on disk.  False otherwise.
func (d *DiskStore) Exist(path string) (bool, error) {
	_, err := os.Stat(path)

	if os.IsNotExist(err) {
		return false, nil
	}

	return true, err
}

// ReadFile returns the contents of the file at the named path.
func (d *DiskStore) ReadFile(path string) (string, error) {
	data, err := ioutil.ReadFile(path)
	return strings.TrimSuffix(string(data), "\n"), err
}

// DeleteFile deletes the file at the named path.
func (d *DiskStore) DeleteFile(path string) error {
	return os.Remove(path)
}

// The CertstrapBackend is a CertAuthorityBackend (see
// certauthority.go) that uses the certstrap command line tool to
// produce certificates.
type CertstrapBackend struct {
	// Directory where certstrap can keep its rootCA key
	// for signing certs
	dataDirectory string
	// authorityName is the CN in our CA certificate.  Storing it
	// here makes it easy to find it later without having the user
	// ask for it again.
	authorityName string
	// cmdExecutor is a bit of indirection around calling exec.
	// Making it easy to substitute another executor
	cmdExecutor command.Executor
	// fileStore is more indirection around file-related calls.
	fileStore FileStore
}

// NewCertstrapBackend returns a CertstrapBackend for the given
// authority name which will store its data in the given dataDir. It
// uses the default implementations of cmdExecutor and fileStore.
func NewCertstrapBackend(dataDir string, authorityName string) *CertstrapBackend {
	return &CertstrapBackend{
		dataDirectory: dataDir,
		authorityName: authorityName,
		cmdExecutor:   command.NewExecExecutor(),
		fileStore:     &DiskStore{},
	}
}

// SetFileStore sets the file store to use.  Used in testing.
func (b *CertstrapBackend) SetFileStore(f FileStore) {
	b.fileStore = f
}

// SetCmdExecutor sets the file store to use.  Used in testing.
func (b *CertstrapBackend) SetCmdExecutor(e command.Executor) {
	b.cmdExecutor = e
}

// Init initializes a new Certificate Authority using certstrap and
// returns the root CA certificate.
func (b *CertstrapBackend) Init() (string, error) {
	isInit, err := b.IsInitialized()
	if err != nil {
		return "", err
	}

	if !isInit {
		err = b.initCA()
		if err != nil {
			return "", err
		}
	}

	b.ensureFilePermissions()

	data, err := b.fileStore.ReadFile(b.certPath(b.authorityName))
	if err != nil {
		return "", errors.Wrap(err, "unable to read certificate data")
	}

	return data, nil
}

func (b *CertstrapBackend) ensureFilePermissions() {
	// The restore process sets the permissions for these files to something
	// that certstrap does not accept.
	// We need to fixit.
	for p, mode := range map[string]os.FileMode{
		b.certPath(b.authorityName): 0444,
		b.crlPath(b.authorityName):  0444,
		b.keyPath(b.authorityName):  0440,
	} {
		err := os.Chmod(p, mode)
		if err != nil {
			logrus.WithError(err).Warnf("Failed to set mode of %s(%s)", p, mode.String())
		}
	}
}

// ReInit re-initializes a new Certificate Authority using certstrap and
// returns the root CA certificate.
func (b *CertstrapBackend) ReInit() (string, error) {
	os.Remove(b.certPath(b.authorityName))
	os.Remove(b.csrPath(b.authorityName))
	os.Remove(b.keyPath(b.authorityName))
	os.Remove(b.crlPath(b.authorityName))
	return b.Init()
}

// IsInitialized returns a bool indicating whether the CA has been
// initialized. This check should succeed if we believe calls to
// CertForService will succeed.
func (b *CertstrapBackend) IsInitialized() (bool, error) {
	caKeyPath := b.keyPath(b.authorityName)
	exist, err := b.fileStore.Exist(caKeyPath)
	if err != nil {
		return false, errors.Wrap(err, "unable to determine if CA is initialized")
	}

	return exist, nil
}

// CertForService creates an x509 certificate and RSA key for the
// given name.
//
// Certstrap will create these on disk, so we read them in and then
// try to clean them up off the disk.
func (b *CertstrapBackend) CertForService(request CertRequest) (*ServiceCertData, error) {
	commonName := request.name

	keyPath := b.keyPath(commonName)
	csrPath := b.csrPath(commonName)
	certPath := b.certPath(commonName)

	err := b.requestCert(request)
	defer b.tryDelete(csrPath)

	if err != nil {
		return nil, err
	}

	err = b.signCert(commonName)
	defer b.tryDelete(keyPath)
	defer b.tryDelete(certPath)

	if err != nil {
		return nil, err
	}

	keyData, err := b.fileStore.ReadFile(keyPath)
	if err != nil {
		return nil, errors.Wrap(err, "error reading key data from disk")
	}

	certData, err := b.fileStore.ReadFile(certPath)
	if err != nil {
		return nil, errors.Wrap(err, "error reading cert data from disk")
	}

	return &ServiceCertData{Key: keyData, Cert: certData}, nil
}

func (b *CertstrapBackend) certstrapCmd(cmdArgs ...string) error {
	args := append([]string{"--depot-path", b.dataDirectory}, cmdArgs...)
	output, err := b.cmdExecutor.CombinedOutput("certstrap", command.Args(args...))

	if err != nil {
		// args[2] will be the certstrap subcommand in use
		msg := fmt.Sprintf("certstrap %s failure: %s", args[2], output)
		return errors.WithMessage(err, msg)
	}

	return nil
}

func (b *CertstrapBackend) initCA() error {
	return b.certstrapCmd("init", "--common-name", b.authorityName, "--passphrase", "")
}

func (b *CertstrapBackend) requestCert(request CertRequest) error {
	args := []string{
		"request-cert",
		"--common-name", request.name,
		"--passphrase", "",
	}
	if len(request.ips) > 0 {
		ipStrs := make([]string, len(request.ips))
		for i, ip := range request.ips {
			ipStrs[i] = ip.String()
		}
		args = append(args, "--ip", strings.Join(ipStrs, ","))
	}
	if len(request.dnsNames) > 0 {
		args = append(args, "--domain", strings.Join(request.dnsNames, ","))
	}
	return b.certstrapCmd(args...)
}

func (b *CertstrapBackend) signCert(commonName string) error {
	return b.certstrapCmd("sign", "--CA", b.authorityName, commonName)
}

func (b *CertstrapBackend) certPath(commonName string) string {
	return b.pathInStore(commonName, "crt")
}

func (b *CertstrapBackend) keyPath(commonName string) string {
	return b.pathInStore(commonName, "key")
}

func (b *CertstrapBackend) csrPath(commonName string) string {
	return b.pathInStore(commonName, "csr")
}

func (b *CertstrapBackend) crlPath(commonName string) string {
	return b.pathInStore(commonName, "crl")
}

func (b *CertstrapBackend) pathInStore(commonName string, ext string) string {
	fileName := strings.Replace(commonName, " ", "_", -1)
	return fmt.Sprintf("%s/%s.%s", b.dataDirectory, fileName, ext)
}

func (b *CertstrapBackend) tryDelete(path string) {
	err := b.fileStore.DeleteFile(path)
	if err != nil {
		logrus.Warnf("unable to delete %s: %s", path, err.Error())
	}
}
