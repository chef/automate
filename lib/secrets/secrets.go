// The secrets package provides methods for generate, storing, and
// retrieving secrets data.
//
// Eventually, we hope to replace the file-system based code in here
// with code that makes use of a service (secrets-service) or Habitat.
package secrets

import (
	"bytes"
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/user"
)

// Commonly used secrets names. When accessing this library from
// Golang code, these vars are provided to avoid copy-pasta errors
var BifrostSuperuserIDName = SecretName{
	Group: "oc_bifrost",
	Name:  "superuser_id",
}

// SecretName represents a secret that a service may want to generate
// or read. Secrets are placed into groups. Conventionally, the group
// is the name of the service that generates the secret.
type SecretName struct {
	Group string
	Name  string
}

func (s SecretName) String() string {
	return fmt.Sprintf("%s.%s", s.Group, s.Name)
}

// SecretNameFromString returns parses the string according to the
// chef_secrets convention of using . as a record separator.
func SecretNameFromString(spec string) (SecretName, error) {
	parts := strings.Split(spec, ".")
	if len(parts) != 2 {
		return SecretName{}, errors.Errorf("secret spec %s has an invalid number of parts (%d)", spec, len(parts))
	}

	return SecretName{
		Group: parts[0],
		Name:  parts[1],
	}, nil
}

// SecretStore allows you to store and retrieve secret data from a
// secrets store.
type SecretStore interface {
	Initialize() error
	SecretsReader
	SecretsWriter
}

// SecretsReader allows you to retrieve secrets data from a secrets
// store.
type SecretsReader interface {
	// NOTE(ssd) 2018-08-20: Exists is here to support idempotent
	// service startup. If we move this to a service, we might
	// consider making an CreateIfNotExists api and simplifying
	// this interface.
	Exists(SecretName) (bool, error)
	GetSecret(SecretName) ([]byte, error)
}

// SecretsWriter allows you to set secrets data from a secrets store.
type SecretsWriter interface {
	SetSecret(SecretName, []byte) error
}

// File-based implementation of a secrets store.
//
// Eventually we might replace this with the secrets-service if we
// decide this is a good model
type diskStore struct {
	basePath string
	ownerUid int
	ownerGid int
}

const (
	// DefaultDiskStoreDataDir is the default directory that we will use when
	// instantiating our disk-based data store. We allow overriding this
	// for local testing.
	DefaultDiskStoreDataDir = "/hab/svc/deployment-service/data/shared"
	// DefaultDiskStoreDataOwner is the user who should own all
	// files and directories in the disk store.
	//
	// TODO(ssd) 2018-08-20: This is unsafe currently because all of our
	// services run as the same user. In Chef Server, we have root own the
	// secrets store, read the secrets as root, and then drop privs when
	// we exec the command.
	DefaultDiskStoreDataOwner = "hab"

	// SecretFileExtension is the extension name for the encrypted secrets file
	SecretFileExtension = ".enc"
)

func NewDiskStoreReader(basePath string) SecretsReader {
	return &diskStore{
		basePath: basePath,
	}
}

func NewDiskStore(basePath string, uid, gid int) SecretStore {
	return &diskStore{
		basePath: basePath,
		ownerUid: uid,
		ownerGid: gid,
	}
}

func NewDefaultSecretStore() (SecretStore, error) {
	u, err := user.Lookup(DefaultDiskStoreDataOwner)
	if err != nil {
		return nil, errors.Wrap(err, "user lookup")
	}

	uid, err := strconv.Atoi(u.Uid)
	if err != nil {
		return nil, errors.Wrap(err, "converting uid to integer")
	}

	gid, err := strconv.Atoi(u.Gid)
	if err != nil {
		return nil, errors.Wrap(err, "converting gid to integer")
	}

	return NewDiskStore(DefaultDiskStoreDataDir, uid, gid), nil
}

// NOTE(ssd) 2018-08-20: Someone could race us to this directory.
// But, in the common case, the parent of the basePath will be owned
// by root and we will run initialize as root in the
// deployment-service.
func (d *diskStore) Initialize() error {
	err := os.MkdirAll(d.basePath, 0700)
	if err != nil {
		return errors.Wrap(err, "could not create shared data directory for secrets store")
	}

	err = os.Chown(d.basePath, d.ownerUid, d.ownerGid)
	if err != nil {
		return errors.Wrap(err, "could now chown shared data directory for secrets store")
	}

	// creates encryption key if it does not exist
	if _, err := d.ensureKey(); err != nil {
		return errors.Wrap(err, "could not create encryption key")
	}

	return nil
}

func (d *diskStore) Exists(secret SecretName) (bool, error) {
	// added logic to check if secret is encrypted or not
	encryptedPath := filepath.Join(d.basePath, secret.Group, addSecretFileExtension(secret.Name))
	encryptedExists, err := fileutils.PathExists(encryptedPath)
	if err != nil {
		return false, err
	}
	if encryptedExists {
		return true, nil
	}
	path := filepath.Join(d.basePath, secret.Group, secret.Name)
	return fileutils.PathExists(path)
}

func (d *diskStore) GetSecret(secret SecretName) ([]byte, error) {
	// checks if the encrypted secret exists
	pathToEncryptedSecret := filepath.Join(d.basePath, secret.Group, addSecretFileExtension(secret.Name))
	encryptedSecretExists, err := fileutils.PathExists(pathToEncryptedSecret)
	if err != nil {
		return nil, errors.Wrap(err, "unexpected error while checking if encrypted file exists (secret may not have been generated yet)")
	}
	if encryptedSecretExists {
		sktJSON := SecretKeyJSON{}
		data, err := ioutil.ReadFile(pathToEncryptedSecret)
		if err != nil {
			return nil, errors.Wrap(err, "unexpected error while reading encrypted secret data")
		}

		if err := json.Unmarshal(data, &sktJSON); err != nil {
			return nil, errors.Wrap(err, "unexpected error while reading encrypted secret data")
		}

		secretKey, err := d.loadKey()
		if err != nil {
			return nil, errors.Wrap(err, "failed to load secret key")
		}
		return secretKey.decrypt(sktJSON.IV, sktJSON.Ciphertext)
	}

	// else return unencrypted secret
	pathToUnencryptedSecret := filepath.Join(d.basePath, secret.Group, secret.Name)
	ret, err := ioutil.ReadFile(pathToUnencryptedSecret)
	if err != nil {
		return nil, errors.Wrap(err, "could not read unencrypted secrets data (secret may not have been generated yet)")
	}
	return ret, err
}

func (d *diskStore) SetSecret(secret SecretName, data []byte) error {
	secretKey, err := d.loadKey()
	if err != nil {
		return errors.Wrap(err, "failed to load secret key")
	}

	parentDir := filepath.Join(d.basePath, secret.Group)
	secretPath := filepath.Join(parentDir, addSecretFileExtension(secret.Name))

	if err := os.MkdirAll(parentDir, 0700); err != nil {
		return errors.Wrap(err, "could not create directory for secrets data")
	}

	if err := os.Chown(parentDir, d.ownerUid, d.ownerGid); err != nil {
		return errors.Wrap(err, "could not chown directory for secrets data")
	}

	iv, ct, err := secretKey.encrypt(data)
	if err != nil {
		return err
	}

	buf := new(bytes.Buffer)

	if err := json.NewEncoder(buf).Encode(SecretKeyJSON{
		Algorithm:  "AES256 CTR mode",
		IV:         iv,
		Ciphertext: ct,
	}); err != nil {
		return errors.Wrap(err, "failed to encode struct SecretKeyJSON data to json")
	}

	if err := fileutils.AtomicWrite(secretPath, bytes.NewReader(buf.Bytes()),
		fileutils.WithAtomicWriteFileMode(0700),
		fileutils.WithAtomicWriteChown(d.ownerUid, d.ownerGid),
	); err != nil {
		return errors.Wrap(err, "error writing the secret file")
	}

	return nil
}

// GenerateRandomBytes generates the requested number of ASCII bytes
// from the hex alphabet. The resulting byte slice should be safe to
// convert to a printable ascii string.
//
// Note, however, that the entropy of the returned byte array is 16^N
// instead of 256^N since each returned byte is from the hex alphabet.
func GenerateRandomBytes(userProvidedLength int) ([]byte, error) {
	// Extend by 1 to account for odd userProvidedLength values
	secretsLength := (userProvidedLength + 1) / 2
	randomBytes := make([]byte, secretsLength)
	encodedBytes := make([]byte, secretsLength*2)

	if _, err := rand.Read(randomBytes); err != nil {
		return nil, errors.Wrap(err, "failed to read random data while generating random bytes")
	}

	hex.Encode(encodedBytes, randomBytes)

	// Truncate the encoded bytes if it is longer than the
	// user-desired length (i.e. we were passed on odd value)
	return encodedBytes[:userProvidedLength], nil
}

// PrepareSSHPrivateKey takes the contents of a SSH key and saves them into a temporary file
func PrepareSSHPrivateKey(keyContent string) (string, error) {
	// Specify "" for the temp dir as ioutil will pick TMPDIR or OS default
	keyPath, err := ioutil.TempFile("", ".inspec")
	if err != nil {
		return "", err
	}

	err = ioutil.WriteFile(keyPath.Name(), []byte(keyContent), 0400)
	if err != nil {
		return "", errors.New("Failed to write inspec private key " + keyPath.Name() + ": " + err.Error())
	}
	return keyPath.Name(), nil
}

func addSecretFileExtension(name string) string {
	return name + SecretFileExtension
}
