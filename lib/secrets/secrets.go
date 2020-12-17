// The secrets package provides methods for generate, storing, and
// retrieving secrets data.
//
// Eventually, we hope to replace the file-system based code in here
// with code that makes use of a service (secrets-service) or Habitat.
package secrets

import (
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"encoding/base64"
	"encoding/hex"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/user"
	"github.com/chef/toml"
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

// SecretKeyToml secret toml structure thats required to write and read
// to the encrypted secret file
type SecretKeyToml struct {
	Algorithm  string `toml:"algorithm"`
	IV         string `toml:"iv"`
	Ciphertext string `toml:"ciphertext"`
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
	keyExists, err := keyExists(d.basePath)
	if err != nil {
		return err
	}
	if !keyExists {
		_, err := createEncryptionKey(d.basePath)
		if err != nil {
			return err
		}
	}

	return nil
}

func (d *diskStore) Exists(secret SecretName) (bool, error) {
	path := filepath.Join(d.basePath, secret.Group, secret.Name)
	return fileutils.PathExists(path)
}

func (d *diskStore) GetSecret(secret SecretName) ([]byte, error) {
	var ret []byte

	// checks if the encrypted secret exists
	pathToEncryptedSecret := filepath.Join(d.basePath, secret.Group, secret.Name+".enc.toml")
	encryptedSecretExists, err := fileutils.PathExists(pathToEncryptedSecret)

	if encryptedSecretExists {
		ret, err = ioutil.ReadFile(pathToEncryptedSecret)
		if err != nil {
			return nil, errors.Wrap(err, "could not read secrets data (secret may not have been generated yet)")
		}
		// decrypts and returns the secret value
		retE, err := getDecryptedData(d.basePath, ret)
		if err != nil {
			return nil, errors.Wrap(err, "could not decrypt secret with existing key)")
		}
		return retE, err
	}

	// else return unencrypted secret
	pathToUnencryptedSecret := filepath.Join(d.basePath, secret.Group, secret.Name)
	ret, err = ioutil.ReadFile(pathToUnencryptedSecret)
	if err != nil {
		return nil, errors.Wrap(err, "could not read secrets data (secret may not have been generated yet)")
	}
	return ret, err
}

func (d *diskStore) SetSecret(secret SecretName, data []byte) error {
	parentDir := filepath.Join(d.basePath, secret.Group)
	secretPath := filepath.Join(parentDir, secret.Name+".enc.toml")
	err := os.MkdirAll(parentDir, 0700)
	if err != nil {
		return errors.Wrap(err, "could not create directory for secrets data")
	}
	err = os.Chown(parentDir, d.ownerUid, d.ownerGid)
	if err != nil {
		return errors.Wrap(err, "could not chown directory for secrets data")
	}

	// created encrypted data from input data
	encryptedData, iv, err := getEncryptedData(data, d.basePath)
	if err != nil {
		return err
	}

	// writes as toml file
	err = writeToml("AES256 CTR mode", encodeBase64(iv), encodeBase64(encryptedData), secretPath)
	if err != nil {
		return errors.Wrap(err, "could not write secrets data to disk")
	}

	// TODO(ssd) 2018-08-20: Should this be an option we can pass to AtomicWrite?
	err = os.Chown(secretPath, d.ownerUid, d.ownerGid)
	if err != nil {
		return errors.Wrap(err, "could not chown secrets data")
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

// checks if encryption key exists or not
func keyExists(basePath string) (bool, error) {
	path := filepath.Join(basePath, "key")
	return fileutils.PathExists(path)
}

func encodeBase64(b []byte) string {
	return base64.StdEncoding.EncodeToString(b)
}

func decodeBase64(s string) ([]byte, error) {
	data, err := base64.StdEncoding.DecodeString(s)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to decode base64 string")
	}
	return data, nil
}

// Takes in key and the data, then encrypts the base64 encoded data
// Uses AES256 CTR mode for encryption
// returns the ciphertext, iv and error
func encrypt(key []byte, value []byte) ([]byte, []byte, error) {
	block, err := aes.NewCipher(key)
	if err != nil {
		return nil, nil, errors.Wrap(err, "error creating cipher block during encryption")
	}
	iv, err := GenerateRandomBytes(block.BlockSize())
	if err != nil {
		return nil, nil, errors.Wrap(err, "could not generate random iv for encryption")
	}
	b := encodeBase64(value)
	stream := cipher.NewCTR(block, iv)
	ciphertext := make([]byte, len(b))
	stream.XORKeyStream(ciphertext, []byte(b))
	return ciphertext, iv, nil
}

// decrpting the cipher for ctr mode
// returns the decoded data ( originally encoded with base64 ) and error
func decrypt(block cipher.Block, ciphertext []byte, iv []byte) ([]byte, error) {
	stream := cipher.NewCTR(block, iv)
	plain := make([]byte, len(ciphertext))
	stream.XORKeyStream(plain, ciphertext)
	d, err := decodeBase64(string(plain))
	if err != nil {
		return nil, err
	}
	return d, nil
}

// function used to write the secret values to given file in toml format
// stores the name of the algorithm , iv and the ciphertext
func writeToml(algorithm string, iv string, ciphertext string, path string) error {
	secretData := SecretKeyToml{
		Algorithm:  algorithm,
		IV:         iv,
		Ciphertext: ciphertext,
	}

	buf := new(bytes.Buffer)
	if err := toml.NewEncoder(buf).Encode(secretData); err != nil {
		log.Fatal(err)
	}

	err := fileutils.AtomicWrite(path, bytes.NewReader(buf.Bytes()), fileutils.WithAtomicWriteFileMode(0700))
	if err != nil {
		return err
	}
	return nil
}

// takes in the secret value and encrypts it
// return encrypted cipher, iv and error
func getEncryptedData(data []byte, basePath string) ([]byte, []byte, error) {
	// we create the encryption key if it doesnot exist
	var secretKey []byte
	keyExists, err := keyExists(basePath)
	if err != nil {
		return nil, nil, err
	}
	if !keyExists {
		secretKey, err = createEncryptionKey(basePath)
		if err != nil {
			return nil, nil, err
		}
	} else {
		path := filepath.Join(basePath, "key")
		secretKey, err = ioutil.ReadFile(path)
		if err != nil {
			return nil, nil, errors.Wrap(err, "could not read secrets key (unexpected error)")
		}
	}
	encryptedData, iv, err := encrypt(secretKey, data)
	if err != nil {
		return nil, nil, err
	}
	return encryptedData, iv, nil
}

// gets the secret from the toml file and decrypts it
// returns the decrypted data or error
func getDecryptedData(basePath string, ret []byte) ([]byte, error) {
	keyPath := filepath.Join(basePath, "key")
	key, err := ioutil.ReadFile(keyPath)
	block, err := aes.NewCipher(key)
	if err != nil {
		return nil, err
	}
	var secretData SecretKeyToml
	_, err = toml.Decode(string(ret), &secretData)
	if err != nil {
		return nil, err
	}

	// coverts ciphertext to byte from base64
	dcdCipher, err := decodeBase64(secretData.Ciphertext)
	if err != nil {
		return nil, err
	}
	// coverts iv to byte from base64
	dcdIv, err := decodeBase64(secretData.IV)
	if err != nil {
		return nil, err
	}

	decrypted, err := decrypt(block, dcdCipher, dcdIv)
	if err != nil {
		return nil, err
	}
	return decrypted, nil
}

// Creates the Key used for the secret values' encryption
// we generate a random byte array of 32 bytes required for the AES256 encryption
// returns the key
func createEncryptionKey(basePath string) ([]byte, error) {
	secretKey, err := GenerateRandomBytes(32)
	if err != nil {
		return nil, errors.Wrap(err, "could not generate random secret for encryption")
	}

	// creating path to store the secret key
	secretKeyPath := filepath.Join(basePath, "key")
	s := bytes.NewReader(secretKey)
	// writing the key to a file
	err = fileutils.AtomicWrite(secretKeyPath, s, fileutils.WithAtomicWriteFileMode(0700))
	if err != nil {
		return nil, errors.Wrap(err, "could not write secret to disk")
	}
	return secretKey, nil
}
