package secrets

import (
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"encoding/hex"
	"io/ioutil"
	"path/filepath"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/pkg/errors"
)

// SecretKeyJSON secret json structure thats required to write and read
// to the encrypted secret file
type SecretKeyJSON struct {
	Algorithm  string               `json:"algorithm"`
	IV         initializationVector `json:"iv"`
	Ciphertext ciphertext           `json:"ciphertext"`
}

type secretStoreKey struct {
	keyData rawBytes
}

func (d *diskStore) keyPath() string {
	return filepath.Join(d.basePath, "key")
}

func (d *diskStore) keyExists() (bool, error) {
	return fileutils.PathExists(d.keyPath())
}

func (d *diskStore) ensureKey() (secretStoreKey, error) {
	exists, err := d.keyExists()
	if err != nil {
		return secretStoreKey{}, err
	}

	if exists {
		// If the key is already created, we will not recreate it
		return d.loadKey()
	}

	// Generate a 32 byte random key for AES
	rb, err := randomBytes(32)
	if err != nil {
		return secretStoreKey{}, err
	}

	// Serialize the key. In our serialized form, it it represented
	// with hex encoding
	serializedKey, err := rb.MarshalText()
	if err != nil {
		return secretStoreKey{}, err
	}

	// creating path to store the secret key
	s := bytes.NewReader(serializedKey)

	// writing the key to a file
	err = fileutils.AtomicWrite(d.keyPath(), s,
		fileutils.WithAtomicWriteFileMode(0700),
		fileutils.WithAtomicWriteChown(d.ownerUid, d.ownerGid),
	)
	if err != nil {
		return secretStoreKey{}, errors.Wrap(err, "could not write secret to disk")
	}

	return secretStoreKey{
		keyData: rb,
	}, nil
}

func (d *diskStore) loadKey() (secretStoreKey, error) {
	key, err := ioutil.ReadFile(d.keyPath())
	if err != nil {
		return secretStoreKey{}, err
	}
	rb := rawBytes{}
	if err := rb.UnmarshalText(key); err != nil {
		return secretStoreKey{}, err
	}
	return secretStoreKey{
		keyData: rb,
	}, nil
}

func (key secretStoreKey) encrypt(plaintext []byte) (initializationVector, ciphertext, error) {
	block, err := aes.NewCipher(key.keyData)
	if err != nil {
		return nil, nil, errors.Wrap(err, "error creating cipher block during encryption")
	}
	iv, err := randomBytes(block.BlockSize())
	if err != nil {
		return nil, nil, errors.Wrap(err, "could not generate random iv for encryption")
	}
	stream := cipher.NewCTR(block, iv)
	ct := make([]byte, len(plaintext))
	stream.XORKeyStream(ct, plaintext)

	return iv, ct, nil
}

func (key secretStoreKey) decrypt(iv initializationVector, ct ciphertext) ([]byte, error) {
	block, err := aes.NewCipher(key.keyData)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to covert secret key to block")
	}
	stream := cipher.NewCTR(block, iv)
	plain := make([]byte, len(ct))
	stream.XORKeyStream(plain, ct)

	return plain, nil
}

type rawBytes []byte

func randomBytes(numBytes int) (rawBytes, error) {
	rb := make([]byte, numBytes)
	if _, err := rand.Read(rb); err != nil {
		return nil, errors.Wrap(err, "failed to read random data while generating random bytes")
	}
	return rb, nil
}

func (rb *rawBytes) UnmarshalText(text []byte) error {
	b := make([]byte, hex.DecodedLen(len(text)))
	_, err := hex.Decode(b, text)
	if err != nil {
		return err
	}
	*rb = b
	return nil
}

func (rb rawBytes) MarshalText() ([]byte, error) {
	if rb == nil {
		return []byte(""), nil
	}
	strVal := hex.EncodeToString(rb)
	return []byte(strVal), nil
}

type ciphertext = rawBytes
type initializationVector = rawBytes
