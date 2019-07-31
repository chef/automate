package keys

// Generate key bindata
//go:generate go run ../../tools/gen-keys https://license-generation-service.chef.co/keys keys.bindata.go

import (
	"crypto/sha256"
	"encoding/hex"
	"time"

	"github.com/pkg/errors"

	lic "github.com/chef/automate/lib/license"
)

// PublicKeysData is our license public keys helper
type PublicKeysData struct {
	Keys []string `json:"keys"`
}

type LoadedKeyData map[string][]byte

type LicenseParser struct {
	keyData LoadedKeyData
}

type LicenseMetadata struct {
	ConfiguredAt time.Time
}

func NewLicenseParser(pub PublicKeysData) *LicenseParser {
	return &LicenseParser{
		keyData: LoadPublicKeys(pub),
	}
}

func (l *LicenseParser) Parse(licenseData string) (*lic.License, error) {
	publicKeySha256, err := lic.GetKeySha256(licenseData)
	if err != nil {
		return nil, errors.Wrap(err, "GetKeySha256")
	}

	license, err := lic.Read(licenseData, l.keyData[publicKeySha256])
	if err != nil {
		return nil, errors.Wrap(err, "Read")
	}

	return license, nil
}

// LoadPublicKeys loads our keys and returns map with checksums
func LoadPublicKeys(pub PublicKeysData) LoadedKeyData {
	keysWithChecksums := make(map[string][]byte, len(pub.Keys))
	for _, v := range pub.Keys {
		key := []byte(v)
		checksum := makeChecksum(key)
		keysWithChecksums[checksum] = key
	}
	return keysWithChecksums
}

func makeChecksum(key []byte) string {
	keySha256 := sha256.Sum256(key)
	return hex.EncodeToString(keySha256[:])
}
