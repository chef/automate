package keys

// Generate key bindata
//go:generate curl --fail --silent --output data/keys.json http://license-acceptance.chef.co/keys
//go:generate go-bindata -pkg $GOPACKAGE -o keys.bindata.go data/...

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
)

// PublicKeysData is our license public keys helper
type PublicKeysData struct {
	Keys []string `json:"keys"`
}

// LoadPublicKeys loads our keys and returns map with checksums
func LoadPublicKeys(pubKeysData []byte) (map[string][]byte, error) {
	pub := PublicKeysData{}

	err := json.Unmarshal(pubKeysData, &pub)
	if err != nil {
		return nil, err
	}

	keysWithChecksums := make(map[string][]byte)
	for _, v := range pub.Keys {
		key := []byte(v)
		checksum := makeChecksum(key)

		keysWithChecksums[checksum] = key
	}

	return keysWithChecksums, nil
}

func makeChecksum(key []byte) string {
	keySha256 := sha256.Sum256(key)
	keyString := hex.EncodeToString(keySha256[:])

	return keyString
}
