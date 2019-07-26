package keys

// Generate key bindata
//go:generate go run ../../tools/gen-keys https://license-generation-service.chef.co/keys keys.bindata.go

import (
	"crypto/sha256"
	"encoding/hex"
)

// PublicKeysData is our license public keys helper
type PublicKeysData struct {
	Keys []string `json:"keys"`
}

// LoadPublicKeys loads our keys and returns map with checksums
func LoadPublicKeys(pub PublicKeysData) map[string][]byte {
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
