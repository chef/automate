package keys_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/license-control-service/pkg/keys"
)

var keysFixture = keys.PublicKeysData{
	Keys: []string{
		`-----BEGIN PUBLIC KEY-----
MIGbMBAGByqGSM49AgEGBSuBBAAjA4GGAAQBFiE1Y7Go1RcZL4iA2zPIlWy/gMqw
9FHC+jbpdv24x+EONqwh1YF6wxcXWNFxxTip/6b7Ch3LD1hDM7Q6JxD7A9EAftfP
H347+8MKNzBhektOJka/vfTYlg8OF9MuFORet/FXpp24xHTdT3JARRy/SQHYRwCF
D6U+KRSFP9c/TXSQw2c=
-----END PUBLIC KEY-----

`,
	},
}

func TestLoadPublicKeys(t *testing.T) {
	keyMap := keys.LoadPublicKeys(keysFixture)

	expectedKey := "e0df28c8bc68150edbfef98cd6b7dc439c1f80c7e7ef747893a6893a2f7b60f7"
	_, ok := keyMap[expectedKey]
	assert.True(t, ok, "We didn't find the checksum we expected in the keys map")
}

// This checks that we have at least 1 builtin key
func TestBuiltinKeyData(t *testing.T) {
	assert.Equal(t, 1, len(keys.BuiltinKeyData.Keys), "expected at least 1 key in keys.BuiltinKeyData.Keys")
}
