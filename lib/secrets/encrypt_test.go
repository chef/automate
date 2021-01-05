package secrets

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestSecretStoreKey(t *testing.T) {
	keyData, err := randomBytes(32)
	require.NoError(t, err)
	k := secretStoreKey{
		keyData: keyData,
	}

	plaintext, err := randomBytes(1477)
	require.NoError(t, err)
	iv, ct, err := k.encrypt(plaintext)
	require.NoError(t, err)
	require.Len(t, iv, 16)
	require.Len(t, ct, 1477)
	require.NotEqual(t, plaintext, ct)

	pt, err := k.decrypt(iv, ct)
	require.NoError(t, err)
	require.Equal(t, []byte(plaintext), pt)
}
