package secrets

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestMarshalers(t *testing.T) {
	iv := initializationVector{0xff, 0xee, 0x0d, 0x00}
	v, err := iv.MarshalText()
	require.NoError(t, err)
	require.Equal(t, []byte("ffee0d00"), v)

	iv2 := initializationVector{}
	err = iv2.UnmarshalText([]byte("ffee0d00"))
	require.NoError(t, err)
	require.Equal(t, iv, iv2)
}
