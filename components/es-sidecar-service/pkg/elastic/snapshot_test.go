package elastic

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestLatestMajorVersion(t *testing.T) {
	t.Run("with one version", func(t *testing.T) {
		ver, err := clusterVersion([]string{"6.2.2"})
		require.NoError(t, err)
		require.Equal(t, 6, ver)
	})

	t.Run("with two matching majors", func(t *testing.T) {
		ver, err := clusterVersion([]string{"6.2.2", "6.3.4"})
		require.NoError(t, err)
		require.Equal(t, 6, ver)
	})

	t.Run("with two different majors", func(t *testing.T) {
		_, err := clusterVersion([]string{"5.1.0", "6.2.2"})
		require.Error(t, err)
	})

	t.Run("with invalid version", func(t *testing.T) {
		_, err := clusterVersion([]string{"foo"})
		require.Error(t, err)
	})
}
