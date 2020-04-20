package backup

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestSnapshotsInBucket(t *testing.T) {
	obs := []BucketObject{
		{Name: "1.snapshot"},
		{Name: "2.snapshot"},
		{Name: "3.snapshot"},
		{Name: "4.snapshot"},
		{Name: "snapshot.integrity"},
		{Name: "some_junk"},
	}

	t.Run("no filters", func(t *testing.T) {
		all := []string{"1", "2", "3", "4"}
		res, err := snapshotsInBucket(obs)
		require.NoError(t, err)
		require.Equal(t, all, res)
	})

	t.Run("only filter", func(t *testing.T) {
		res, err := snapshotsInBucket(obs, OnlySnapshots([]string{"2", "4"}))
		require.NoError(t, err)
		require.Equal(t, []string{"2", "4"}, res)
	})

	t.Run("exclude filters", func(t *testing.T) {
		res, err := snapshotsInBucket(obs, ExcludeSnapshots([]string{"2", "4"}))
		require.NoError(t, err)
		require.Equal(t, []string{"1", "3"}, res)

	})
}
