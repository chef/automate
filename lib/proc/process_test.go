package proc

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestListAllHappyPath(t *testing.T) {
	listing, err := ListAll(WithProcMount("testdata/proc-test-happy/"))
	require.NoError(t, err)

	assert.Len(t, listing, 2)

	mapping := map[int]*Process{}
	mapping[listing[0].Pid] = listing[0]
	mapping[listing[1].Pid] = listing[1]

	assert.NotNil(t, mapping[1], "expected pid 1")
	assert.NotNil(t, mapping[682], "expected pid 682")

	pid1Stat, err := mapping[1].Stat()
	require.NoError(t, err)

	assert.Equal(t, &ProcessStat{
		Pid:   1,
		PPid:  0,
		State: "S",
		Comm:  "systemd",
	}, pid1Stat)

	pid682Stat, err := mapping[682].Stat()
	require.NoError(t, err)

	assert.Equal(t, &ProcessStat{
		Pid:   682,
		PPid:  1,
		State: "S",
		Comm:  "login",
	}, pid682Stat)

}

func TestListAllMissingStat(t *testing.T) {
	t.Run("test ListAll", func(t *testing.T) {
		listing, err := ListAll(WithProcMount("testdata/proc-test-missing-stat/"))
		require.NoError(t, err)

		assert.Len(t, listing, 2)

		mapping := map[int]*Process{}
		mapping[listing[0].Pid] = listing[0]
		mapping[listing[1].Pid] = listing[1]

		assert.NotNil(t, mapping[1], "expected pid 1")
		assert.NotNil(t, mapping[682], "expected pid 682")

		pid1Stat, err := mapping[1].Stat()
		require.NoError(t, err)

		assert.Equal(t, &ProcessStat{
			Pid:   1,
			PPid:  0,
			State: "S",
			Comm:  "systemd",
		}, pid1Stat)

		_, err = mapping[682].Stat()
		require.Error(t, err)
	})
}

func TestTreeHappyPath(t *testing.T) {
	tree, err := Tree(WithProcMount("testdata/proc-test-happy/"))
	require.NoError(t, err)

	pid1Stat, err := tree[1].Stat()
	require.NoError(t, err)

	assert.Equal(t, &ProcessStat{
		Pid:   1,
		PPid:  0,
		State: "S",
		Comm:  "systemd",
	}, pid1Stat)
	assert.Len(t, tree[1].Children, 1)
	assert.Equal(t, 682, tree[1].Children[0].Pid)
	assert.Equal(t, tree[682], tree[1].Children[0])
}

func TestTreeMissingStat(t *testing.T) {
	tree, err := Tree(WithProcMount("testdata/proc-test-missing-stat/"))
	require.NoError(t, err)

	pid1Stat, err := tree[1].Stat()
	require.NoError(t, err)

	assert.Equal(t, &ProcessStat{
		Pid:   1,
		PPid:  0,
		State: "S",
		Comm:  "systemd",
	}, pid1Stat)
	assert.Len(t, tree[1].Children, 0)
	assert.NotNil(t, tree[682])
	assert.Len(t, tree[682].Children, 0)
}
