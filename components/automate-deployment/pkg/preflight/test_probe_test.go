package preflight

import (
	"io/ioutil"
	"os"
	"path"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestLocalProbeFileThings(t *testing.T) {
	tmpdir := t.TempDir()

	testData := []byte("blah")
	filePath := path.Join(tmpdir, "file")
	ioutil.WriteFile(filePath, testData, 0777)
	symlinkPath := path.Join(tmpdir, "symlink")
	err := os.Symlink(filePath, symlinkPath)
	require.NoError(t, err)

	reporter := NewInMemoryPrintReporter()
	probes := NewTestProbe(reporter)

	isSymlink, err := probes.IsSymlink(symlinkPath)
	require.NoError(t, err)
	assert.True(t, isSymlink)

	isSymlink, err = probes.IsSymlink(filePath)
	require.NoError(t, err)
	assert.False(t, isSymlink)

	data, err := probes.File(filePath)
	require.NoError(t, err)
	assert.Equal(t, testData, data)
}
