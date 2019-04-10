package executablecache

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const testExecutable = `#!/bin/sh
echo hello
exit 0
`

const testVersion = "20180522150700"

const testVersionOld = "20180522150600"
const testVersionOlder = "20180522150500"

func TestExecutableCacheRoundTrip(t *testing.T) {
	tmpdir, err := ioutil.TempDir("", "TestExecutableCache")
	require.NoError(t, err)
	defer os.RemoveAll(tmpdir)

	ec := New(WithCacheDir(tmpdir))
	exists, err := ec.Exists(testVersion)
	require.NoError(t, err)
	assert.False(t, exists)

	cmd, err := ec.Store(testVersion, []byte(testExecutable))
	require.NoError(t, err)
	cmd.Stdin = nil
	cmd.Stdout = nil
	cmd.Stderr = nil

	output, err := cmd.Output()
	require.NoError(t, err)
	assert.Equal(t, "hello\n", string(output))

	exists, err = ec.Exists(testVersion)
	assert.True(t, exists)
}

func TestExecutableCacheRoundLatest(t *testing.T) {
	tmpdir, err := ioutil.TempDir("", "TestExecutableCache")
	require.NoError(t, err)
	defer os.RemoveAll(tmpdir)

	ec := New(WithCacheDir(tmpdir))
	exists, err := ec.Exists(testVersion)
	require.NoError(t, err)
	assert.False(t, exists)
	versions := []string{testVersionOlder, testVersionOld, testVersion}

	for _, v := range versions {
		cmd, err := ec.Store(v, []byte(testExecutable))
		require.NoError(t, err)
		assert.NotNil(t, cmd)
	}

	cmd, version := ec.Latest()
	require.NotNil(t, cmd)
	assert.Equal(t, testVersion, version)
	cmd.Stdin = nil
	cmd.Stdout = nil
	cmd.Stderr = nil

	output, err := cmd.Output()
	require.NoError(t, err)
	assert.Equal(t, "hello\n", string(output))

}
