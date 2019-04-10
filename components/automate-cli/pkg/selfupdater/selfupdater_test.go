package selfupdater

import (
	"context"
	"io/ioutil"
	"os"
	"path"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/selfupdater/executablecache"
)

type mockUpdateSource struct {
	mock.Mock
}

func (m *mockUpdateSource) DesiredVersion(ctx context.Context) (string, error) {
	args := m.Called(ctx)
	return args.String(0), args.Error(1)
}

func (m *mockUpdateSource) FetchLatest(ctx context.Context) ([]byte, string, error) {
	args := m.Called(ctx)
	ba := args.Get(0).([]byte)
	return ba, args.String(1), args.Error(2)
}

func (m *mockUpdateSource) ExpectExecutableFetch(ctx context.Context, desiredVersion string, executable string) {
	m.On("DesiredVersion", ctx).Return(desiredVersion, nil)
	m.On("FetchLatest", ctx).Return([]byte(executable), desiredVersion, nil)
}

func TestExecutableIsFetchedAndCachedWhenUpdateRequired(t *testing.T) {
	ctx := context.Background()
	tmpdir, err := ioutil.TempDir("", "TestExecutableCache")
	require.NoError(t, err)
	defer os.RemoveAll(tmpdir)
	executablecache := executablecache.New(executablecache.WithCacheDir(tmpdir))
	updateSource := &mockUpdateSource{}
	updateSource.ExpectExecutableFetch(ctx, "1", "#!/bin/sh\n")

	updater := &selfUpdater{
		executableCache: executablecache,
		updateSource:    updateSource,
		myVersion:       "0",
	}

	next, err := updater.NextExecutable(ctx)
	require.NoError(t, err)
	require.NotNil(t, next)
	assert.True(t, next.Available())
	assert.Equal(t, "1", next.Version)
	assert.Equal(t, path.Join(tmpdir, "1"), next.Cmd.Path)
}

func TestExecutableIsTakenFromCacheIfExists(t *testing.T) {
	ctx := context.Background()
	tmpdir, err := ioutil.TempDir("", "TestExecutableCache")
	require.NoError(t, err)
	defer os.RemoveAll(tmpdir)
	executablecache := executablecache.New(executablecache.WithCacheDir(tmpdir))
	updateSource := &mockUpdateSource{}
	updateSource.ExpectExecutableFetch(ctx, "1", "#!/bin/sh\n")

	updater := &selfUpdater{
		executableCache: executablecache,
		updateSource:    updateSource,
		myVersion:       "0",
	}

	_, err = updater.NextExecutable(ctx)
	require.NoError(t, err)

	_, err = updater.NextExecutable(ctx)
	require.NoError(t, err)

	updateSource.AssertNumberOfCalls(t, "DesiredVersion", 2)
	updateSource.AssertNumberOfCalls(t, "FetchLatest", 1)
}

func TestNextExecutableIsNotAvailableWhenVersionsMatch(t *testing.T) {
	ctx := context.Background()
	tmpdir, err := ioutil.TempDir("", "TestExecutableCache")
	require.NoError(t, err)
	defer os.RemoveAll(tmpdir)
	executablecache := executablecache.New(executablecache.WithCacheDir(tmpdir))
	updateSource := &mockUpdateSource{}
	updateSource.ExpectExecutableFetch(ctx, "1", "#!/bin/sh\n")

	updater := &selfUpdater{
		executableCache: executablecache,
		updateSource:    updateSource,
		myVersion:       "1",
	}

	next, err := updater.NextExecutable(ctx)
	require.NoError(t, err)
	assert.False(t, next.Available())
}
