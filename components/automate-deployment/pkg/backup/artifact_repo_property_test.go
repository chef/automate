package backup

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"reflect"
	"sync"
	"testing"

	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/commands"
	"github.com/leanovate/gopter/gen"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type mockReadOnlyBucket struct {
}

func (m *mockReadOnlyBucket) NewReader(ctx context.Context, path string, verifier ObjectVerifier) (io.ReadCloser, error) {
	return ioutil.NopCloser(bytes.NewBufferString("habpkgdata")), nil
}

func (m *mockReadOnlyBucket) NewWriter(ctx context.Context, path string) (BlobWriter, error) {
	return nil, errors.New("unimplemented")
}

func (m *mockReadOnlyBucket) List(ctx context.Context, pathPrefix string, delimited bool) ([]BucketObject, []SharedPrefix, error) {
	return nil, nil, errors.New("unimplemented")
}

func (m *mockReadOnlyBucket) Delete(ctx context.Context, objectPaths []string) error {
	return errors.New("unimplemented")
}

type mockWriteOnlyBucket struct {
	lock              sync.Mutex
	uploadedArtifacts []string
}

type mockBlobWriter struct {
	io.Writer
}

func (*mockBlobWriter) BlobSHA256() string {
	return "unimplemented"
}

func (*mockBlobWriter) Fail(err error) error {
	return err
}

func (*mockBlobWriter) Close() error {
	return nil
}

func newMockBlobWriter() BlobWriter {
	return &mockBlobWriter{
		Writer: ioutil.Discard,
	}
}

func (m *mockWriteOnlyBucket) NewReader(ctx context.Context, path string, verifier ObjectVerifier) (io.ReadCloser, error) {
	return nil, errors.New("unimplemented")
}

func (m *mockWriteOnlyBucket) NewWriter(ctx context.Context, path string) (BlobWriter, error) {
	m.lock.Lock()
	defer m.lock.Unlock()
	m.uploadedArtifacts = append(m.uploadedArtifacts, path)
	return newMockBlobWriter(), nil
}

func (m *mockWriteOnlyBucket) List(ctx context.Context, pathPrefix string, delimited bool) ([]BucketObject, []SharedPrefix, error) {
	return nil, nil, errors.New("unimplemented")
}

func (m *mockWriteOnlyBucket) Delete(ctx context.Context, objectPaths []string) error {
	return errors.New("unimplemented")
}

type testRepoState struct {
	baseDir   string
	snapshots map[string][]string
}
type repoTestSystem struct {
	repo    *ArtifactRepo
	baseDir string
}

type snapshotCommand struct {
	Name      string
	Artifacts []string
}

type snapshotCommandResult struct {
	meta ArtifactRepoSnapshotMetadata
	err  error
}

func (value snapshotCommand) Run(rts commands.SystemUnderTest) commands.Result {
	logrus.Infof("Creating snapshot %s", value.Name)
	roBucket := &mockReadOnlyBucket{}
	meta, err := rts.(*repoTestSystem).repo.Snapshot(context.Background(),
		value.Name, roBucket, NewArrayStream([]string(value.Artifacts)))
	return snapshotCommandResult{
		meta: meta,
		err:  err,
	}
}

func (value snapshotCommand) NextState(state commands.State) commands.State {
	st := state.(*testRepoState)
	if _, exists := st.snapshots[value.Name]; exists {
		return st
	}
	st.snapshots[value.Name] = value.Artifacts
	return st
}

func (value snapshotCommand) PreCondition(state commands.State) bool {
	return true
}

func (value snapshotCommand) PostCondition(state commands.State, result commands.Result) *gopter.PropResult {
	st := state.(*testRepoState)
	res := result.(snapshotCommandResult)
	if res.err != nil && res.err != ErrSnapshotExists {
		return &gopter.PropResult{Status: gopter.PropError, Error: res.err}
	}
	// The snapshot file should exist and contain the artifacts we required for the snapshot
	if _, exists := st.snapshots[value.Name]; !exists {
		return &gopter.PropResult{Status: gopter.PropFalse}
	}
	var expectedArtifacts []string
	if res.err == ErrSnapshotExists {
		return &gopter.PropResult{Status: gopter.PropTrue}
	} else {
		expectedArtifacts = value.Artifacts
	}
	snapshotPath := path.Join(st.baseDir, "shared/builder/snapshots", fmt.Sprintf("%s.snapshot", value.Name))
	artifacts, err := readGzipFile(snapshotPath)
	if err != nil {
		return &gopter.PropResult{Status: gopter.PropError, Error: errors.Wrap(err, "snapshot file not created")}
	}
	if !stringSliceEquals(expectedArtifacts, artifacts) {
		return gopter.NewPropResult(false, "did not find expected artifacts in the repo")
	}

	allExist, err := checkArtifactsExist(st.baseDir, artifacts)
	if err != nil {
		return &gopter.PropResult{Status: gopter.PropError, Error: errors.Wrap(err, "could not check artifact file")}
	}
	if !allExist {
		return gopter.NewPropResult(false, "missing artifacts")
	}

	return &gopter.PropResult{Status: gopter.PropTrue}
}

func (value snapshotCommand) String() string {
	return fmt.Sprintf("SNAPSHOT(%s)", value.Name)
}

type removeSnapshotCommand struct {
	SnapshotName string
}

type removeSnapshotCommandResult struct {
	Err error
}

func (value removeSnapshotCommand) Run(rts commands.SystemUnderTest) commands.Result {
	logrus.Infof("Removing snapshot %s", value.SnapshotName)
	return removeSnapshotCommandResult{
		Err: rts.(*repoTestSystem).repo.Remove(context.Background(), value.SnapshotName),
	}
}

func (value removeSnapshotCommand) NextState(state commands.State) commands.State {
	st := state.(*testRepoState)
	delete(st.snapshots, value.SnapshotName)
	return st
}

func (value removeSnapshotCommand) PreCondition(state commands.State) bool {
	return true
}

func (value removeSnapshotCommand) PostCondition(state commands.State, result commands.Result) *gopter.PropResult {
	st := state.(*testRepoState)
	res := result.(removeSnapshotCommandResult)
	err := res.Err

	if err != nil && !IsNotExist(err) {
		return &gopter.PropResult{
			Status: gopter.PropError,
			Error:  err,
		}
	}

	for s, artifacts := range st.snapshots {
		allExist, err := checkArtifactsExist(st.baseDir, artifacts)
		if err != nil {
			return &gopter.PropResult{Status: gopter.PropError, Error: errors.Wrap(err, "could not check artifact file")}
		}
		if !allExist {
			return gopter.NewPropResult(false, fmt.Sprintf("missing artifacts for snapshot %s", s))
		}
	}

	return gopter.NewPropResult(true, "")
}

func (value removeSnapshotCommand) String() string {
	return fmt.Sprintf("REMOVE(%s)", value.SnapshotName)
}

type restoreSnapshotCommand struct {
	SnapshotName string
}

type restoreSnapshotCommandResult struct {
	RestoredFiles []string
	Err           error
}

func (value restoreSnapshotCommand) Run(rts commands.SystemUnderTest) commands.Result {
	logrus.Infof("Restoring snapshot %s", value.SnapshotName)
	dstBucket := &mockWriteOnlyBucket{}
	err := rts.(*repoTestSystem).repo.Restore(context.Background(), dstBucket, value.SnapshotName)
	return restoreSnapshotCommandResult{
		Err:           err,
		RestoredFiles: sortStrings(dstBucket.uploadedArtifacts),
	}
}

func (value restoreSnapshotCommand) NextState(state commands.State) commands.State {
	return state
}

func (value restoreSnapshotCommand) PreCondition(state commands.State) bool {
	return true
}

func (value restoreSnapshotCommand) PostCondition(state commands.State, result commands.Result) *gopter.PropResult {
	st := state.(*testRepoState)
	res := result.(restoreSnapshotCommandResult)
	err := res.Err

	if err != nil {
		if IsNotExist(err) {
			if _, exists := st.snapshots[value.SnapshotName]; exists {
				return gopter.NewPropResult(false, "expected snapshot to restore")
			}
			if len(res.RestoredFiles) == 0 {
				return gopter.NewPropResult(true, "")
			}
			return gopter.NewPropResult(false, "expected 0 artifacts restored")
		}
		return &gopter.PropResult{
			Status: gopter.PropError,
			Error:  err,
		}
	}

	if !stringSliceEquals(st.snapshots[value.SnapshotName], res.RestoredFiles) {
		return gopter.NewPropResult(false, "restored files did not match files expected to be restored")
	}

	return gopter.NewPropResult(true, "")
}

func (value restoreSnapshotCommand) String() string {
	return fmt.Sprintf("RESTORE(%s)", value.SnapshotName)
}

var genSnapshotCommand = gen.Struct(
	reflect.TypeOf(snapshotCommand{}),
	map[string]gopter.Gen{
		"Name":      stringGen(2),
		"Artifacts": smallStringListGen(),
	},
).WithShrinker(gopter.NoShrinker)

func genRemoveSnapshotCommand(availableSnapshot []string) gopter.Gen {
	snapshotNameGen := []gen.WeightedGen{
		{
			Weight: 1,
			Gen:    stringGen(5),
		},
	}

	if len(availableSnapshot) > 0 {
		ifs := make([]interface{}, len(availableSnapshot))
		for i, v := range availableSnapshot {
			ifs[i] = v
		}
		snapshotNameGen = append(
			snapshotNameGen,
			gen.WeightedGen{
				Weight: 3,
				Gen:    gen.OneConstOf(ifs...),
			})
	}

	return gen.Weighted(snapshotNameGen).Map(func(snapshotName string) removeSnapshotCommand {
		return removeSnapshotCommand{
			SnapshotName: snapshotName,
		}
	}).WithShrinker(gopter.NoShrinker)
}

func genRestoreSnapshotCommand(availableSnapshot []string) gopter.Gen {
	snapshotNameGen := []gen.WeightedGen{
		{
			Weight: 1,
			Gen:    stringGen(5),
		},
	}

	if len(availableSnapshot) > 0 {
		ifs := make([]interface{}, len(availableSnapshot))
		for i, v := range availableSnapshot {
			ifs[i] = v
		}
		snapshotNameGen = append(
			snapshotNameGen,
			gen.WeightedGen{
				Weight: 3,
				Gen:    gen.OneConstOf(ifs...),
			})
	}

	return gen.Weighted(snapshotNameGen).Map(func(snapshotName string) restoreSnapshotCommand {
		return restoreSnapshotCommand{
			SnapshotName: snapshotName,
		}
	}).WithShrinker(gopter.NoShrinker)
}

func numOpenFD(t *testing.T) int {
	fds, err := ioutil.ReadDir("/proc/self/fd/")
	require.NoError(t, err)
	return len(fds)
}

func TestArtifactRepo(t *testing.T) {
	parameters := gopter.DefaultTestParameters()
	parameters.MinSuccessfulTests = 300
	parameters.MaxShrinkCount = 1
	parameters.MaxSize = 80
	properties := gopter.NewProperties(parameters)

	baseDir, err := ioutil.TempDir("", "artifact-repo-test")
	require.NoError(t, err)
	defer os.RemoveAll(baseDir)

	repoCommands := &commands.ProtoCommands{
		NewSystemUnderTestFunc: func(initialState commands.State) commands.SystemUnderTest {
			s := initialState.(*testRepoState)
			require.NoError(t, os.Mkdir(s.baseDir, 0700))
			fsLoc := FilesystemLocationSpecification{
				Path: s.baseDir,
			}

			return &repoTestSystem{
				repo:    NewArtifactRepo(fsLoc),
				baseDir: s.baseDir,
			}
		},
		DestroySystemUnderTestFunc: func(sut commands.SystemUnderTest) {
			rts := sut.(*repoTestSystem)
			os.RemoveAll(rts.baseDir)
		},
		InitialStateGen: stringGen(20).Map(func(dirName string) *testRepoState {
			return &testRepoState{
				baseDir:   path.Join(baseDir, dirName),
				snapshots: make(map[string][]string),
			}
		}),
		GenCommandFunc: func(state commands.State) gopter.Gen {
			st := state.(*testRepoState)
			keys := make([]string, len(st.snapshots))
			i := 0
			for k := range st.snapshots {
				keys[i] = k
				i++
			}
			return gen.OneGenOf(genSnapshotCommand,
				genRestoreSnapshotCommand(keys), genRemoveSnapshotCommand(keys)).WithShrinker(gopter.NoShrinker)
		},
	}

	fdsBefore := numOpenFD(t)
	properties.Property("artifact repo", commands.Prop(repoCommands))
	properties.TestingRun(t)
	fdsAfter := numOpenFD(t)
	assert.Equal(t, fdsBefore, fdsAfter, "file descriptors leaked")
}
