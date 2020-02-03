package backup

import (
	"bufio"
	"bytes"
	"compress/gzip"
	"context"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"reflect"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/leanovate/gopter"
	"github.com/leanovate/gopter/commands"
	"github.com/leanovate/gopter/gen"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
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

func readGzipFile(filePath string) ([]string, error) {
	lines := []string{}
	f, err := os.Open(filePath)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	g, err := gzip.NewReader(f)
	if err != nil {
		return nil, err
	}
	defer g.Close()

	scanner := bufio.NewScanner(g)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines, scanner.Err()
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

func checkArtifactsExist(baseDir string, artifacts []string) (bool, error) {
	for _, artifact := range artifacts {
		p := path.Join(baseDir, "shared/builder/artifacts", artifact)
		exists, err := fileutils.PathExists(p)
		if err != nil {
			return false, errors.Wrap(err, "could not check artifact file")
		}
		if !exists {
			logrus.Infof("Could not find %q", p)
			return false, nil
		}
	}
	return true, nil
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

func stringGen(size int) gopter.Gen {
	return gen.SliceOfN(size, gen.AlphaNumChar()).Map(func(r []rune) string {
		return string(r)
	}).WithShrinker(gen.StringShrinker)
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

func TestArtifactRepo(t *testing.T) {
	parameters := gopter.DefaultTestParameters()
	parameters.MinSuccessfulTests = 200
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
			return gen.OneGenOf(genSnapshotCommand, genRemoveSnapshotCommand(keys)).WithShrinker(gopter.NoShrinker)
		},
	}

	properties.Property("artifact repo", commands.Prop(repoCommands))
	properties.TestingRun(t)
}
