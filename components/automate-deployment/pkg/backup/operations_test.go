package backup

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"testing"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/lib/platform/command"
)

var timestamp = "20060102150405"

func newTestBackupDir(desc string) (string, func()) {
	tmpDir, _ := ioutil.TempDir("", desc)
	src := filepath.Join(tmpDir, "src")
	dst := filepath.Join(tmpDir, "dst")
	_ = os.MkdirAll(src, 0750)
	_ = os.MkdirAll(dst, 0750)
	_ = ioutil.WriteFile(filepath.Join(src, "a"), []byte("a contents"), 0750)
	_ = ioutil.WriteFile(filepath.Join(src, "b"), []byte("b contents"), 0750)

	return tmpDir, func() { os.RemoveAll(tmpDir) }
}

func rsyncStdout() string {
	return `drwxr-x---             80 2018/08/28 14:18:15 .
-rwxr-x---             10 2018/08/28 14:18:15 a
-rwxr-x---             10 2018/08/28 14:18:15 b	
`
}

func newBackupContext(backupDir string) Context {
	parsed, _ := time.Parse(api.BackupTaskFormat, timestamp)
	ts, _ := ptypes.TimestampProto(parsed)
	backupTask := &api.BackupTask{Id: ts}
	return Context{
		locationSpec:    testLocationSpec(backupDir),
		bucket:          NewFilesystemBucket(path.Join(backupDir, backupTask.TaskID())),
		backupTask:      backupTask,
		ctx:             context.Background(),
		metadataWritten: NewObjectManifest(),
	}
}

func newPathCopyOperation(name, tmpDir string, cmd command.Executor, matchers []RsyncMatcher) *PathCopyOperation {
	return &PathCopyOperation{
		Name:          name,
		ObjectName:    []string{"some-service", "dst"},
		SrcPath:       filepath.Join(tmpDir, "src"),
		RsyncMatchers: matchers,
		cmdExecutor:   cmd,
	}
}

func newMetadataWriterOperation(name string) *MetadataWriterOperation {
	return &MetadataWriterOperation{
		Spec: &Spec{
			Name:          name,
			WriteMetadata: true,
		},
		ObjectName: []string{"dst"},
	}
}

func newCommandExecuteOperation(name string, objectName []string, cmd Cmd, origin, pkg string, cmdExec command.Executor) *CommandExecuteOperation {
	return &CommandExecuteOperation{
		Name:        name,
		ObjectName:  objectName,
		Cmd:         cmd,
		PkgOrigin:   origin,
		PkgName:     pkg,
		cmdExecutor: cmdExec,
	}
}

func newTestCmd() Cmd {
	return Cmd{
		Name:    "couchdb",
		Dump:    []string{"couchdb", "dump"},
		Restore: []string{"couchdb", "import"},
	}
}

func TestPathCopyOperationBackup(t *testing.T) {
	logrus.SetOutput(ioutil.Discard)
	t.Run("copies files", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("copies-files")
		defer cleanup()

		cmd := command.NewMockExecutor(t)
		expectedCmd := command.ExpectedCommand{
			Cmd: "hab",
			Args: []string{
				"pkg",
				"exec",
				"core/rsync",
				"rsync",
				"--recursive",
				"--list-only",
				filepath.Join(tmpDir, "src") + "/",
			},
		}
		cmd.Expect("Output", expectedCmd).Return(rsyncStdout(), nil)

		bctx := newBackupContext(tmpDir)
		eChan := make(chan OperationProgress, 10)
		op := newPathCopyOperation("copy", tmpDir, cmd, nil)
		om := NewObjectManifest()

		err := op.Backup(bctx, om, eChan)
		require.NoError(t, err, "execute should not return an error")
		cmd.AssertAllCalled()

		expectedSHAs := map[string]string{
			"some-service/dst/a": "b8b92be8398e4722a49b349b931e89b63c835d7157dee5f0100a799e4a20399f",
			"some-service/dst/b": "b3a59db76dea91f56365980991dad282e9de3f4e4598b06d76b9748c7397e33d",
		}
		assert.Equal(t, expectedSHAs, om.ObjectSHA256s())
	})

	t.Run("supports matchers", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("support-matchers")
		defer cleanup()

		cmd := command.NewMockExecutor(t)
		matchers := []RsyncMatcher{
			Include("a"),
			Exclude("b"),
		}
		expectedCmd := command.ExpectedCommand{
			Cmd: "hab",
			Args: []string{
				"pkg",
				"exec",
				"core/rsync",
				"rsync",
				"--recursive",
				"--list-only",
				"--include",
				"a",
				"--exclude",
				"b",
				filepath.Join(tmpDir, "src") + "/",
			},
		}
		// This is a lie. Since we don't actually run rsync in the tests, the consant
		// string doesn't obey the matchers
		cmd.Expect("Output", expectedCmd).Return(rsyncStdout(), nil)

		bctx := newBackupContext(tmpDir)
		eChan := make(chan OperationProgress, 10)
		om := NewObjectManifest()

		op := newPathCopyOperation("copy", tmpDir, cmd, matchers)

		err := op.Backup(bctx, om, eChan)
		require.NoError(t, err, "execute should not return an error")
		cmd.AssertAllCalled()
	})

	t.Run("creates destination directory", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("creates-dst-dir")
		defer cleanup()

		cmd := command.NewMockExecutor(t)
		cmd.Expect("Output", command.ExpectedCommand{}).Return(rsyncStdout(), nil)
		bctx := newBackupContext(tmpDir)
		eChan := make(chan OperationProgress, 10)

		// remove the dst dir
		dstDir := filepath.Join(tmpDir, timestamp, "some-service", "dst")
		os.RemoveAll(dstDir)
		op := newPathCopyOperation("copy", tmpDir, cmd, nil)
		om := NewObjectManifest()

		err := op.Backup(bctx, om, eChan)
		require.NoError(t, err, "execute should not return an error")

		// make sure it's there
		_, err = os.Stat(dstDir)
		require.NoError(t, err, "destination dir was not created")
		cmd.AssertAllCalled()
	})

	t.Run("publishes operation progress to the channel", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("updates-channel")
		defer cleanup()

		cmd := command.NewMockExecutor(t)
		cmd.Expect("Output", command.ExpectedCommand{}).Return(rsyncStdout(), nil)
		bctx := newBackupContext(tmpDir)
		eChan := make(chan OperationProgress, 10)
		om := NewObjectManifest()

		op := newPathCopyOperation("publish", tmpDir, cmd, nil)
		err := op.Backup(bctx, om, eChan)
		require.NoError(t, err, "execute should not return an error")

		var e OperationProgress
		select {
		case e = <-eChan:
		default:
		}
		cmd.AssertAllCalled()
		assert.Equal(t, "publish", e.Name, "operation progress channel did not receive updates")
	})

	t.Run("respects the context", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("updates-channel")
		defer cleanup()

		cmd := command.NewMockExecutor(t)
		ctx, cancel := context.WithCancel(context.Background())
		bctx := newBackupContext(tmpDir)
		bctx.ctx = ctx
		eChan := make(chan OperationProgress, 10)
		om := NewObjectManifest()

		op := newPathCopyOperation("copy", tmpDir, cmd, nil)

		// cancel the context before we start the operation
		cancel()
		err := op.Backup(bctx, om, eChan)
		require.Error(t, err, "does not return the context error")
	})
}

// TestMetadataWriterOperation tests the metadata writer
func TestMetadataWriterOperationBackup(t *testing.T) {
	logrus.SetOutput(ioutil.Discard)
	t.Run("marshals metadata to metadata.json", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("writes-metadata")
		defer cleanup()
		expectedJSONPath := filepath.Join(tmpDir, timestamp, "dst", "metadata.json")

		bctx := newBackupContext(tmpDir)
		eChan := make(chan OperationProgress, 10)
		op := newMetadataWriterOperation("writes-metadata")
		om := NewObjectManifest()

		om.blobSHA256s.Store("some-service/dst/a", "file-a-sha256")
		om.blobSHA256s.Store("some-service/dst/b", "file-b-sha256")

		err := op.Backup(bctx, om, eChan)
		require.NoError(t, err, "execute should not return an error")

		data, err := ioutil.ReadFile(expectedJSONPath)
		require.NoError(t, err, "metadata.json was not written to correct location")

		metadata := &Metadata{}
		err = json.Unmarshal(data, metadata)
		require.NoError(t, err, "could not marshal metadata.json onto Metadata struct")

		assert.Equal(t, "writes-metadata", metadata.Spec.Name, "marshaled name is not correct")

		expectedSHAs := map[string]string{
			"some-service/dst/a": "file-a-sha256",
			"some-service/dst/b": "file-b-sha256",
		}
		assert.Equal(t, expectedSHAs, metadata.ContentsSHA256)
	})

	t.Run("creates destination directory", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("create-dir")
		defer cleanup()

		expectedDestDir := filepath.Join(tmpDir, timestamp, "dst")
		// remove the dst dir
		os.RemoveAll(expectedDestDir)

		bctx := newBackupContext(tmpDir)
		eChan := make(chan OperationProgress, 10)
		om := NewObjectManifest()
		op := newMetadataWriterOperation("create-dir")

		err := op.Backup(bctx, om, eChan)
		require.NoError(t, err, "execute should not return an error")

		// make sure it's there
		_, err = os.Stat(expectedDestDir)
		require.NoError(t, err, "destination dir was not created")
	})

	t.Run("publishes operation progress to the channel", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("publish-progress")
		defer cleanup()

		bctx := newBackupContext(tmpDir)
		eChan := make(chan OperationProgress, 10)
		om := NewObjectManifest()
		op := newMetadataWriterOperation("publish-progress")

		err := op.Backup(bctx, om, eChan)
		require.NoError(t, err, "execute should not return an error")

		var e OperationProgress
		select {
		case e = <-eChan:
		default:
		}

		assert.Equal(t, "publish-progress", e.Name, "operation progress channel did not receive updates")
	})

	t.Run("respects the context", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("publish-progress")
		defer cleanup()

		ctx, cancel := context.WithCancel(context.Background())
		bctx := newBackupContext(tmpDir)
		bctx.ctx = ctx
		eChan := make(chan OperationProgress, 10)
		om := NewObjectManifest()

		op := newMetadataWriterOperation("publish-progress")

		// cancel the context before we start the operation
		cancel()
		err := op.Backup(bctx, om, eChan)
		require.Error(t, err, "does not return the context error")
	})
}

type mockBucket struct {
	reader BlobReader
	writer BlobWriter
}

func (m *mockBucket) NewReader(ctx context.Context, objectName string, verifier ObjectVerifier) (io.ReadCloser, error) {
	return m.reader, nil
}

func (m *mockBucket) NewWriter(ctx context.Context, objectName string) (BlobWriter, error) {
	return m.writer, nil
}

func (m *mockBucket) List(ctx context.Context, objectNamePrefix string, delimited bool) ([]BucketObject, []SharedPrefix, error) {
	return nil, nil, fmt.Errorf("not implemented")
}

func (m *mockBucket) Delete(ctx context.Context, objects []string) error {
	return fmt.Errorf("not implemented")
}

type devnullWriter struct {
	io.Writer
}

func (w *devnullWriter) Close() error {
	return nil
}

func (w *devnullWriter) Fail(err error) error {
	return err
}

func (w *devnullWriter) BlobSHA256() string {
	return "TEST_CHECKSUM_devnullWriter"
}

func newDevnullWriter() BlobWriter {
	return &devnullWriter{ioutil.Discard}
}

// TestCommandExecuteOperation tests the command execute operation
func TestCommandExecuteOperationBackup(t *testing.T) {
	logrus.SetOutput(ioutil.Discard)

	t.Run("runs the command", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("runs-command")
		defer cleanup()

		objectName := []string{"some-service", "dst"}
		//expectedDstDir := filepath.Join(tmpDir, timestamp, "some-service", "dst")

		origin := "override"
		pkg := "my-couchdb"
		name := "couchdb"
		cmd := newTestCmd()
		cmdExec := command.NewMockExecutor(t)
		bctx := newBackupContext(tmpDir)
		writer := newDevnullWriter()
		bctx.bucket = &mockBucket{
			writer: writer,
		}
		eChan := make(chan OperationProgress, 10)
		om := NewObjectManifest()

		op := newCommandExecuteOperation(name, objectName, cmd, origin, pkg, cmdExec)

		expected := command.ExpectedCommand{
			Cmd: "hab",
			Args: []string{
				"pkg", "exec", origin + "/" + pkg,
				"couchdb", "dump",
			},
			Stdout: writer,
		}
		cmdExec.Expect("Run", expected).Return("dumping -> /mnt/backup/couchdb", nil)

		err := op.Backup(bctx, om, eChan)
		require.NoError(t, err, "execute should not return an error")
		cmdExec.AssertAllCalled()

		expectedSHAs := map[string]string{"some-service/dst/couchdb": "TEST_CHECKSUM_devnullWriter"}
		assert.Equal(t, expectedSHAs, om.ObjectSHA256s())
	})

	t.Run("creates destination directory", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("creates-dest-dir")
		defer cleanup()

		objectName := []string{"some-service", "dst"}
		expectedDstDir := filepath.Join(tmpDir, timestamp, "some-service", "dst")
		// remove the dst path
		os.RemoveAll(expectedDstDir)
		origin := "override"
		name := "couchdb"
		pkg := "my-couchdb"
		cmd := newTestCmd()
		cmdExec := command.NewMockExecutor(t)
		cmdExec.Expect("Run", command.ExpectedCommand{}).Return("dumping -> /mnt/backup/couchdb", nil)
		bctx := newBackupContext(tmpDir)
		eChan := make(chan OperationProgress, 10)
		om := NewObjectManifest()

		op := newCommandExecuteOperation(name, objectName, cmd, origin, pkg, cmdExec)

		err := op.Backup(bctx, om, eChan)
		require.NoError(t, err, "execute should not return an error")

		// make sure it's there
		_, err = os.Stat(expectedDstDir)
		require.NoError(t, err, "destination path was not created")
	})

	t.Run("publishes operation progress to the channel", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("publishes-progress")
		defer cleanup()
		objectName := []string{"some-service", "dst"}
		origin := "override"
		pkg := "my-couchdb"
		name := "publish-progress"
		cmd := newTestCmd()
		cmdExec := command.NewMockExecutor(t)
		cmdExec.Expect("Run", command.ExpectedCommand{}).Return("dumping -> /mnt/backup/couchdb", nil)
		bctx := newBackupContext(tmpDir)
		eChan := make(chan OperationProgress, 10)
		om := NewObjectManifest()

		op := newCommandExecuteOperation(name, objectName, cmd, origin, pkg, cmdExec)

		err := op.Backup(bctx, om, eChan)
		require.NoError(t, err, "execute should not return an error")

		var e OperationProgress
		select {
		case e = <-eChan:
		default:
		}

		assert.Equal(t, "publish-progress", e.Name, "operation progress channel did not receive updates")
	})

	t.Run("respects the context", func(t *testing.T) {
		tmpDir, cleanup := newTestBackupDir("publishes-progress")
		defer cleanup()
		dstPath := []string{tmpDir, "dst"}
		origin := "override"
		pkg := "my-couchdb"
		name := "publish-progress"
		cmd := newTestCmd()
		cmdExec := command.NewMockExecutor(t)
		ctx, cancel := context.WithCancel(context.Background())
		bctx := newBackupContext(tmpDir)
		bctx.ctx = ctx
		eChan := make(chan OperationProgress, 10)
		om := NewObjectManifest()

		op := newCommandExecuteOperation(name, dstPath, cmd, origin, pkg, cmdExec)

		// cancel the context before we start the operation
		cancel()
		err := op.Backup(bctx, om, eChan)
		require.Error(t, err, "does not return the context error")
	})
}

type ESProgress struct {
	progressPercentage float64
	snapshotState      es_sidecar.SnapshotState
	message            string
}

func (p ESProgress) GetProgressPercentage() float64 {
	return p.progressPercentage
}

func (p ESProgress) GetSnapshotState() es_sidecar.SnapshotState {
	return p.snapshotState
}

func (p ESProgress) GetMessage() string {
	return p.message
}

func TestESProgress(t *testing.T) {
	logrus.SetOutput(ioutil.Discard)

	t.Run("Preserves progress error message", func(t *testing.T) {
		progChan := make(chan OperationProgress, 5)
		defer close(progChan)
		err := monitorEsProgress(context.Background(), "foo", logrus.NewEntry(logrus.New()), progChan, func() (esProgress, error) {
			return ESProgress{
				progressPercentage: 1,
				message:            "badness",
				snapshotState:      es_sidecar.SnapshotState_FAILED,
			}, nil
		})
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "badness")
		require.Equal(t, 1, len(progChan))
		prog := <-progChan
		assert.Equal(t, "foo", prog.Name)
		assert.Equal(t, float64(0), prog.Progress)
	})

	t.Run("Returns successfully on SnapshotState_SUCCESS", func(t *testing.T) {
		progChan := make(chan OperationProgress, 5)
		defer close(progChan)
		err := monitorEsProgress(context.Background(), "foo", logrus.NewEntry(logrus.New()), progChan, func() (esProgress, error) {
			return ESProgress{
				progressPercentage: 1,
				message:            "all good",
				snapshotState:      es_sidecar.SnapshotState_SUCCESS,
			}, nil
		})
		assert.NoError(t, err)
		require.Equal(t, 2, len(progChan))
		prog := <-progChan
		assert.Equal(t, "foo", prog.Name)
		assert.Equal(t, float64(0), prog.Progress)
		prog = <-progChan
		assert.Equal(t, "foo", prog.Name)
		assert.Equal(t, float64(100), prog.Progress)
	})

	t.Run("Partial progress", func(t *testing.T) {
		progChan := make(chan OperationProgress, 5)
		defer close(progChan)
		count := 0
		err := monitorEsProgress(context.Background(), "foo", logrus.NewEntry(logrus.New()), progChan, func() (esProgress, error) {
			if count == 0 {
				count++
				return ESProgress{
					progressPercentage: 50.0,
					message:            "all good",
					snapshotState:      es_sidecar.SnapshotState_IN_PROGRESS,
				}, nil
			}
			return ESProgress{
				progressPercentage: 1,
				message:            "all good",
				snapshotState:      es_sidecar.SnapshotState_SUCCESS,
			}, nil
		})
		assert.NoError(t, err)
		require.Equal(t, 3, len(progChan))
		prog := <-progChan
		assert.Equal(t, "foo", prog.Name)
		assert.Equal(t, float64(0), prog.Progress)
		prog = <-progChan
		assert.Equal(t, "foo", prog.Name)
		assert.Equal(t, float64(0.5)*100, prog.Progress)
		prog = <-progChan
		assert.Equal(t, "foo", prog.Name)
		assert.Equal(t, float64(100), prog.Progress)
	})

	t.Run("Retries errors", func(t *testing.T) {
		progChan := make(chan OperationProgress, 5)
		defer close(progChan)
		count := 0
		err := monitorEsProgress(context.Background(), "foo", logrus.NewEntry(logrus.New()), progChan, func() (esProgress, error) {
			if count == 0 {
				count++
				return nil, errors.New("badness")
			}
			return ESProgress{
				progressPercentage: 1,
				message:            "all good",
				snapshotState:      es_sidecar.SnapshotState_SUCCESS,
			}, nil
		})
		assert.NoError(t, err)
		require.Equal(t, 2, len(progChan))
		prog := <-progChan
		assert.Equal(t, "foo", prog.Name)
		assert.Equal(t, float64(0), prog.Progress)
		prog = <-progChan
		assert.Equal(t, "foo", prog.Name)
		assert.Equal(t, float64(100), prog.Progress)
	})

	t.Run("Gives up after too many errors", func(t *testing.T) {
		progChan := make(chan OperationProgress, 5)
		defer close(progChan)
		count := 0
		err := monitorEsProgress(context.Background(), "foo", logrus.NewEntry(logrus.New()), progChan, func() (esProgress, error) {
			count++
			return nil, errors.New("badness")

		})
		assert.Error(t, err)
		require.Equal(t, 1, len(progChan))
		assert.Contains(t, err.Error(), "badness")
		prog := <-progChan
		assert.Equal(t, "foo", prog.Name)
		assert.Equal(t, float64(0), prog.Progress)
	})
}
