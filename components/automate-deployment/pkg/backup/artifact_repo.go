package backup

import (
	"context"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"

	"github.com/chef/automate/lib/stringutils"
	"github.com/pkg/errors"
)

type ArtifactRepo struct {
	artifactsRoot Bucket
	snapshotsRoot Bucket
}

func NewArtifactRepo(backupLocationSpec LocationSpecification) *ArtifactRepo {
	return &ArtifactRepo{
		artifactsRoot: backupLocationSpec.ToBucket("shared/builder/artifacts"),
		snapshotsRoot: backupLocationSpec.ToBucket("shared/builder/snapshots"),
	}
}

type ArtifactRepoSnapshot interface {
	Name() string
	List() ArtifactStream
	SHA256() string
}

type loggingStream struct {
	stream ArtifactStream
	out    io.Writer
}

func (s *loggingStream) Next() (string, error) {
	next, err := s.stream.Next()
	if err != nil {
		return "", err
	}

	if _, err := fmt.Fprintln(s.out, next); err != nil {
		return "", err
	}

	return next, nil
}

func (s *loggingStream) Close() error {
	return s.stream.Close()
}

func NewLoggingStream(stream ArtifactStream, out io.Writer) ArtifactStream {
	return &loggingStream{
		stream: stream,
		out:    out,
	}
}

type uploadSnapshotArtifactIterator struct {
	ctx               context.Context
	artifactsToUpload ArtifactStream
	snapshotTmpFile   *os.File
	src               Bucket
}

func newUploadSnapshotArtifactIterator(ctx context.Context, src Bucket,
	requiredArtifacts ArtifactStream, artifactsInRepo ArtifactStream) (*uploadSnapshotArtifactIterator, error) {

	snapshotTmpFile, err := ioutil.TempFile("", "snapshot-new")
	if err != nil {
		return nil, err
	}

	// TODO: check to make sure requiredArtifacts is read until EOF
	artifactsToUpload := Sub(NewLoggingStream(requiredArtifacts, snapshotTmpFile), artifactsInRepo)

	return &uploadSnapshotArtifactIterator{
		ctx:               ctx,
		artifactsToUpload: artifactsToUpload,
		snapshotTmpFile:   snapshotTmpFile,
		src:               src,
	}, nil
}

func (b *uploadSnapshotArtifactIterator) Next() (BlobUploadRequest, error) {
	nextKey, err := b.artifactsToUpload.Next()
	if err != nil {
		return BlobUploadRequest{}, err
	}

	reader, err := b.src.NewReader(b.ctx, nextKey, &NoOpObjectVerifier{})
	if err != nil {
		return BlobUploadRequest{}, err
	}
	return BlobUploadRequest{
		Key:    nextKey,
		Reader: reader,
	}, nil
}

func (b *uploadSnapshotArtifactIterator) ReadSnapshotFile() (io.Reader, error) {
	_, err := b.snapshotTmpFile.Seek(0, os.SEEK_SET)
	if err != nil {
		return nil, err
	}

	return b.snapshotTmpFile, nil
}

func (b *uploadSnapshotArtifactIterator) Close() error {
	b.snapshotTmpFile.Close()
	os.Remove(b.snapshotTmpFile.Name())
	return nil
}

func (repo *ArtifactRepo) Snapshot(ctx context.Context, name string,
	srcBucket Bucket, requiredArtifacts ArtifactStream) (ArtifactRepoSnapshot, error) {

	r, err := repo.openSnapshotFile(ctx, name)
	if err != nil {
		if !IsNotExist(err) {
			return nil, err
		}
	} else {
		r.Close()
		return nil, errors.New("Snapshot already exists")
	}

	artifactsInRepo := repo.ListArtifacts(ctx)
	defer artifactsInRepo.Close()

	uploadIterator, err := newUploadSnapshotArtifactIterator(ctx, srcBucket, requiredArtifacts, artifactsInRepo)
	if err != nil {
		return nil, err
	}
	defer uploadIterator.Close() // nolint: errcheck

	uploader := NewBulkUploader(repo.artifactsRoot, "")
	if err := uploader.Upload(ctx, uploadIterator); err != nil {
		return nil, err
	}

	snapshotFileReader, err := uploadIterator.ReadSnapshotFile()
	if err != nil {
		return nil, err
	}

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	snapshotFileWriter, err := repo.snapshotsRoot.NewWriter(ctx, fmt.Sprintf("%s.snapshot", name))
	if err != nil {
		return nil, err
	}

	if _, err := io.Copy(snapshotFileWriter, snapshotFileReader); err != nil {
		return nil, snapshotFileWriter.Fail(err)
	}

	if err := snapshotFileWriter.Close(); err != nil {
		return nil, err
	}

	return nil, nil
}

type restoreOptions struct {
	validateChecksum bool
}

type RestoreOpts func(*restoreOptions)

func ArtifactRepoRestoreValidateChecksum(checksum string) RestoreOpts {
	return func(r *restoreOptions) {
		r.validateChecksum = true
	}
}

func (repo *ArtifactRepo) Restore(ctx context.Context, dstBucket Bucket, name string, optFuncs ...RestoreOpts) error {
	opts := restoreOptions{}
	for _, o := range optFuncs {
		o(&opts)
	}
	if opts.validateChecksum {
		// TODO: checksum validation
		panic("validate checksum unimplemented")
	}

	snapshotFilesReader, err := repo.openSnapshotFile(ctx, name)
	if err != nil {
		return err
	}

	artifactsToRestore := LineReaderStream(snapshotFilesReader)
	defer artifactsToRestore.Close()

	uploader := NewBulkUploader(dstBucket, "")

	uploadIterator, err := newUploadSnapshotArtifactIterator(ctx, repo.artifactsRoot,
		artifactsToRestore, EmptyStream())
	if err != nil {
		return err
	}

	if err := uploader.Upload(ctx, uploadIterator); err != nil {
		return err
	}

	return nil
}

func (repo *ArtifactRepo) Remove(ctx context.Context, name string) error {
	snapshotFilesReader, err := repo.openSnapshotFile(ctx, name)
	if err != nil {
		return err
	}

	snapshotFiles := LineReaderStream(snapshotFilesReader)
	defer snapshotFiles.Close()

	// TODO[integrity]: We should really 2 phase commit this change. As it stands now, if
	// we remove this file and then fail to remove the artifacts, they will be
	// leaked and we'll have no way to clean them up
	if err := repo.removeSnapshotFile(ctx, name); err != nil {
		return err
	}

	// Get the artifacts in all the snapshots excluding the one we're trying to delete
	remainingSnapshotsFiles := repo.ListArtifacts(ctx, name)
	defer remainingSnapshotsFiles.Close()

	// Remove artifacts from the snapshot we're trying to exist which exist in other
	// snapshots
	filesToDelete := Sub(snapshotFiles, remainingSnapshotsFiles)
	defer filesToDelete.Close()

	bulkDeleter := NewBulkDeleter(repo.artifactsRoot, "")
	return bulkDeleter.Delete(ctx, filesToDelete)
}

func (repo *ArtifactRepo) ListArtifacts(ctx context.Context, excludedSnapshots ...string) ArtifactStream {
	snapshots, _, err := repo.snapshotsRoot.List(ctx, "", false)
	if err != nil {
		return ErrStream(err)
	}

	streams := make([]ArtifactStream, 0, len(snapshots))
	for _, snapshot := range snapshots {
		if !strings.HasSuffix(snapshot.Name, ".snapshot") {
			continue
		}
		name := strings.TrimSuffix(snapshot.Name, ".snapshot")
		if stringutils.SliceContains(excludedSnapshots, name) {
			continue
		}
		reader, err := repo.openSnapshotFile(ctx, name)
		if err != nil {
			for _, s := range streams {
				s.Close()
			}
			return ErrStream(err)
		}
		streams = append(streams, LineReaderStream(reader))
	}

	return Merge(streams...)
}

func (repo *ArtifactRepo) openSnapshotFile(ctx context.Context, name string) (io.ReadCloser, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	reader, err := repo.snapshotsRoot.NewReader(ctx, fmt.Sprintf("%s.snapshot", name), &NoOpObjectVerifier{})
	if err != nil {
		return nil, err
	}

	tmpFile, err := ioutil.TempFile("", fmt.Sprintf("snapshot-%s", name))
	if err != nil {
		return nil, err
	}

	if err := os.Remove(tmpFile.Name()); err != nil {
		tmpFile.Close()
		return nil, err
	}

	if _, err := io.Copy(tmpFile, reader); err != nil {
		tmpFile.Close()
		return nil, err
	}

	if _, err := tmpFile.Seek(0, os.SEEK_SET); err != nil {
		tmpFile.Close()
		return nil, err
	}

	return tmpFile, nil
}

func (repo *ArtifactRepo) removeSnapshotFile(ctx context.Context, name string) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	return repo.snapshotsRoot.Delete(ctx, []string{fmt.Sprintf("%s.snapshot", name)})
}
