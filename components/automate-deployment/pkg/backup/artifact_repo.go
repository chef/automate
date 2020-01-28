package backup

import (
	"compress/gzip"
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

type ArtifactRepoProgressReporter interface {
	ReportProgress(completed int64, total int64)
}

func NewArtifactRepo(backupLocationSpec LocationSpecification) *ArtifactRepo {
	return &ArtifactRepo{
		artifactsRoot: backupLocationSpec.ToBucket("shared/builder/artifacts"),
		snapshotsRoot: backupLocationSpec.ToBucket("shared/builder/snapshots"),
	}
}

type ArtifactRepoSnapshotMetadata struct {
	Name     string
	Checksum string
}

type uploadSnapshotArtifactIterator struct {
	ctx                      context.Context
	artifactsToUpload        ArtifactStream
	snapshotTmpFile          *os.File
	artifactsToUploadTmpFile *os.File
	numArtifactsToUpload     int64
	src                      Bucket
}

func newUploadSnapshotArtifactIterator(ctx context.Context, src Bucket,
	requiredArtifacts ArtifactStream, artifactsInRepo ArtifactStream) (*uploadSnapshotArtifactIterator, error) {

	artifactsToUploadTmpFile, err := ioutil.TempFile("", "artifacts-to-upload")
	if err != nil {
		return nil, err
	}
	if err := os.Remove(artifactsToUploadTmpFile.Name()); err != nil {
		artifactsToUploadTmpFile.Close()
		return nil, err
	}

	snapshotTmpFile, err := ioutil.TempFile("", "snapshot-new")
	if err != nil {
		artifactsToUploadTmpFile.Close()
		return nil, err
	}

	// TODO: check to make sure requiredArtifacts is read until EOF

	// We're going to write out the required artifacts and artifacts to upload to temp files.
	// We need to do this because we want to get the number of artifacts to upload to report
	// progres
	artifactsToUploadCounter := NewCountingStream(
		Sub(NewLoggingStream(requiredArtifacts, snapshotTmpFile), artifactsInRepo))
	artifactsToUploadWriter := NewLoggingStream(artifactsToUploadCounter, artifactsToUploadTmpFile)
	defer artifactsToUploadWriter.Close()

	if err := ConsumeStream(artifactsToUploadWriter); err != nil {
		artifactsToUploadTmpFile.Close()
		snapshotTmpFile.Close()
		return nil, err
	}
	if err := artifactsToUploadWriter.Close(); err != nil {
		artifactsToUploadTmpFile.Close()
		snapshotTmpFile.Close()
		return nil, err
	}
	if _, err := artifactsToUploadTmpFile.Seek(0, os.SEEK_SET); err != nil {
		artifactsToUploadTmpFile.Close()
		snapshotTmpFile.Close()
		return nil, err
	}

	artifactsToUpload := LineReaderStream(artifactsToUploadTmpFile)

	return &uploadSnapshotArtifactIterator{
		ctx:                      ctx,
		artifactsToUpload:        artifactsToUpload,
		snapshotTmpFile:          snapshotTmpFile,
		artifactsToUploadTmpFile: artifactsToUploadTmpFile,
		numArtifactsToUpload:     artifactsToUploadCounter.Count(),
		src:                      src,
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

func (b *uploadSnapshotArtifactIterator) EstimatedSize() int64 {
	return b.numArtifactsToUpload
}

func (b *uploadSnapshotArtifactIterator) WriteSnapshotFile(w io.Writer) error {
	_, err := b.snapshotTmpFile.Seek(0, os.SEEK_SET)
	if err != nil {
		return err
	}

	g := gzip.NewWriter(w)

	_, err = io.Copy(g, b.snapshotTmpFile)
	if err != nil {
		return err
	}

	err = g.Close()
	if err != nil {
		return err
	}

	return nil
}

func (b *uploadSnapshotArtifactIterator) Close() error {
	b.snapshotTmpFile.Close()
	b.artifactsToUpload.Close()
	os.Remove(b.snapshotTmpFile.Name())
	return nil
}

type artifactSnapshotOptions struct {
	progress ArtifactRepoProgressReporter
}

type ArtifactSnapshotOpt func(*artifactSnapshotOptions)

func ArtifactRepoSnapshotReportProgress(progress ArtifactRepoProgressReporter) ArtifactSnapshotOpt {
	return func(r *artifactSnapshotOptions) {
		r.progress = progress
	}
}

func (repo *ArtifactRepo) Snapshot(ctx context.Context, name string, srcBucket Bucket,
	requiredArtifacts ArtifactStream, optFuncs ...ArtifactSnapshotOpt) (ArtifactRepoSnapshotMetadata, error) {
	opts := artifactSnapshotOptions{}
	for _, o := range optFuncs {
		o(&opts)
	}

	r, _, err := repo.openSnapshotFile(ctx, name)
	if err != nil {
		if !IsNotExist(err) {
			return ArtifactRepoSnapshotMetadata{}, err
		}
	} else {
		r.Close()
		return ArtifactRepoSnapshotMetadata{}, errors.New("Snapshot already exists")
	}

	artifactsInRepo := repo.ListArtifacts(ctx)
	defer artifactsInRepo.Close()

	uploadIterator, err := newUploadSnapshotArtifactIterator(ctx, srcBucket, requiredArtifacts, artifactsInRepo)
	if err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}
	defer uploadIterator.Close() // nolint: errcheck

	uploader := NewBulkUploader(repo.artifactsRoot, "")
	if err := uploader.Upload(ctx, uploadIterator, opts.progress); err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	snapshotFileWriter, err := repo.snapshotsRoot.NewWriter(ctx, fmt.Sprintf("%s.snapshot", name))
	if err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}

	err = uploadIterator.WriteSnapshotFile(snapshotFileWriter)
	if err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}

	// we don't need to close this as it gets closed with uploadIterator
	//snapshotFileReader := newChecksummingReader(ioutil.NopCloser(_snapshotFileReader))

	if err := snapshotFileWriter.Close(); err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}

	return ArtifactRepoSnapshotMetadata{
		Name:     name,
		Checksum: "",
	}, nil
}

type artifactRestoreOptions struct {
	validateChecksum bool
	checksum         string
	progress         ArtifactRepoProgressReporter
}

type ArtifactRestoreOpt func(*artifactRestoreOptions)

func ArtifactRepoRestoreValidateChecksum(checksum string) ArtifactRestoreOpt {
	return func(r *artifactRestoreOptions) {
		r.validateChecksum = true
		r.checksum = checksum
	}
}

func ArtifactRepoRestoreReportProgress(progress ArtifactRepoProgressReporter) ArtifactRestoreOpt {
	return func(r *artifactRestoreOptions) {
		r.progress = progress
	}
}

func (repo *ArtifactRepo) Restore(ctx context.Context, dstBucket Bucket, name string, optFuncs ...ArtifactRestoreOpt) error {
	opts := artifactRestoreOptions{}
	for _, o := range optFuncs {
		o(&opts)
	}

	snapshotFilesReader, checksum, err := repo.openSnapshotFile(ctx, name)
	if err != nil {
		return err
	}

	if opts.validateChecksum && checksum != opts.checksum {
		return errors.New("Invalid checksum")
	}

	artifactsToRestore := LineReaderStream(snapshotFilesReader)
	defer artifactsToRestore.Close()

	uploader := NewBulkUploader(dstBucket, "")

	uploadIterator, err := newUploadSnapshotArtifactIterator(ctx, repo.artifactsRoot,
		artifactsToRestore, EmptyStream())
	if err != nil {
		return err
	}

	if err := uploader.Upload(ctx, uploadIterator, opts.progress); err != nil {
		return err
	}

	return nil
}

func (repo *ArtifactRepo) Remove(ctx context.Context, name string) error {
	snapshotFilesReader, _, err := repo.openSnapshotFile(ctx, name)
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
		reader, _, err := repo.openSnapshotFile(ctx, name)
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

func (repo *ArtifactRepo) openSnapshotFile(ctx context.Context, name string) (io.ReadCloser, string, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	r, err := repo.snapshotsRoot.NewReader(ctx, fmt.Sprintf("%s.snapshot", name), &NoOpObjectVerifier{})
	if err != nil {
		return nil, "", err
	}

	// r is closed with reader
	reader := newChecksummingReader(r)
	defer reader.Close()

	tmpFile, err := ioutil.TempFile("", fmt.Sprintf("snapshot-%s", name))
	if err != nil {
		return nil, "", err
	}

	if err := os.Remove(tmpFile.Name()); err != nil {
		tmpFile.Close()
		return nil, "", err
	}

	g, err := gzip.NewReader(reader)
	if err != nil {
		return nil, "", err
	}
	defer g.Close()

	if _, err := io.Copy(tmpFile, g); err != nil {
		tmpFile.Close()
		return nil, "", err
	}

	checksum := reader.BlobSHA256()

	if _, err := tmpFile.Seek(0, os.SEEK_SET); err != nil {
		tmpFile.Close()
		return nil, "", err
	}

	return tmpFile, checksum, nil
}

func (repo *ArtifactRepo) removeSnapshotFile(ctx context.Context, name string) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	return repo.snapshotsRoot.Delete(ctx, []string{fmt.Sprintf("%s.snapshot", name)})
}
