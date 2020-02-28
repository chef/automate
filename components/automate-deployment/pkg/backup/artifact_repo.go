package backup

import (
	"bytes"
	"compress/gzip"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"regexp"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"go.uber.org/multierr"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/stringutils"
)

var ErrSnapshotExists error = errors.New("Snapshot already exists")

const SnapshotIntegrityMetadataFileName = "snapshot.integrity"

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
	Name     string `json:"name"`
	Checksum string `json:"checksum"`
}

// uploadSnapshotArtifactIterator is used to sync files from one bucket to another.
// It is provided a list of artifacts that are in the dst bucket, along with a list
// of artifacts that are required from the src bucket. Only the missing artifacts
// will be uploaded.
// It is also capable of writing out the snapshot file once the artifacts have been
// synced
type uploadSnapshotArtifactIterator struct {
	ctx                      context.Context
	artifactsToUpload        ArtifactStream
	snapshotTmpFile          *os.File
	artifactsToUploadTmpFile *os.File
	numArtifactsToUpload     int64
	src                      Bucket
}

func logClose(c io.Closer, msg string) {
	fileutils.LogClose(c, logrus.StandardLogger(), msg)
}

func logErr(err error, msg string) {
	if err != nil {
		logrus.WithError(err).Error(msg)
	}
}

func newUploadSnapshotArtifactIterator(ctx context.Context, src Bucket,
	requiredArtifacts ArtifactStream, artifactsInRepo ArtifactStream) (*uploadSnapshotArtifactIterator, error) {

	artifactsToUploadTmpFile, err := ioutil.TempFile("", "artifacts-to-upload")
	if err != nil {
		return nil, err
	}
	if err := os.Remove(artifactsToUploadTmpFile.Name()); err != nil {
		logClose(artifactsToUploadTmpFile, "failed to close temporary file")
		return nil, err
	}

	snapshotTmpFile, err := ioutil.TempFile("", "snapshot-new")
	if err != nil {
		logClose(artifactsToUploadTmpFile, "failed to close temporary file")
		return nil, err
	}

	// We're going to write out the required artifacts and artifacts to upload to temp files.
	// We need to do this because we want to get the number of artifacts to upload to report
	// progress
	artifactsToUploadCounter := NewCountingStream(
		Sub(NewLoggingStream(requiredArtifacts, snapshotTmpFile), artifactsInRepo))
	defer logClose(artifactsToUploadCounter, "failed to close counting stream")

	artifactsToUploadWriter := NewLoggingStream(
		artifactsToUploadCounter,
		artifactsToUploadTmpFile)
	defer logClose(artifactsToUploadWriter, "failed to close logging stream")

	if err := ConsumeStream(artifactsToUploadWriter); err != nil {
		logClose(artifactsToUploadTmpFile, "failed to close temporary file")
		logClose(snapshotTmpFile, "failed to close temporary file")
		return nil, err
	}
	if err := artifactsToUploadWriter.Close(); err != nil {
		logClose(artifactsToUploadTmpFile, "failed to close temporary file")
		logClose(snapshotTmpFile, "failed to close temporary file")
		return nil, err
	}
	if _, err := artifactsToUploadTmpFile.Seek(0, os.SEEK_SET); err != nil {
		logClose(artifactsToUploadTmpFile, "failed to close temporary file")
		logClose(snapshotTmpFile, "failed to close temporary file")
		return nil, err
	}

	artifactsToUpload := NewLineReaderStream(artifactsToUploadTmpFile)

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

func (b *uploadSnapshotArtifactIterator) WriteSnapshotFile(w io.Writer) (string, error) {
	if _, err := b.snapshotTmpFile.Seek(0, os.SEEK_SET); err != nil {
		return "", err
	}

	checksum := sha256.New()
	m := io.MultiWriter(w, checksum)
	g := gzip.NewWriter(m)

	if _, err := io.Copy(g, b.snapshotTmpFile); err != nil {
		return "", err
	}

	if err := g.Close(); err != nil {
		return "", err
	}

	return hex.EncodeToString(checksum.Sum(nil)), nil
}

func (b *uploadSnapshotArtifactIterator) Close() error {
	err1 := errors.Wrap(b.snapshotTmpFile.Close(), "failed to close temporary file")
	err2 := errors.Wrap(b.artifactsToUpload.Close(), "failed to close artifact stream")
	err3 := errors.Wrap(os.Remove(b.snapshotTmpFile.Name()), "failed to close temporary file")
	return multierr.Combine(err1, err2, err3)
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
		logClose(r, "failed to close snapshot file reader")
		return ArtifactRepoSnapshotMetadata{}, ErrSnapshotExists
	}

	artifactsInRepo := repo.ListAssumedArtifacts(ctx)
	defer logClose(artifactsInRepo, "failed to close artifact list stream")

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

	snapshotFileWriter, err := repo.snapshotsRoot.NewWriter(ctx, toSnapshotName(name))
	if err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}

	checksum, err := uploadIterator.WriteSnapshotFile(snapshotFileWriter)
	if err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}

	if err := snapshotFileWriter.Close(); err != nil {
		return ArtifactRepoSnapshotMetadata{}, err
	}

	return ArtifactRepoSnapshotMetadata{
		Name:     name,
		Checksum: checksum,
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

	artifactsToRestore, checksum, err := repo.openSnapshotFile(ctx, name)
	if err != nil {
		return err
	}
	defer logClose(artifactsToRestore, "failed to close snapshot file reader")

	if opts.validateChecksum && checksum != opts.checksum {
		// TODO(jaym): We probably need a specific error type for this
		return errors.New("Invalid checksum")
	}

	uploader := NewBulkUploader(dstBucket, "")

	uploadIterator, err := newUploadSnapshotArtifactIterator(ctx, repo.artifactsRoot,
		artifactsToRestore, EmptyStream())
	if err != nil {
		return err
	}
	defer uploadIterator.Close() // nolint: errcheck

	if err := uploader.Upload(ctx, uploadIterator, opts.progress); err != nil {
		return err
	}

	return nil
}

func (repo *ArtifactRepo) Remove(ctx context.Context, name string) error {
	snapshotFiles, _, err := repo.openSnapshotFile(ctx, name)
	if err != nil {
		return err
	}
	defer logClose(snapshotFiles, "failed to close snapshot stream")

	// TODO[integrity]: We should really 2 phase commit this change. As it stands now, if
	// we remove this file and then fail to remove the artifacts, they will be
	// leaked and we'll have no way to clean them up
	if err := repo.removeSnapshotFile(ctx, name); err != nil {
		return err
	}

	if err := repo.removeFromSnapshotIntegrity(ctx, name); err != nil {
		return err
	}

	// Get the artifacts in all the snapshots excluding the one we're trying to delete
	remainingSnapshotsFiles := repo.ListAssumedArtifacts(ctx, name)
	defer logClose(remainingSnapshotsFiles, "failed to close artifact list stream")

	// Remove artifacts from the snapshot we're trying to exist which exist in other
	// snapshots
	filesToDelete := Sub(snapshotFiles, remainingSnapshotsFiles)
	defer logClose(filesToDelete, "failed to close fileToDelete stream")

	bulkDeleter := NewBulkDeleter(repo.artifactsRoot, "")
	return bulkDeleter.Delete(ctx, filesToDelete)
}

// ListAssumedArtifacts aggregates all the required artifacts from all snapshots
// and returns a stream. It is presumed to be much cheaper to scan every snapshot
// metadata file and aggregate the artifacts rather than list every object that
// is currently in the repo. The downside of course is that we aren't sure that
// such objects exist.
func (repo *ArtifactRepo) ListAssumedArtifacts(ctx context.Context, excludedSnapshots ...string) ArtifactStream {
	var lastMergedStream ArtifactStream

	// Merge as we go in order to avoid running out of file descriptors and/or stack
	// in the case where someone has a lot of snapshots.
	snapshots, err := repo.snapshots(ctx, ExcludeSnapshots(excludedSnapshots))
	if err != nil {
		return ErrStream(err)
	}

	for _, snapshotName := range snapshots {
		snapshotStream, _, err := repo.openSnapshotFile(ctx, snapshotName)
		if err != nil {
			return ErrStream(err)
		}
		if lastMergedStream == nil {
			lastMergedStream = snapshotStream
		} else {
			lastMergedStream, err = mergeIntoFile(lastMergedStream, snapshotStream)
			if err != nil {
				return ErrStream(err)
			}
		}
	}

	if lastMergedStream == nil {
		return EmptyStream()
	}

	return lastMergedStream
}

// ListArtifacts returns a stream of the actual object names from the bucket.
func (repo *ArtifactRepo) ListArtifacts(ctx context.Context) ArtifactStream {
	stream, err := NewBucketObjectNameStream(ctx, repo.artifactsRoot, "")
	if err != nil {
		return ErrStream(err)
	}

	return stream
}

func snapshotsInBucket(objects []BucketObject, filters ...FilterSnapshotOpt) ([]string, error) {
	options := &filterSnapshotOptions{
		exclude: []string{},
		only:    []string{},
	}
	snapshots := []string{}

	for _, o := range filters {
		o(options)
	}

	for _, object := range objects {
		if !strings.HasSuffix(object.Name, ".snapshot") {
			continue
		}
		name := strings.TrimSuffix(object.Name, ".snapshot")

		if stringutils.SliceContains(options.exclude, name) {
			continue
		}

		snapshots = append(snapshots, name)
	}

	if len(options.only) > 0 {
		for _, required := range options.only {
			if !stringutils.SliceContains(snapshots, required) {
				return snapshots, errors.Errorf("missing desired snapshot %s", required)
			}
		}

		return options.only, nil
	}

	return snapshots, nil
}

type filterSnapshotOptions struct {
	exclude []string
	only    []string
}

type FilterSnapshotOpt func(*filterSnapshotOptions)

func ExcludeSnapshots(snapshots []string) FilterSnapshotOpt {
	return func(r *filterSnapshotOptions) {
		r.exclude = snapshots
	}
}

func OnlySnapshots(snapshots []string) FilterSnapshotOpt {
	return func(r *filterSnapshotOptions) {
		r.only = snapshots
	}
}

// Snapshots filters through the repo's snapshot bucket and returns
// a slice of snapshot names
func (repo *ArtifactRepo) snapshots(ctx context.Context, filters ...FilterSnapshotOpt) ([]string, error) {
	// Gather all snapshots names we
	bucketObjects, _, err := repo.snapshotsRoot.List(ctx, "", false)
	if err != nil {
		return nil, err
	}

	return snapshotsInBucket(bucketObjects, filters...)
}

// ValidateSnapshotIntegrity validates snapshot integrity by scanning the repo
// artifacts list and ensuring that each snapshot's required artifacts are
// present.
func (repo *ArtifactRepo) ValidateSnapshotIntegrity(ctx context.Context, filters ...FilterSnapshotOpt) error {
	replayableArtifactsStream, err := NewReplayableStream(repo.ListArtifacts(ctx))
	if err != nil {
		return errors.Wrap(err, "gathering artifact list")
	}
	defer logClose(replayableArtifactsStream, "close replayable artifacts stream")

	integrityMetadata, err := repo.ReadSnapshotIntegrityMetadata(ctx)
	if err != nil {
		return err
	}

	snapshots, err := repo.snapshots(ctx, filters...)
	if err != nil {
		return err
	}

	for _, snapshotName := range snapshots {
		logrus.WithField("snapshot", snapshotName).Info("Validating snapshot integrity")
		snapshotStream, _, err := repo.openSnapshotFile(ctx, snapshotName)
		if err != nil {
			return err
		}

		// Reset the replayable artifact stream each time so that we can
		// replay it.
		if err = replayableArtifactsStream.Reset(); err != nil {
			return err
		}

		defer logClose(snapshotStream, "close snapshot stream")
		if err != nil {
			return err
		}

		missing := Sub(snapshotStream, replayableArtifactsStream)

		if err = repo.updateSnapshotIntegrityMetadata(ctx, integrityMetadata, snapshotName, missing); err != nil {
			return err
		}
	}

	return repo.writeSnapshotIntegrityMetadata(ctx, integrityMetadata)
}

type ArtifactRepoIntegrityMetadata struct {
	Snapshots map[string]ArtifactRepoSnapshotIntegrityMetadata `json:"snapshots"`
}

func NewArtifactRepoIntegrityMetadata() *ArtifactRepoIntegrityMetadata {
	return &ArtifactRepoIntegrityMetadata{
		Snapshots: map[string]ArtifactRepoSnapshotIntegrityMetadata{},
	}
}

type ArtifactRepoSnapshotIntegrityMetadata struct {
	LastVerified string   `json:"last_verified"`
	Missing      []string `json:"missing"`
	Corrupted    bool     `json:"corrupted"`
}

func NewArtifactRepoSnapshotIntegrityMetadata() ArtifactRepoSnapshotIntegrityMetadata {
	return ArtifactRepoSnapshotIntegrityMetadata{
		Missing: []string{},
	}
}

// updateSnapshotIntegrityMetadata consumes a missing artifact stream for a given
// snapshot and pdates an in memory repo integrity metadata with a new snapshot
// integrity metadata result.
func (repo *ArtifactRepo) updateSnapshotIntegrityMetadata(
	ctx context.Context,
	currentMetadata *ArtifactRepoIntegrityMetadata,
	snapshotName string,
	missingArtifacts ArtifactStream) error {

	defer logClose(missingArtifacts, "close missing stream")

	metadata, exists := currentMetadata.Snapshots[snapshotName]
	if !exists {
		metadata = NewArtifactRepoSnapshotIntegrityMetadata()
	} else {
		metadata.Missing = []string{}
	}

	metadata.LastVerified = time.Now().Format(api.BackupTaskFormat)
	metadata.Corrupted = false

	for {
		artifact, err := missingArtifacts.Next()
		if artifact != "" {
			metadata.Corrupted = true
			metadata.Missing = append(metadata.Missing, artifact)
		}

		if err == io.EOF {
			break
		}

		if err != nil {
			return err
		}
	}

	currentMetadata.Snapshots[snapshotName] = metadata

	return nil
}

func (repo *ArtifactRepo) removeFromSnapshotIntegrity(ctx context.Context, name string) error {
	meta, err := repo.ReadSnapshotIntegrityMetadata(ctx)
	if err != nil {
		return err
	}

	_, exists := meta.Snapshots[name]
	if !exists {
		return nil
	}

	delete(meta.Snapshots, name)
	return repo.writeSnapshotIntegrityMetadata(ctx, meta)
}

// mergeIntoFile takes 2 streams and merges them into 1. This function is not
// lazy and merges them into a file, and then returns a Stream that reads
// from the beginning of that file
func mergeIntoFile(a ArtifactStream, b ArtifactStream) (ArtifactStream, error) {
	defer logClose(a, "failed to close stream")
	defer logClose(b, "failed to close stream")

	tmpFile, err := ioutil.TempFile("", "lastMerged")
	if err != nil {
		return nil, err
	}
	defer logErr(os.Remove(tmpFile.Name()), "failed to remove temp file")

	s := NewLoggingStream(Merge(a, b), tmpFile)
	defer logClose(s, "failed to close logging stream")

	// Consume the stream to get it written to the tmpfile
	if err := ConsumeStream(s); err != nil {
		logClose(tmpFile, "failed to close temp file")
		return nil, err
	}

	// seek to the beginning of the file
	if _, err := tmpFile.Seek(0, os.SEEK_SET); err != nil {
		logClose(tmpFile, "failed to close temp file")
		return nil, err
	}

	return NewLineReaderStream(tmpFile), nil
}

// openGzipFile returns an artifact stream of an uncrompressed gzip file, the
// file contents checkum, and any error encountered.
func (repo *ArtifactRepo) openGzipFile(ctx context.Context, name string) (ArtifactStream, string, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	r, err := repo.snapshotsRoot.NewReader(ctx, name, &NoOpObjectVerifier{})
	if err != nil {
		return nil, "", err
	}

	// r is closed with reader
	reader := newChecksummingReader(r)
	defer logClose(reader, "failed to close file reader")

	tmpFile, err := ioutil.TempFile("", name)
	if err != nil {
		return nil, "", err
	}

	if err := os.Remove(tmpFile.Name()); err != nil {
		logClose(tmpFile, "failed to close temp file")
		return nil, "", err
	}

	g, err := gzip.NewReader(reader)
	if err != nil {
		logClose(tmpFile, "failed to close temp file")
		return nil, "", err
	}
	defer logClose(g, "failed to close gzip reader")

	if _, err := io.Copy(tmpFile, g); err != nil {
		logClose(tmpFile, "failed to close temp file")
		return nil, "", err
	}

	checksum := reader.BlobSHA256()

	if _, err := tmpFile.Seek(0, os.SEEK_SET); err != nil {
		logClose(tmpFile, "failed to close temp file")
		return nil, "", err
	}

	return NewLineReaderStream(tmpFile), checksum, nil
}

func (repo *ArtifactRepo) openSnapshotFile(ctx context.Context, name string) (ArtifactStream, string, error) {
	return repo.openGzipFile(ctx, toSnapshotName(name))
}

// ReadSnapshotIntegrityMetadata reads the snapshot integrity metadata file
func (repo *ArtifactRepo) ReadSnapshotIntegrityMetadata(ctx context.Context) (*ArtifactRepoIntegrityMetadata, error) {
	metadata := NewArtifactRepoIntegrityMetadata()

	metadataStream, _, err := repo.openGzipFile(ctx, SnapshotIntegrityMetadataFileName)
	if err != nil {
		if IsNotExist(err) {
			return metadata, nil
		}

		return nil, err
	}

	buff := &bytes.Buffer{}
	writer := NewLoggingStream(metadataStream, buff)
	defer logClose(writer, "failed to logging stream")

	if err := ConsumeStream(writer); err != nil {
		return nil, err
	}

	return metadata, json.Unmarshal(buff.Bytes(), metadata)
}

func (repo *ArtifactRepo) writeSnapshotIntegrityMetadata(ctx context.Context, metadata *ArtifactRepoIntegrityMetadata) error {
	bytes, err := json.Marshal(metadata)
	if err != nil {
		return err
	}

	metadataWriter, err := repo.snapshotsRoot.NewWriter(ctx, SnapshotIntegrityMetadataFileName)
	if err != nil {
		return err
	}
	defer logClose(metadataWriter, "failed to close metadata writer")

	gzipWriter := gzip.NewWriter(metadataWriter)
	defer logClose(gzipWriter, "failed to close gzip writer")

	_, err = gzipWriter.Write(bytes)
	return err
}

func (repo *ArtifactRepo) removeSnapshotFile(ctx context.Context, name string) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	return repo.snapshotsRoot.Delete(ctx, []string{toSnapshotName(name)})
}

func toSnapshotName(name string) string {
	r := regexp.MustCompile(`\w+\.snapshot`)
	if r.Match([]byte(name)) {
		return name
	}

	return fmt.Sprintf("%s.snapshot", name)
}
