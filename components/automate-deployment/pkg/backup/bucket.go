package backup

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"hash"
	"io"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"sort"
	"strings"

	"go.uber.org/multierr"
	"golang.org/x/oauth2/google"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
	"gocloud.dev/blob"
	"gocloud.dev/blob/gcsblob"
	"gocloud.dev/blob/s3blob"
	"gocloud.dev/gcerrors"
	"gocloud.dev/gcp"

	"github.com/chef/automate/lib/io/fileutils"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

// BucketObject represents the metadata for a stored object
type BucketObject struct {
	// The name is the name of the object in the bucket relative to the bucket
	// path. It can be a singular object name or a relative path.
	// For example, if we instantiated an s3 bucket with the bucket name
	// of foo, and a base path of bar/baz, all access to object would
	// be implicitly relative to s3://foo/bar/baz. Calling
	// NewReader(ctx, "quuz") would access s3://foo/bar/baz/quux
	Name string
}

// SharedPrefix represents a group of object names that exist
type SharedPrefix string

// A BlobWriter is a WriteCloser that can also be failed
type BlobWriter interface {
	io.WriteCloser
	Fail(error) error
	BlobSHA256() string
}

type BlobReader interface {
	io.ReadCloser
	BlobSHA256() string
}

// A Bucket is a place where blobs can be stored
type Bucket interface {
	// NewReader returns a ReadCloser for the given storage key. The reader must be closed
	// by the caller. It is provided only when there is not an error.
	// If a object with the given name is not found, calling IsNotExist with the error
	// must return true
	NewReader(ctx context.Context, path string, verifier ObjectVerifier) (io.ReadCloser, error)

	// NewWriter returns a BlobWriter that can write to the given storage key. The data may
	// not be flushed until Close is called
	NewWriter(ctx context.Context, path string) (BlobWriter, error)

	// List returns a list of BucketObjects with the given object name prefix.
	// If there are no matching objects stored, empty lists will be returned.
	//
	// If delimited is set to true, SharedPrefix objects will be returned. Doing this is
	// equivalent to asking for a 1 level deep directory search. As an example, if pathPrefix
	// was set to 'foo/', then the directory 'foo/bar' is a possible return value, but
	// not 'foo/bar/baz'. If 'foo/quux' exists and is an object, then it would be returned in the
	// BucketObject list.
	//
	// The pathPrefix provided is assumed to end in '/'
	List(ctx context.Context, pathPrefix string, delimited bool) ([]BucketObject, []SharedPrefix, error)

	Delete(ctx context.Context, objectPaths []string) error
}

// IsNotExist returns true if the error represents a object access against a objects that
// does not exist
func IsNotExist(err error) bool {
	return os.IsNotExist(err) ||
		gcerrors.Code(err) == gcerrors.NotFound ||
		// blob.IsNotExist returns the wrong answer for minio's NoSuchKey error
		strings.Contains(err.Error(), "NoSuchKey")
}

type fsBucket struct {
	basePath string
}

// NewFilesystemBucket returns a Bucket backed by a filesystem directory
func NewFilesystemBucket(basePath string) Bucket {
	return fsBucket{
		basePath: basePath,
	}
}

type checksummingAtomicWriter struct {
	fileutils.WriteCloserFailer
	sha256 hash.Hash
}

func (w *checksummingAtomicWriter) Write(p []byte) (n int, err error) {
	w.sha256.Write(p) //nolint: errcheck // docs for hash.Hash say this never errors
	return w.WriteCloserFailer.Write(p)
}

func (w *checksummingAtomicWriter) BlobSHA256() string {
	return hex.EncodeToString(w.sha256.Sum(nil))
}

func newChecksummingAtomicWriter(writePath string, opts ...fileutils.AtomicWriteOpt) (BlobWriter, error) {
	w, err := fileutils.NewAtomicWriter(writePath, opts...)
	if err != nil {
		return nil, err
	}
	return &checksummingAtomicWriter{
		WriteCloserFailer: w,
		sha256:            sha256.New(),
	}, nil
}

type checksummingReader struct {
	io.ReadCloser
	sha256 hash.Hash
	tee    io.Reader
}

func newChecksummingReader(r io.ReadCloser) BlobReader {
	s := sha256.New()
	return &checksummingReader{
		ReadCloser: r,
		sha256:     s,
		tee:        io.TeeReader(r, s),
	}
}

func (r *checksummingReader) Read(p []byte) (int, error) {
	return r.tee.Read(p)
}

func (r *checksummingReader) BlobSHA256() string {
	return hex.EncodeToString(r.sha256.Sum(nil))
}

func (bkt fsBucket) NewReader(ctx context.Context, relPath string, verifier ObjectVerifier) (io.ReadCloser, error) {
	validatePath(relPath)
	r, err := os.Open(path.Join(bkt.basePath, relPath))
	if err != nil {
		return nil, err
	}
	cksumReader := newChecksummingReader(r)
	defer cksumReader.Close()
	return newVerifiedReader(relPath, cksumReader, verifier)
}

func (bkt fsBucket) NewWriter(ctx context.Context, relPath string) (BlobWriter, error) {
	validatePath(relPath)
	writePath := path.Join(bkt.basePath, relPath)
	writeDir := path.Dir(writePath)

	if err := os.MkdirAll(writeDir, defaultDirPerms); err != nil {
		return nil, errors.Wrapf(err, "failed to create directory %s", writeDir)
	}

	return newChecksummingAtomicWriter(writePath, fileutils.WithAtomicWriteFileMode(0600), fileutils.WithAtomicWriteNoSync(true))
}

func (bkt fsBucket) List(ctx context.Context, pathPrefix string, delimited bool) ([]BucketObject, []SharedPrefix, error) {
	validatePath(pathPrefix)

	dir := path.Join(bkt.basePath, pathPrefix)
	objs := []BucketObject{}
	sharedPrefixes := []SharedPrefix{}

	// Check if the directory where the objects would exist exists
	if _, err := os.Stat(dir); err != nil {
		if os.IsNotExist(err) {
			// If it doesn't exist, there is no error. We return empty lists
			return objs, sharedPrefixes, nil
		}
		return objs, sharedPrefixes, err
	}

	// When we're in the delimited mode, we don't need to talk the directory recursively. We
	// can list the files in the given directory (pathPrefix) and break those into the
	// BlobObject and SharedPrefix lists
	if delimited {
		dirs, err := ioutil.ReadDir(dir)
		if err != nil {
			return objs, sharedPrefixes, err
		}

		for _, d := range dirs {
			relPath := path.Join(pathPrefix, d.Name())
			if d.IsDir() {
				// Directories are not a real thing in the object world, but they represent
				// that a group of objects that exist under path name.
				//
				// NOTE: This is slightly a lie. It's possible that under d, there are only
				//       directories, or d itself is empty. We should really prune these out
				//       since no objects exist under it. In S3, this case cannot exist since
				//       it's not actually a filesystem.
				//       By not pruning, we expose a slight race condition. It is possible
				//       that the backups have started writing the status file. The directory
				//       will get created. If the backups are listed before the status file
				//       is written, the listing will be unable to distinguish between an
				//       old completed backup that did not have a status file and a new
				//       incomplete one.
				if relPath[len(relPath)-1] != '/' {
					relPath = relPath + "/"
				}
				sharedPrefixes = append(sharedPrefixes, SharedPrefix(relPath))
			} else {
				// There is an actual file there. return it as a BlobObject
				objs = append(objs, BucketObject{
					Name: relPath,
				})
			}
		}
		return objs, sharedPrefixes, nil
	}

	// If delimited mode was not asked for, we need to recursively find all the objects
	err := filepath.Walk(dir, func(p string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		// Ignore the .tmp files because they're not complete
		if strings.HasPrefix(path.Base(p), ".tmp.") {
			return nil
		}

		// Directories aren't actual things, so skip them
		if !info.IsDir() {
			relPath, err := filepath.Rel(bkt.basePath, p)
			if err != nil {
				return err
			}
			objs = append(objs, BucketObject{
				Name: relPath,
			})
		}
		return nil
	})
	return objs, sharedPrefixes, err
}

func (bkt fsBucket) Delete(_ context.Context, objectPaths []string) error {
	dirsToCheck := []string{}
	for _, objectPath := range objectPaths {
		validatePath(objectPath)

		filePath := path.Join(bkt.basePath, objectPath)
		if err := os.Remove(filePath); err != nil {
			if !os.IsNotExist(err) {
				return errors.Wrapf(err, "failed to delete %s", filePath)
			}
		} else {
			dirPath := path.Dir(filePath)
			dirsToCheck = append(dirsToCheck, dirPath)
		}
	}

	sort.Slice(dirsToCheck, func(i, j int) bool { return len(dirsToCheck[i]) > len(dirsToCheck[j]) })

	for _, dir := range dirsToCheck {
		errContainsNonDir := errors.New("contains non dir")
		err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			if !info.IsDir() {
				return errContainsNonDir
			}
			return nil
		})
		if err != nil {
			if os.IsNotExist(err) || err == errContainsNonDir {
				continue
			}
			return errors.Wrap(err, "failed to cleanup filesystem bucket")
		}
		logrus.Debugf("Deleting empty directory %s. No files found under it", dir)
		if err := os.RemoveAll(dir); err != nil {
			return errors.Wrapf(err, "failed to delete %s", dir)
		}
	}

	return nil
}

func validatePath(path string) {
	if path == "" {
		return
	}
	// In a object store, `foo//bar` is not the same as `foo/bar`
	// There are other similar cases that I may or may not enumerate,
	// hopefully this will help catch some bugs

	if path[0] == '/' || strings.Contains(path, "//") || strings.Contains(path, "./") || strings.Contains(path, "..") {
		logrus.WithField("path", path).Fatal("Invalid storage. Contains // or starts with /")
	}
}

type writeCloserFailerWrapper struct {
	io.WriteCloser
	sha256 hash.Hash
}

func newWriteCloserFailerWrapper(w io.WriteCloser) *writeCloserFailerWrapper {
	return &writeCloserFailerWrapper{
		WriteCloser: w,
		sha256:      sha256.New(),
	}
}

func (w *writeCloserFailerWrapper) Fail(err error) error {
	return multierr.Combine(
		err,
		w.Close(),
	)
}

func (w *writeCloserFailerWrapper) Write(p []byte) (n int, err error) {
	w.sha256.Write(p) //nolint: errcheck // docs for hash.Hash say this never errors
	return w.WriteCloser.Write(p)
}

func (w *writeCloserFailerWrapper) BlobSHA256() string {
	return hex.EncodeToString(w.sha256.Sum(nil))
}

type s3Bucket struct {
	bucket   *blob.Bucket
	s3Svc    *s3.S3
	name     string
	basePath string
}

func NewS3Bucket(name string, basePath string, c *aws.Config) (Bucket, error) {
	s, err := session.NewSession(c)
	if err != nil {
		return nil, err
	}

	bucket, err := s3blob.OpenBucket(context.Background(), s, name, nil)
	if err != nil {
		return nil, err
	}

	s3Svc := s3.New(s)

	return &s3Bucket{
		bucket:   bucket,
		s3Svc:    s3Svc,
		name:     name,
		basePath: basePath,
	}, nil
}

func (bkt *s3Bucket) NewReader(ctx context.Context, name string, verifier ObjectVerifier) (io.ReadCloser, error) {
	relPath := path.Join(bkt.basePath, name)
	validatePath(relPath)
	r, err := bkt.bucket.NewReader(ctx, relPath, nil)
	if err != nil {
		return nil, err
	}
	cksumReader := newChecksummingReader(r)
	defer cksumReader.Close()
	return newVerifiedReader(name, cksumReader, verifier)
}

func (bkt *s3Bucket) NewWriter(ctx context.Context, name string) (BlobWriter, error) {
	relPath := path.Join(bkt.basePath, name)
	validatePath(relPath)
	writer, err := bkt.bucket.NewWriter(ctx, relPath, nil)
	if err != nil {
		return nil, err
	}
	return newWriteCloserFailerWrapper(writer), nil
}

func (bkt *s3Bucket) Delete(ctx context.Context, objectPaths []string) error {
	if len(objectPaths) == 0 {
		return nil
	}

	objectIdents := make([]*s3.ObjectIdentifier, len(objectPaths))
	for i, objectPath := range objectPaths {
		fullObjectPath := path.Join(bkt.basePath, objectPath)
		validatePath(fullObjectPath)
		objectIdents[i] = &s3.ObjectIdentifier{
			Key: aws.String(fullObjectPath),
		}
		logrus.Debugf("marking %s for delete", fullObjectPath)
	}

	req := &s3.DeleteObjectsInput{
		Bucket: aws.String(bkt.name),
		Delete: &s3.Delete{
			Objects: objectIdents,
			Quiet:   aws.Bool(true),
		},
	}

	resp, err := bkt.s3Svc.DeleteObjectsWithContext(ctx, req)
	if err != nil {
		logrus.
			WithField("objectPaths", objectPaths).
			WithError(err).
			Error("Failed to delete objects")
		return errors.Wrap(err, "failed to delete objects")
	}

	for _, e := range resp.Errors {
		path := ""
		reason := ""
		if e.Key != nil {
			path = *e.Key
		}
		if e.Message != nil {
			reason = *e.Message
		}
		logrus.WithFields(logrus.Fields{
			"path":   path,
			"reason": reason,
		}).Warn("Failed to delete object")
	}

	return nil
}

func (bkt *s3Bucket) List(ctx context.Context, pathPrefix string, delimited bool) ([]BucketObject, []SharedPrefix, error) {
	pathPrefix = path.Join(bkt.basePath, pathPrefix)
	validatePath(pathPrefix)

	prefix := pathPrefix
	if prefix != "" && prefix[len(prefix)-1] != '/' {
		prefix = prefix + "/"
	}

	objs := []BucketObject{}
	prefixes := []SharedPrefix{}

	req := &s3.ListObjectsV2Input{
		Bucket: aws.String(bkt.name),
		Prefix: aws.String(prefix),
	}

	if delimited {
		req.Delimiter = aws.String("/")
	}

	err := bkt.s3Svc.ListObjectsV2Pages(req, func(page *s3.ListObjectsV2Output, lastPage bool) bool {
		if page.Contents != nil {
			for _, s3obj := range page.Contents {
				relKey := strings.TrimPrefix(strings.TrimPrefix(*s3obj.Key, bkt.basePath), "/")

				objs = append(objs, BucketObject{
					Name: relKey,
				})
			}
		}
		if page.CommonPrefixes != nil {
			for _, prefix := range page.CommonPrefixes {
				relKey := strings.TrimPrefix(strings.TrimPrefix(*prefix.Prefix, bkt.basePath), "/")
				prefixes = append(prefixes, SharedPrefix(relKey))
			}
		}
		return true
	})

	return objs, prefixes, err
}

type errBucket struct {
	err error
}

func (bkt errBucket) NewReader(ctx context.Context, path string, verifier ObjectVerifier) (io.ReadCloser, error) {
	return nil, bkt.err
}

func (bkt errBucket) NewWriter(ctx context.Context, path string) (BlobWriter, error) {
	return nil, bkt.err
}

func (bkt errBucket) List(ctx context.Context, pathPrefix string, delimited bool) ([]BucketObject, []SharedPrefix, error) {
	return nil, nil, bkt.err
}

func (bkt errBucket) Delete(ctx context.Context, objectPaths []string) error {
	return bkt.err
}

func ToObjectPaths(deleteObjs []BucketObject) []string {
	objPaths := make([]string, len(deleteObjs))
	for i, o := range deleteObjs {
		objPaths[i] = o.Name
	}
	return objPaths
}

type gcsBucket struct {
	bucket   *blob.Bucket
	name     string
	basePath string
}

type GCSConfig struct {
	GoogleApplicationCredentials string
	ProjectID                    string
}

func NewGCSBucket(name string, basePath string, c *GCSConfig) (Bucket, error) {
	ctx := context.Background()

	var creds *google.Credentials
	var err error

	if c.GoogleApplicationCredentials != "" {
		creds, err = google.CredentialsFromJSON(ctx, []byte(c.GoogleApplicationCredentials),
			"https://www.googleapis.com/auth/cloud-platform")
	} else {
		creds, err = gcp.DefaultCredentials(ctx)
		if err != nil {
			return nil, err
		}
	}
	if err != nil {
		return nil, errors.Wrap(err, "failed to get GCS credentials")
	}

	client, err := gcp.NewHTTPClient(
		gcp.DefaultTransport(),
		gcp.CredentialsTokenSource(creds))
	if err != nil {
		return nil, err
	}

	bucket, err := gcsblob.OpenBucket(ctx, client, name, nil)
	if err != nil {
		return nil, err
	}

	return &gcsBucket{
		bucket:   bucket,
		name:     name,
		basePath: basePath,
	}, nil
}

func (bkt *gcsBucket) NewReader(ctx context.Context, name string, verifier ObjectVerifier) (io.ReadCloser, error) {
	relPath := path.Join(bkt.basePath, name)
	validatePath(relPath)
	r, err := bkt.bucket.NewReader(ctx, relPath, nil)
	if err != nil {
		return nil, err
	}
	cksumReader := newChecksummingReader(r)
	defer cksumReader.Close()
	return newVerifiedReader(name, cksumReader, verifier)
}

func (bkt *gcsBucket) NewWriter(ctx context.Context, name string) (BlobWriter, error) {
	return nil, errors.New("Unimplemented")

}

func (bkt *gcsBucket) Delete(ctx context.Context, objectPaths []string) error {
	return errors.New("Unimplemented")
}

func (bkt *gcsBucket) List(ctx context.Context, pathPrefix string, delimited bool) ([]BucketObject, []SharedPrefix, error) {
	pathPrefix = path.Join(bkt.basePath, pathPrefix)
	validatePath(pathPrefix)

	prefix := pathPrefix
	if prefix != "" && prefix[len(prefix)-1] != '/' {
		prefix = prefix + "/"
	}

	objs := []BucketObject{}
	prefixes := []SharedPrefix{}

	delimiter := ""
	if delimited {
		delimiter = "/"
	}
	iter := bkt.bucket.List(&blob.ListOptions{
		Prefix:    prefix,
		Delimiter: delimiter,
	})

	for {
		obj, err := iter.Next(ctx)
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, nil, err
		}

		relKey := strings.TrimPrefix(strings.TrimPrefix(obj.Key, bkt.basePath), "/")
		if obj.IsDir {
			prefixes = append(prefixes, SharedPrefix(relKey))
		} else {
			objs = append(objs, BucketObject{
				Name: relKey,
			})
		}
	}

	return objs, prefixes, nil
}
