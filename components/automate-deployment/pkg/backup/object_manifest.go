package backup

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"sync"

	"github.com/pkg/errors"
)

type SnapshotChecksumMismatchError struct {
	objectName    string
	expectedCkSum string
	actualCkSum   string
}

func newSnapshotChecksumMismatchError(objectName, expectedCkSum, actualCkSum string) error {
	return &SnapshotChecksumMismatchError{
		objectName:    objectName,
		expectedCkSum: expectedCkSum,
		actualCkSum:   actualCkSum,
	}
}

func (s *SnapshotChecksumMismatchError) Error() string {
	return fmt.Sprintf("Object %q in the snapshot has been modified or is corrupted. Expected checksum: %q; actual: %q", s.objectName, s.expectedCkSum, s.actualCkSum)
}

type SnapshotChecksumMissingError struct {
	objectName string
}

func newSnapshotChecksumMissingError(objectName string) error {
	return &SnapshotChecksumMissingError{
		objectName: objectName,
	}
}

func (s *SnapshotChecksumMissingError) Error() string {
	return fmt.Sprintf("Object %q is present in the snapshot but this object is not recorded in the backup metadata", s.objectName)
}

type RequiredChecksumDataMissingError struct {
	objectName string
}

func newRequiredChecksumDataMissingError(objectName string) error {
	return &RequiredChecksumDataMissingError{
		objectName: objectName,
	}
}

func (e *RequiredChecksumDataMissingError) Error() string {
	return fmt.Sprintf("snapshot integrity cannot be verified because file %q containing checksum data is missing from the snapshot", e.objectName)
}

func IsSnapshotChecksumError(err error) bool {
	if _, ok := err.(*SnapshotChecksumMismatchError); ok {
		return true
	}
	if _, ok := err.(*SnapshotChecksumMissingError); ok {
		return true
	}
	if _, ok := err.(*RequiredChecksumDataMissingError); ok {
		return true
	}
	return false

}

type ObjectManifest struct {
	// sync.Map provides no type-safety. Don't access it directly. Wrap access in
	// functions to get type-safety back.
	blobSHA256s *sync.Map
}

func NewObjectManifest() ObjectManifest {
	return ObjectManifest{blobSHA256s: &sync.Map{}}
}

func (o ObjectManifest) WriteFinished(objectName string, w BlobWriter) {
	o.blobSHA256s.Store(objectName, w.BlobSHA256())
}

func (o ObjectManifest) DataWritten(objectName string, data []byte) {
	cksumBytes := sha256.Sum256(data)
	o.blobSHA256s.Store(objectName, hex.EncodeToString(cksumBytes[:]))
}

func (o ObjectManifest) ObjectSHA256s() map[string]string {
	cksums := make(map[string]string)
	o.blobSHA256s.Range(func(objectName, cksum interface{}) bool {
		if obj, ok := objectName.(string); ok {
			if c, ok := cksum.(string); ok {
				cksums[obj] = c
			}
		}
		return true
	})
	return cksums
}

// ObjectVerifier checks objects as they are restored to ensure they have the
// correct checksums as recorded when they were backed up
type ObjectVerifier interface {
	// ObjectValid checks if the object is present in the backup metadata to
	// prevent a malicious actor from sneaking an extra file into a backup.
	ObjectValid(objectName string) error
	ValidateBlobContent(objectName string, r BlobReader) error
}

// NoOpObjectVerifier implements the ObjectVerifier interface but returns the
// success case for all methods. It's here for compatibility with older backups
// that don't have any checksums.
type NoOpObjectVerifier struct {
}

func (o *NoOpObjectVerifier) ObjectValid(objectName string) error {
	return nil
}

func (o *NoOpObjectVerifier) ValidateBlobContent(objectName string, r BlobReader) error {
	return nil
}

type SHA256Verifier struct {
	blobSHA256s map[string]string
}

func (o *SHA256Verifier) ObjectValid(objectName string) error {
	if _, haveObject := o.blobSHA256s[objectName]; !haveObject {
		return errors.Errorf("Object %q is present in the snapshot but this object is not recorded in the backup metadata", objectName)
	}
	return nil
}

func (o *SHA256Verifier) ValidateBlobContent(objectName string, r BlobReader) error {
	expectedCkSum, haveCksum := o.blobSHA256s[objectName]
	if !haveCksum {
		return newSnapshotChecksumMissingError(objectName)
	}
	actualCkSum := r.BlobSHA256()
	if actualCkSum != expectedCkSum {
		return newSnapshotChecksumMismatchError(objectName, expectedCkSum, actualCkSum)
	}

	return nil
}

type verifiedReader struct {
	*os.File
}

func (v *verifiedReader) Close() error {
	v.File.Close()
	return os.Remove(v.File.Name())
}

func newVerifiedReader(objectName string, r BlobReader, o ObjectVerifier) (io.ReadCloser, error) {
	f, err := ioutil.TempFile("", "chef-automate-restore-staged-file")
	if err != nil {
		return nil, errors.Wrapf(err, "failed to create tempfile to stage backup object %s", objectName)
	}
	_, err = io.Copy(f, r)
	if err != nil {
		f.Close()
		os.Remove(f.Name())
		return nil, errors.Wrapf(err, "failed to copy backup object %s to tempfile", objectName)
	}
	_, err = f.Seek(0, 0)
	if err != nil {
		f.Close()
		os.Remove(f.Name())
		return nil, errors.Wrapf(err, "failed to copy backup object %s to tempfile", objectName)
	}
	if err = o.ValidateBlobContent(objectName, r); err != nil {
		return nil, err
	}

	return &verifiedReader{File: f}, nil
}
