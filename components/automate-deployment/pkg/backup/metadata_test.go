package backup

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type noExistBucket struct{}

var exampleNoExistErr = errors.New("NoSuchKey - bucket for this test case always returns a not exist error")

func (b *noExistBucket) NewReader(ctx context.Context, objectName string, verifier ObjectVerifier) (io.ReadCloser, error) {
	return nil, exampleNoExistErr
}

func (b *noExistBucket) NewWriter(ctx context.Context, objectName string) (BlobWriter, error) {
	return nil, exampleNoExistErr
}

func (b *noExistBucket) List(ctx context.Context, objectNamePrefix string, delimited bool) ([]BucketObject, []SharedPrefix, error) {
	return nil, nil, fmt.Errorf("not implemented")
}

func (b *noExistBucket) Delete(ctx context.Context, objects []string) error {
	return fmt.Errorf("not implemented")
}

func (b *noExistBucket) Read(p []byte) (n int, err error) {
	// backup.IsNotExist() (defined in bucket.go) checks for string "NoSuchKey"
	// because that's what minio returns
	return 0, exampleNoExistErr
}

func TestLoadMetadataVerifier(t *testing.T) {
	mdChecksumsByName := make(map[string]string)
	mdChecksumsByName["example/metadata.json"] = "example-sha256"

	mdChecksums := MetadataChecksums{ContentsSHA256: mdChecksumsByName}
	mdChecksumsJSON, err := json.Marshal(mdChecksums)
	require.NoError(t, err)

	t.Run("returns an error when given an incorrect SHA256 for the checksums.json file", func(t *testing.T) {
		ctx := context.Background()

		tmpDir, err := ioutil.TempDir("", "LoadMetadataVerifier-test")
		require.NoError(t, err)

		bucket := fsBucket{basePath: tmpDir}
		defer os.RemoveAll(tmpDir)

		w, err := bucket.NewWriter(ctx, metadataChecksumsObjectName)
		require.NoError(t, err)

		_, err = w.Write(mdChecksumsJSON)
		require.NoError(t, err)

		err = w.Close()
		require.NoError(t, err)

		_, err = LoadMetadataVerifier(ctx, bucket, "incorrect-sha256-for-test")
		require.Error(t, err)
	})

	t.Run("returns a SHA256Verifier when the checksums file exists", func(t *testing.T) {
		bucketReader := newChecksummingReader(ioutil.NopCloser(bytes.NewBuffer(mdChecksumsJSON)))
		bucket := &mockBucket{reader: bucketReader}
		verifier, err := LoadMetadataVerifier(context.Background(), bucket, "")
		require.NoError(t, err)
		require.IsType(t, &SHA256Verifier{}, verifier)

		if sha256verifier, ok := verifier.(*SHA256Verifier); ok {
			assert.Equal(t, mdChecksumsByName, sha256verifier.blobSHA256s)
		}
	})

	t.Run("returns a no op verifier when the checksums file doesn't exist", func(t *testing.T) {
		bucket := &noExistBucket{}
		verifier, err := LoadMetadataVerifier(context.Background(), bucket, "")
		require.NoError(t, err)
		assert.Equal(t, &NoOpObjectVerifier{}, verifier)
	})

}

// checksum verification happens in the bucket implementations; mockBucket
// doesn't implement the checksum verification, so we have to use a real bucket
// implementation.
func setupFsBucketMetadata(t *testing.T, metadataJSON []byte) (Bucket, func()) {
	ctx := context.Background()

	tmpDir, err := ioutil.TempDir("", "LoadServiceMetadata-test")
	require.NoError(t, err)

	bucket := fsBucket{basePath: tmpDir}
	cleanup := func() { os.RemoveAll(tmpDir) }

	w, err := bucket.NewWriter(ctx, "example/metadata.json")
	require.NoError(t, err)

	_, err = w.Write(metadataJSON)
	require.NoError(t, err)

	err = w.Close()
	require.NoError(t, err)

	return bucket, cleanup
}

func TestLoadMetadataVerification(t *testing.T) {

	cksumsByName := make(map[string]string)
	metadata := Metadata{}

	metadataJSON, err := json.Marshal(metadata)
	require.NoError(t, err)

	mdSha := sha256.Sum256(metadataJSON)
	cksumsByName["example/metadata.json"] = hex.EncodeToString(mdSha[:])
	verifier := &SHA256Verifier{blobSHA256s: cksumsByName}

	// test metadata load:
	// when checksums match, return metadata
	// when checksums don't match err
	t.Run("loads the metadata when the metadata file is successfully verified", func(t *testing.T) {
		bucket, cleanup := setupFsBucketMetadata(t, metadataJSON)
		defer cleanup()
		md, err := LoadServiceMetadata(context.Background(), bucket, "example", verifier)
		require.NoError(t, err)
		assert.IsType(t, &Metadata{}, md)
	})

	t.Run("returns an error if the metadata file cannot be verified", func(t *testing.T) {
		badMetadataJSON := make([]byte, len(metadataJSON))
		copy(badMetadataJSON, metadataJSON)
		badMetadataJSON = append(badMetadataJSON, '\n')

		bucket, cleanup := setupFsBucketMetadata(t, badMetadataJSON)
		defer cleanup()
		_, err := LoadServiceMetadata(context.Background(), bucket, "example", verifier)
		require.Error(t, err)
	})

	t.Run("returns an error if the metadata file exists but doesn't have a recorded checksum", func(t *testing.T) {
		cksumsByName := make(map[string]string)
		verifier := &SHA256Verifier{blobSHA256s: cksumsByName}

		bucket, cleanup := setupFsBucketMetadata(t, metadataJSON)
		defer cleanup()

		_, err := LoadServiceMetadata(context.Background(), bucket, "example", verifier)
		require.Error(t, err)
	})
}
