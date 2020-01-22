package backup

import (
	"context"
	"io"
	"path"

	"github.com/sirupsen/logrus"
)

type BlobUploadRequest struct {
	// Key is the key corresponding to the data that will be returned by Reader
	Key string
	// Reader is a reader for that data for Key
	Reader io.ReadCloser
}

type BulkUploadIterator interface {
	// Next will return objects that need to be uploaded
	Next() (BlobUploadRequest, error)
}

type BulkUploadError struct {
	Failures map[string]error
}

func (*BulkUploadError) Error() string {
	return "BulkUplaodError"
}

type BulkUploader interface {
	Upload(context.Context, BulkUploadIterator) error
}

type mockBulkUploader struct {
	prefix string
}

func NewBulkUploader(dst Bucket, prefix string) BulkUploader {
	return &mockBulkUploader{
		prefix: prefix,
	}
}

func (m *mockBulkUploader) Upload(ctx context.Context, iterator BulkUploadIterator) error {
	for {
		req, err := iterator.Next()
		if err != nil {
			if err == io.EOF {
				break
			}
			logrus.WithError(err).Error("Bulk upload failed")
			return err
		}
		key := path.Join(m.prefix, req.Key)
		logrus.Infof("Writing Artifact %s", key)
	}
	logrus.Info("Finished Upload")

	return nil
}
