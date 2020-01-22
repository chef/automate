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

type simpleBulkUploader struct {
	prefix string
	dst    Bucket
}

func NewBulkUploader(dst Bucket, prefix string) BulkUploader {
	return &simpleBulkUploader{
		prefix: prefix,
		dst:    dst,
	}
}

func (m *simpleBulkUploader) Upload(ctx context.Context, iterator BulkUploadIterator) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	for {
		req, err := iterator.Next()
		if err != nil {
			if err == io.EOF {
				break
			}
			logrus.WithError(err).Error("Bulk upload failed")
			return err
		}
		m.doUpload(ctx, req)
	}
	logrus.Info("Finished Upload")

	return nil
}

func (m *simpleBulkUploader) doUpload(ctx context.Context, req BlobUploadRequest) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	defer req.Reader.Close()

	key := path.Join(m.prefix, req.Key)
	logrus.Infof("Uploading Artifact %s", key)

	writer, err := m.dst.NewWriter(ctx, key)
	if err != nil {
		return err
	}

	if _, err := io.Copy(writer, req.Reader); err != nil {
		return nil
	}

	if err := writer.Close(); err != nil {
		return err
	}

	return nil
}
