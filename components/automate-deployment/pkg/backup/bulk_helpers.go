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
	EstimatedSize() int64
}

type BulkUploader interface {
	Upload(context.Context, BulkUploadIterator, ArtifactRepoProgressReporter) error
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

func (m *simpleBulkUploader) Upload(ctx context.Context, iterator BulkUploadIterator,
	progress ArtifactRepoProgressReporter) error {

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	estimatedSize := iterator.EstimatedSize()
	var count int64
	for {
		req, err := iterator.Next()
		if err != nil {
			if err == io.EOF {
				break
			}
			logrus.WithError(err).Error("Bulk upload failed")
			return err
		}
		if err := m.doUpload(ctx, req); err != nil {
			return err
		}
		count++
		if progress != nil {
			progress.ReportProgress(count, estimatedSize)
		}
	}
	logrus.Debug("Finished Upload")

	return nil
}

func (m *simpleBulkUploader) doUpload(ctx context.Context, req BlobUploadRequest) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	defer req.Reader.Close() // nolint: errcheck

	key := path.Join(m.prefix, req.Key)
	logrus.Debugf("Uploading Artifact %s", key)

	writer, err := m.dst.NewWriter(ctx, key)
	if err != nil {
		return err
	}

	if _, err := io.Copy(writer, req.Reader); err != nil {
		return err
	}

	if err := writer.Close(); err != nil {
		return err
	}

	return nil
}

type BulkDeleteIterator interface {
	// Next will return objects that need to be delete
	Next() (string, error)
}

type BulkDeleter interface {
	Delete(context.Context, BulkDeleteIterator) error
}

type simpleBulkDeleter struct {
	prefix string
	bkt    Bucket
}

func NewBulkDeleter(bkt Bucket, prefix string) BulkDeleter {
	return &simpleBulkDeleter{
		prefix: prefix,
		bkt:    bkt,
	}
}

func (m *simpleBulkDeleter) Delete(ctx context.Context, iterator BulkDeleteIterator) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	batchIterator := newBulkDeleteBatcher(900, m.prefix, iterator)
	for {
		req, err := batchIterator.Next()
		if err != nil {
			if err == io.EOF {
				break
			}
			logrus.WithError(err).Error("Bulk delete failed")
			return err
		}
		if err := m.doDelete(ctx, req); err != nil {
			return err
		}
	}
	logrus.Info("Finished Delete")

	return nil
}

func (m *simpleBulkDeleter) doDelete(ctx context.Context, keysToDelete []string) error {
	for _, key := range keysToDelete {
		logrus.Infof("Deleting Artifact %s", key)
	}
	return m.bkt.Delete(ctx, keysToDelete)
}

type bulkDeleteBatcher struct {
	prefix   string
	iterator BulkDeleteIterator
	buffer   []string
	err      error
}

func newBulkDeleteBatcher(batchSize int, prefix string, iterator BulkDeleteIterator) *bulkDeleteBatcher {
	return &bulkDeleteBatcher{
		iterator: iterator,
		buffer:   make([]string, batchSize),
	}
}

func (b *bulkDeleteBatcher) Next() ([]string, error) {
	if b.err != nil {
		return nil, b.err
	}

	items := b.buffer[:0]
	i := 0
	for len(items) < cap(items) {
		item, err := b.iterator.Next()
		if err != nil {
			b.err = err
			if len(items) > 0 {
				return items, nil
			}
			return nil, err
		}
		if b.prefix != "" {
			items = append(items, path.Join(b.prefix, item))
		} else {
			// minimize allocs if we don't need to prefix
			items = append(items, item)

		}
		i++
	}

	return items, nil
}
