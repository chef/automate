package backup

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"net/http"
	"path"
	"strings"
	"time"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	config "github.com/chef/automate/api/config/shared"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/secrets"
)

// LocationSpecification describes where data lives. It returns a bucket to write to
type LocationSpecification interface {
	// ToBucket converts the location specification to a bucket. All accessess have an
	// implicit prefix of baseKey
	ToBucket(baseKey string) Bucket

	// ConfigureBackupRestoreTask configures the task so that its backup location is
	// consistent with self
	ConfigureBackupRestoreTask(*api.BackupRestoreTask) error

	// String returns a string representation
	String() string
}

// GatewayLocationSpecification describes how to communicate with the backup gateway
type GatewayLocationSpecification struct {
	BucketName string
	Endpoint   string
	BasePath   string
	CertPool   *x509.CertPool

	secretStore     secrets.SecretsReader
	secretGroupName string
}

// ToBucket returns a backup bucket that can be used to communicate with the
// backup gateway.
func (gws GatewayLocationSpecification) ToBucket(key string) Bucket {
	tr := httputils.NewDefaultTransport()
	tr.TLSClientConfig = &tls.Config{RootCAs: gws.CertPool}

	accessKeyName := secrets.SecretName{Group: gws.secretGroupName, Name: "access_key"}
	secretKeyName := secrets.SecretName{Group: gws.secretGroupName, Name: "secret_key"}
	accessKey, err := gws.secretStore.GetSecret(accessKeyName)
	if err != nil {
		return errBucket{err: err}
	}
	secretKey, err := gws.secretStore.GetSecret(secretKeyName)
	if err != nil {
		return errBucket{err: err}
	}

	c := &aws.Config{
		Region:           aws.String("us-east-1"),
		Endpoint:         aws.String(gws.Endpoint),
		Credentials:      credentials.NewStaticCredentials(string(accessKey), string(secretKey), ""),
		S3ForcePathStyle: aws.Bool(true),
		HTTPClient:       &http.Client{Transport: tr},
	}

	bucket, err := NewS3Bucket(gws.BucketName, path.Join(gws.BasePath, key), c)
	if err != nil {
		logrus.WithError(err).Warn("could not initialize backup gateway bucket")
		return errBucket{err: err}
	}

	return bucket
}

// ConfigureBackupRestoreTask is a NOOP callback implementation to satisfy the
// LocationSpecification interface.
func (gws GatewayLocationSpecification) ConfigureBackupRestoreTask(req *api.BackupRestoreTask) error {
	return nil
}

// String is the backup gateway identified as a string
func (gws GatewayLocationSpecification) String() string {
	return fmt.Sprintf("<backup-gateway s3://%s/%s/%s>", gws.Endpoint, gws.BucketName, gws.BasePath)
}

type FilesystemLocationSpecification struct {
	Path string
}

func (fsspec FilesystemLocationSpecification) ToBucket(baseKey string) Bucket {
	return NewFilesystemBucket(path.Join(fsspec.Path, baseKey))
}

func (fsspec FilesystemLocationSpecification) ConfigureBackupRestoreTask(req *api.BackupRestoreTask) error {
	req.BackupDir = fsspec.Path
	return nil
}

func (fsspec FilesystemLocationSpecification) String() string {
	return fmt.Sprintf("local directory <%s>", fsspec.Path)
}

type S3LocationSpecification struct {
	// Required
	BucketName string
	Endpoint   string

	// Optional
	BasePath     string
	AccessKey    string
	SecretKey    string
	SessionToken string
}

func (s3spec S3LocationSpecification) ToBucket(baseKey string) Bucket {
	c := &aws.Config{
		Region:           aws.String("us-east-1"),
		S3ForcePathStyle: aws.Bool(true),
	}

	if s3spec.Endpoint != "" {
		c.Endpoint = aws.String(s3spec.Endpoint)

		region := s3EndpointRegion(s3spec.Endpoint)
		if region != "" {
			c.Region = aws.String(region)
		}
		if strings.HasPrefix(s3spec.Endpoint, "http://") {
			c.DisableSSL = aws.Bool(true)
		}
	} else {
		// We need to wire a context to this function.
		ctx, cancel := context.WithTimeout(context.TODO(), 10*time.Second)
		defer cancel()
		region, err := s3BucketLocation(ctx, s3spec.BucketName)
		if err != nil {
			logrus.WithError(err).Warn("Failed to get bucket region. Try specifying the region endpoint")
		} else {
			logrus.Debugf("Using region %s", region)
			c.Region = aws.String(region)
		}
	}

	if s3spec.AccessKey != "" {
		c.Credentials = credentials.NewStaticCredentials(s3spec.AccessKey, s3spec.SecretKey, s3spec.SessionToken)
	}

	bucket, err := NewS3Bucket(s3spec.BucketName, path.Join(s3spec.BasePath, baseKey), c)
	if err != nil {
		// I should have just let the creation the Context return an error :(
		logrus.WithError(err).Warn("could not initialize bucket")
		return errBucket{err: err}
	}

	return bucket
}

func (s3spec S3LocationSpecification) ConfigureBackupRestoreTask(req *api.BackupRestoreTask) error {
	req.S3BackupLocation = &api.S3BackupLocation{
		BucketName:   s3spec.BucketName,
		BasePath:     s3spec.BasePath,
		Endpoint:     s3spec.Endpoint,
		AccessKey:    s3spec.AccessKey,
		SecretKey:    s3spec.SecretKey,
		SessionToken: s3spec.SessionToken,
	}
	return nil
}

func (s3spec S3LocationSpecification) String() string {
	return fmt.Sprintf("s3 repository <s3://%s/%s>", s3spec.BucketName, s3spec.BasePath)
}

// NewRemoteLocationSpecificationFromRestoreTask takes BackupRestoreTask and converts
// it into a corresponding LocationSpecification type depending on the backup location.
func NewRemoteLocationSpecificationFromRestoreTask(restoreTask *api.BackupRestoreTask) LocationSpecification {
	if restoreTask.GetS3BackupLocation().GetBucketName() != "" {
		return S3LocationSpecification{
			BucketName:   restoreTask.GetS3BackupLocation().GetBucketName(),
			BasePath:     restoreTask.GetS3BackupLocation().GetBasePath(),
			Endpoint:     restoreTask.GetS3BackupLocation().GetEndpoint(),
			AccessKey:    restoreTask.GetS3BackupLocation().GetAccessKey(),
			SecretKey:    restoreTask.GetS3BackupLocation().GetSecretKey(),
			SessionToken: restoreTask.GetS3BackupLocation().GetSessionToken(),
		}
	}

	return FilesystemLocationSpecification{Path: restoreTask.GetBackupDir()}
}

// NewRemoteLocationSpecificationFromGlobalConfig takes the GlobalConfig and converts
// it into a corresponding LocationSpecification type depending on the backup location.
func NewRemoteLocationSpecificationFromGlobalConfig(globalConfig *config.GlobalConfig) LocationSpecification {
	switch globalConfig.GetV1().GetBackups().GetLocation().GetValue() {
	case "s3":
		return S3LocationSpecification{
			BucketName:   globalConfig.GetV1().GetBackups().GetS3().GetBucket().GetName().GetValue(),
			BasePath:     globalConfig.GetV1().GetBackups().GetS3().GetBucket().GetBasePath().GetValue(),
			Endpoint:     globalConfig.GetV1().GetBackups().GetS3().GetBucket().GetEndpoint().GetValue(),
			AccessKey:    globalConfig.GetV1().GetBackups().GetS3().GetCredentials().GetAccessKey().GetValue(),
			SecretKey:    globalConfig.GetV1().GetBackups().GetS3().GetCredentials().GetSecretKey().GetValue(),
			SessionToken: globalConfig.GetV1().GetBackups().GetS3().GetCredentials().GetSessionToken().GetValue(),
		}
	default:
		return FilesystemLocationSpecification{
			Path: globalConfig.GetV1().GetBackups().GetFilesystem().GetPath().GetValue(),
		}
	}
}

// NewBackupGatewayLocationSpec takes the backup gateway endpoint, bucket name,
// base path, root TLS certificate and a secret store and returns a compatible
// backup location specification.
func NewBackupGatewayLocationSpec(endpoint,
	bucketName,
	basePath string,
	rootCert []byte,
	secretStore secrets.SecretsReader) (LocationSpecification, error) {
	return NewMinioLocationSpec(endpoint, bucketName, basePath, "backup-gateway",
		rootCert, secretStore)
}

func NewMinioLocationSpec(endpoint,
	bucketName,
	basePath string,
	groupName string,
	rootCert []byte,
	secretStore secrets.SecretsReader) (LocationSpecification, error) {

	certPool := x509.NewCertPool()
	if !certPool.AppendCertsFromPEM(rootCert) {
		return nil, errors.New("building root ca cert pool")
	}

	return GatewayLocationSpecification{
		Endpoint:        endpoint,
		BucketName:      bucketName,
		BasePath:        basePath,
		CertPool:        certPool,
		secretGroupName: groupName,
		secretStore:     secretStore,
	}, nil
}

type GCSLocationSpecification struct {
	// Required
	BucketName string

	// Optional
	BasePath                     string
	GoogleApplicationCredentials string
	ProjectID                    string
}

func (gcsSpec GCSLocationSpecification) ToBucket(baseKey string) Bucket {
	bucket, err := NewGCSBucket(gcsSpec.BucketName, path.Join(gcsSpec.BasePath, baseKey), &GCSConfig{
		GoogleApplicationCredentials: gcsSpec.GoogleApplicationCredentials,
		ProjectID:                    gcsSpec.ProjectID,
	})

	if err != nil {
		// I should have just let the creation the Context return an error :(
		logrus.WithError(err).Warn("could not initialize bucket")
		return errBucket{err: err}
	}

	return bucket
}

func (gcsSpec GCSLocationSpecification) ConfigureBackupRestoreTask(req *api.BackupRestoreTask) error {
	req.GcsBackupLocation = &api.GCSBackupLocation{
		BucketName:                   gcsSpec.BucketName,
		BasePath:                     gcsSpec.BasePath,
		ProjectId:                    gcsSpec.ProjectID,
		GoogleApplicationCredentials: gcsSpec.GoogleApplicationCredentials,
	}
	return nil
}

func (gcsSpec GCSLocationSpecification) String() string {
	return fmt.Sprintf("gcs repository <gcs://%s/%s>", gcsSpec.BucketName, gcsSpec.BasePath)
}
