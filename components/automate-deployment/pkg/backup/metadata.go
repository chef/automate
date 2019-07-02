package backup

import (
	"context"
	"encoding/json"
	"io/ioutil"
	"path"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/deployment"
)

const (
	metadataFileBaseName = "metadata.json"
)

// Metadata is a wrapper struct for metadata that should be backed up.
type Metadata struct {
	Spec                     *Spec             `json:"spec"`
	Task                     *api.BackupTask   `json:"task"`
	DeploymentServiceVersion string            `json:"deployment_service_version"`
	ContentsSHA256           map[string]string `json:"contents_sha256"`
}

func ShowBackupChecksum(bucket Bucket) (string, error) {
	objectName := metadataChecksumsObjectName
	reader, err := bucket.NewReader(context.TODO(), objectName, &NoOpObjectVerifier{})
	if err != nil && IsNotExist(err) {
		return "", err
	}
	if err != nil {
		return "", errors.Wrapf(err, "error opening backup metadata checksums object with key %s", objectName)
	}
	defer reader.Close()

	jsonBytes, err := ioutil.ReadAll(reader)
	if err != nil {
		return "", errors.Wrapf(err, "error reading backup metadata checksums object at %s", objectName)
	}
	backupChecksummer := NewObjectManifest()
	backupChecksummer.DataWritten(objectName, jsonBytes)
	backupSHA256 := backupChecksummer.ObjectSHA256s()[metadataChecksumsObjectName]
	return backupSHA256, nil
}

// LoadMetadataVerifier returns an ObjectVerifier that can be used to verify
// the integrity of the metadata.json file for each component backup. The
// metadata.json files then contain checksums of the individual files in the
// per-service backup.
func LoadMetadataVerifier(ctx context.Context, bucket Bucket, sha256 string) (ObjectVerifier, error) {
	cksums := &MetadataChecksums{}

	var ckSumsFileVerifier ObjectVerifier
	// This is the verifier object that knows the expected checksum of the
	// `checksums.json` file at the root of the backup (that file has the
	// checksums of the metadata.json file for each service's backup).

	// empty string case happens when the user does not pass `--sha256` to the
	// restore subcommand, or when we are computing the checksum for the `show`
	// subcommand.
	if sha256 == "" {
		ckSumsFileVerifier = &NoOpObjectVerifier{}
	} else {
		checksumsFileExpectedChecksum := make(map[string]string)
		checksumsFileExpectedChecksum[metadataChecksumsObjectName] = sha256
		ckSumsFileVerifier = &SHA256Verifier{blobSHA256s: checksumsFileExpectedChecksum}
	}

	objectName := metadataChecksumsObjectName

	reader, err := bucket.NewReader(ctx, objectName, ckSumsFileVerifier)
	// Backups created before the checksum code was added won't have this file.
	// To keep them working, we no-op the verification in this case. But if the
	// user gave us a SHA, then we require the file to exist
	if err != nil && IsNotExist(err) && sha256 != "" {
		return nil, newRequiredChecksumDataMissingError(metadataChecksumsObjectName)
	}
	if err != nil && IsNotExist(err) {
		return &NoOpObjectVerifier{}, nil
	}
	if err != nil {
		return nil, errors.Wrapf(err, "error opening backup metadata checksums object with key %s", objectName)
	}
	defer reader.Close()

	jsonBytes, err := ioutil.ReadAll(reader)
	if err != nil {
		return nil, errors.Wrapf(err, "error reading backup metadata checksums object at %s", objectName)
	}

	if err = json.Unmarshal(jsonBytes, cksums); err != nil {
		return nil, errors.Wrapf(err, "failed to parse backup metadata checksums at %s", objectName)
	}

	return &SHA256Verifier{blobSHA256s: cksums.ContentsSHA256}, nil
}

// LoadServiceMetadata takes a backup bucket and service name and returns
// a new Metadata instance from the backed up metadata in the given bucket.
// Errors caused by the metadata not existing will return true when passed to
// `backup.IsNotExist(err)`
func LoadServiceMetadata(ctx context.Context, bucket Bucket, svcName string, objectVerifier ObjectVerifier) (*Metadata, error) {
	metadata := &Metadata{}

	mdStorageKey := path.Join(svcName, metadataFileBaseName)

	reader, err := bucket.NewReader(ctx, mdStorageKey, objectVerifier)
	if err != nil {
		// Return an IsNotExist error unwrapped so caller can also check for IsNotExist
		if IsNotExist(err) {
			return metadata, err
		} else {
			return metadata, errors.Wrapf(err, "error opening backup metadata key %s", mdStorageKey)
		}
	}
	defer reader.Close()

	mdBytes, err := ioutil.ReadAll(reader)
	if err != nil {
		return metadata, errors.Wrapf(err, "error reading backup metadata at %s", mdStorageKey)
	}

	if err = json.Unmarshal(mdBytes, metadata); err != nil {
		return metadata, errors.Wrapf(err, "failed to parse backup metadata at %s", mdStorageKey)
	}

	return metadata, nil
}

type Specless = string

func LoadAllSpecsFromBackup(ctx context.Context, bucket Bucket, verifier ObjectVerifier) ([]Spec, []Specless, error) {
	_, services, err := bucket.List(ctx, "", true)
	if err != nil {
		return nil, nil, err
	}

	specs := make([]Spec, 0, len(services))
	specless := []string{}
	for _, service := range services {
		metadata, err := LoadServiceMetadata(ctx, bucket, string(service), verifier)
		if err != nil {
			if IsNotExist(errors.Cause(err)) {
				logrus.Warnf("Missing metadata for %s", service)
				specless = append(specless, string(service))
				continue
			} else {
				return nil, nil, err
			}
		}
		if metadata.Spec == nil {
			return nil, nil, errors.Wrapf(err, "Spec missing for %s", service)
		}
		specs = append(specs, *metadata.Spec)
	}

	return specs, specless, nil
}

func (m *Metadata) Verifier() ObjectVerifier {
	// TODO: go's default values makes this hard to get right. Hopefully it can
	// be improved by bumping versions.
	// in particular, if a service uses the rsync-ish backup operation, and is
	// backed up when it has zero files, we can't tell the difference between
	// that case and the case of an older backup that didn't include the SHA-2 sums.
	// Then it would be possible to insert a "bad" file into a backup and it
	// would get "restored"

	if len(m.ContentsSHA256) == 0 {
		return &NoOpObjectVerifier{}
	} else {
		return &SHA256Verifier{blobSHA256s: m.ContentsSHA256}
	}
}
