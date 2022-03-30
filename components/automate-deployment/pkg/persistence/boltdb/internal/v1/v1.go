package v1

import (
	"github.com/boltdb/bolt"
	"github.com/golang/protobuf/proto"
	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence/boltdb/internal/v1/schema"
)

// V1 is the V1 implementation for storage
type V1 struct{}

// noConfigSentinel is written into the config bucket when the user
// persists a deployment with no configuration.
//
// We use a sentinel value rather than looking for the presence of the
// key to differentiate between an incomplete boltdb record and an
// intentionally saved nil configuration.
var noConfigSentinel = []byte("NOCONFIG")

// New returns the V1 implementation
func New() *V1 {
	return &V1{}
}

// Initialize sets up the bucks in boltdb for V1
func (*V1) Initialize(tx *bolt.Tx) error {
	_, err := tx.CreateBucketIfNotExists([]byte("v1"))
	if err != nil {
		return errors.Wrap(err, "Could not create 'v1' bucket")
	}
	return nil
}

// ReadDeployment reads a v1 deployment
func (*V1) ReadDeployment(tx *bolt.Tx) (*deployment.Deployment, error) {
	v1Bucket := tx.Bucket([]byte("v1"))
	if v1Bucket == nil {
		return nil, persistence.ErrDoesNotExist
	}

	internalDeploymentData := v1Bucket.Get([]byte("deployment"))
	if internalDeploymentData == nil {
		return nil, persistence.ErrDoesNotExist
	}

	deploymentConfigData := v1Bucket.Get([]byte("config"))
	if deploymentConfigData == nil {
		return nil, persistence.ErrDoesNotExist
	}

	internalDeployment := schema.Deployment{}
	err := proto.Unmarshal(internalDeploymentData, &internalDeployment)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to deserialize deployment")
	}

	var deploymentConfig *dc.AutomateConfig
	if !isNoConfigSentinel(deploymentConfigData) {
		deploymentConfig = &dc.AutomateConfig{}
		err = proto.Unmarshal(deploymentConfigData, deploymentConfig)
		if err != nil {
			return nil, errors.Wrap(err, "Failed to deserialize deployment config")
		}
	}

	return deserializeDeployment(internalDeployment, deploymentConfig)
}

// WriteDeployment writes a v1 deployment
func (*V1) WriteDeployment(tx *bolt.Tx, deployment *deployment.Deployment) error {
	v1Bucket := tx.Bucket([]byte("v1"))

	serializableDeployment, deploymentConfig := convertToSerializableDeployment(deployment)

	deploymentData, err := proto.Marshal(serializableDeployment)
	if err != nil {
		return errors.Wrap(err, "Failed to serialize deployment")
	}
	err = v1Bucket.Put([]byte("deployment"), deploymentData)
	if err != nil {
		return errors.Wrap(err, "Failed to write serialized deployment")
	}

	var configData []byte
	if deploymentConfig != nil {
		configData, err = proto.Marshal(deploymentConfig)
		if err != nil {
			return errors.Wrap(err, "Failed to serialize deployment config")
		}
	} else {
		configData = noConfigSentinel
	}

	err = v1Bucket.Put([]byte("config"), configData)
	if err != nil {
		return errors.Wrap(err, "Failed to write serialized deployment config")
	}

	return nil
}

// Cleanup removes the buckets used by v1
func (*V1) Cleanup(tx *bolt.Tx) error {
	v1Bucket := tx.Bucket([]byte("v1"))
	if v1Bucket != nil {
		err := tx.DeleteBucket([]byte("v1"))
		if err != nil {
			return err
		}
	}
	return nil
}

// Name returns v1
func (*V1) Name() string {
	return "v1"
}

func isNoConfigSentinel(data []byte) bool {
	return string(data) == string(noConfigSentinel)
}

func convertToSerializableDeployment(externalDeployment *deployment.Deployment) (*schema.Deployment, *dc.AutomateConfig) {
	internalDeployment := &schema.Deployment{
		Id:         externalDeployment.ID,
		LastAction: externalDeployment.LastAction,
		Deployed:   externalDeployment.Deployed,
	}
	ts, err := ptypes.TimestampProto(externalDeployment.CreatedAt)
	if err != nil {
		// This should never happen. If it does, ts will just default to now
		logrus.WithError(err).Error("Failed to read deployment timestamp")
	}
	internalDeployment.CreatedAt = ts

	internalDeployment.Services = make([]*schema.Service, len(externalDeployment.ExpectedServices))
	for i, svc := range externalDeployment.ExpectedServices {
		hartPath := ""
		hart, ok := svc.Installable.(*habpkg.Hart)
		if ok {
			hartPath = hart.Path()
		}

		internalSvc := &schema.Service{
			Name:     svc.Name(),
			Origin:   svc.Origin(),
			HartPath: hartPath,
			SslCert:  svc.SSLCert,
			SslKey:   svc.SSLKey,
		}

		switch svc.DeploymentState {
		case deployment.Skip:
			internalSvc.DeploymentState = schema.Service_SKIP
		case deployment.Installed:
			internalSvc.DeploymentState = schema.Service_INSTALLED
		case deployment.Running:
			internalSvc.DeploymentState = schema.Service_RUNNING
		case deployment.Removed:
			internalSvc.DeploymentState = schema.Service_REMOVED
		}

		internalDeployment.Services[i] = internalSvc
	}

	if externalDeployment.CurrentReleaseManifest != nil {
		packages := make([]string, len(externalDeployment.CurrentReleaseManifest.Packages))
		for i, pkg := range externalDeployment.CurrentReleaseManifest.Packages {
			packages[i] = habpkg.Ident(&pkg)
		}

		hartifacts := make([]*schema.Hart, len(externalDeployment.CurrentReleaseManifest.HartOverrides))
		for i, hart := range externalDeployment.CurrentReleaseManifest.HartOverrides {
			hartifacts[i] = &schema.Hart{
				Origin: hart.Origin(),
				Name:   hart.Name(),
				Path:   hart.Path(),
			}
		}

		internalDeployment.CurrentReleaseManifest = &schema.ReleaseManifest{
			Build:    externalDeployment.CurrentReleaseManifest.Build,
			BuildSHA: externalDeployment.CurrentReleaseManifest.BuildSHA,
			Packages: packages,
			Harts:    hartifacts,
		}
	}

	return internalDeployment, externalDeployment.GetUserOverrideConfigForPersistence()
}

func deserializeDeployment(serializedDeployment schema.Deployment, deploymentConfig *dc.AutomateConfig) (*deployment.Deployment, error) {
	deserializedDeployment, err := deployment.RestoreDeploymentFromUserOverrideConfig(deploymentConfig)
	if err != nil {
		return nil, err
	}
	deserializedDeployment.ID = serializedDeployment.Id
	ts, err := ptypes.Timestamp(serializedDeployment.CreatedAt)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to read deployment timestamp")
	}
	deserializedDeployment.CreatedAt = ts
	deserializedDeployment.LastAction = serializedDeployment.LastAction
	deserializedDeployment.Deployed = serializedDeployment.Deployed
	deserializedDeployment.ExpectedServices = make([]*deployment.Service, len(serializedDeployment.Services))
	for i, svc := range serializedDeployment.Services {
		var pkg habpkg.Installable
		if svc.HartPath == "" {
			habPkg := habpkg.New(svc.Origin, svc.Name)
			pkg = &habPkg
		} else {
			hart, err := habpkg.HartFromPath(svc.HartPath)
			if err != nil {
				return nil, errors.Wrap(err, "Failed to construct Hart from path")
			}
			hart.WithName(svc.Name)
			hart.WithOrigin(svc.Origin)
			pkg = &hart
		}
		externalSvc := &deployment.Service{
			Installable: pkg,
			SSLCert:     svc.SslCert,
			SSLKey:      svc.SslKey,
		}

		switch svc.DeploymentState {
		case schema.Service_SKIP:
			externalSvc.DeploymentState = deployment.Skip
		case schema.Service_INSTALLED:
			externalSvc.DeploymentState = deployment.Installed
		case schema.Service_RUNNING:
			externalSvc.DeploymentState = deployment.Running
		case schema.Service_REMOVED:
			externalSvc.DeploymentState = deployment.Removed
		}

		deserializedDeployment.ExpectedServices[i] = externalSvc
	}

	if serializedDeployment.CurrentReleaseManifest != nil {
		packages := make([]habpkg.HabPkg, len(serializedDeployment.CurrentReleaseManifest.Packages))

		for i, pkg := range serializedDeployment.CurrentReleaseManifest.Packages {
			err := packages[i].UnmarshalText([]byte(pkg))
			if err != nil {
				return nil, errors.Wrap(err, "Failed to read packages from the manifest")
			}
		}

		harts := make([]habpkg.Hart, len(serializedDeployment.CurrentReleaseManifest.Harts))
		for i, hart := range serializedDeployment.CurrentReleaseManifest.Harts {
			h, err := habpkg.HartFromPath(hart.Path)
			if err != nil {
				return nil, errors.Wrap(err, "Failed to read hart override from the manifest")
			}
			h.WithOrigin(hart.Origin)
			h.WithName(hart.Name)
			harts[i] = h
		}

		deserializedDeployment.CurrentReleaseManifest = &manifest.A2{
			Build:         serializedDeployment.CurrentReleaseManifest.Build,
			BuildSHA:      serializedDeployment.CurrentReleaseManifest.BuildSHA,
			Packages:      packages,
			HartOverrides: harts,
		}
	}

	return deserializedDeployment, nil
}
