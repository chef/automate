// Package boltdb provides an implementation of DeploymentStore that uses
// boltdb. It can deal with multiple versions of how deployments are serialized
// to disk. Only the lastest version needs to provide an interface to write a
// deployment.
//
// Versions:
//   - v0: This version used gob to serialize most of the deployment. The dex
//         config was serialized using protobuf.
//   - v1: The deployment and the config are serialized using protobuf. The
//         deployment and config live under separate keys in the v1 bucket.
//
// Some reasons why a new version might be created:
// - Assumptions have been made that api.AutomateConfig will move in a compatible
//   way. This means you can add fields and deprecate fields, but can never change
//   the type or reuse a deprecated field. If this is ever needed, all of
//   api.AutomateConfig will need to be redefined under one of the versions and
//   a new version will likely need to be created.
//
// - The deployment struct changes and the types no longer map well. A new version
//   can be created to reduce some of the complexity with how things get serialized,
//   however old versions will still need to be able to read and mapped in a sane way.
//
// - The way the buckets and keys are organized needs to change
package boltdb

import (
	"errors"
	"io"

	"github.com/boltdb/bolt"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence"
	v1 "github.com/chef/automate/components/automate-deployment/pkg/persistence/boltdb/internal/v1"
)

var _ persistence.DeploymentStore = (*DeploymentStore)(nil)

var _ CurrentVersion = (*v1.V1)(nil)

// UpgradeableVersion is a version that can be upgraded to the latest.
// Old versions should implement the UpgradeableVersion interface.
type UpgradeableVersion interface {
	Name() string
	ReadDeployment(tx *bolt.Tx) (*deployment.Deployment, error)
	Cleanup(*bolt.Tx) error
}

// CurrentVersion represents the things the current storage version must be able to do
type CurrentVersion interface {
	UpgradeableVersion
	Initialize(*bolt.Tx) error
	WriteDeployment(*bolt.Tx, *deployment.Deployment) error
}

// DeploymentStore is a deployment store backed by boltdb
type DeploymentStore struct {
	initialized bool
	db          *bolt.DB
	v           CurrentVersion
	upgradeable []UpgradeableVersion
}

// NewDeploymentStore wraps boltdb to be able to read the deployment from boltdb.
// It is expected that all migrations have already run before trying to use
func NewDeploymentStore(db *bolt.DB) *DeploymentStore {
	return &DeploymentStore{
		initialized: false,
		db:          db,
		v:           v1.New(),
		upgradeable: []UpgradeableVersion{},
	}
}

// Initialize prepares the deployment store to be used. It MUST be called after
// creation, otherwise all calls to UpdateDeployment and GetDeployment will fail.
func (s *DeploymentStore) Initialize() error {
	err := s.db.Update(func(tx *bolt.Tx) error {
		err := s.v.Initialize(tx)
		if err != nil {
			return err
		}
		return s.updateToLatestVersion(tx)
	})

	if err == nil {
		s.initialized = true
	}
	return err
}

// TryRead attempts to read the deployment store given the currently
// known versions.
func (s *DeploymentStore) TryRead() (*deployment.Deployment, UpgradeableVersion, error) {
	var version UpgradeableVersion
	var deployment *deployment.Deployment

	upgradeable := make([]UpgradeableVersion, 0, len(s.upgradeable)+1)
	upgradeable = append(upgradeable, s.v)
	upgradeable = append(upgradeable, s.upgradeable...)

	err := s.db.View(func(tx *bolt.Tx) error {
		for _, v := range upgradeable {
			d, err := v.ReadDeployment(tx)

			if err == persistence.ErrDoesNotExist {
				continue
			}

			if err != nil {
				return err
			}

			version = v
			deployment = d
			return nil
		}
		return errors.New("deployment doesn't match known version")
	})

	return deployment, version, err
}

// UpdateDeployment receives the current version of the deployment and
// expects the caller to update it through the callback.  This updated
// version will be committed to the database.
func (s *DeploymentStore) UpdateDeployment(cb persistence.DeploymentUpdateCallback) (*deployment.Deployment, error) {
	if !s.initialized {
		return nil, persistence.ErrNotInitialized
	}
	// Writes are serialized. For now, we will lock for the entire update. If that becomes an
	// issue, we can modify this code to read and write in separate transactions, ensuring
	// safety against concurrent updates with a change id of some sort
	d := &deployment.Deployment{}
	err := s.db.Update(func(tx *bolt.Tx) error {
		currentDeployment, err := s.v.ReadDeployment(tx)
		if err != nil {
			if err == persistence.ErrDoesNotExist {
				currentDeployment = &deployment.Deployment{}
			} else {
				return err
			}
		}
		err = cb(currentDeployment)
		if err != nil {
			return err
		}
		err = s.v.WriteDeployment(tx, currentDeployment)
		*d = *currentDeployment // nolint: govet
		return err
	})

	return d, err
}

// GetDeployment gets the deployment for deploymentID
func (s *DeploymentStore) GetDeployment() (*deployment.Deployment, error) {
	deployment := &deployment.Deployment{}

	if !s.initialized {
		return nil, persistence.ErrNotInitialized
	}

	err := s.db.View(func(tx *bolt.Tx) error {
		d, err := s.v.ReadDeployment(tx)
		if err != nil {
			return err
		}
		*deployment = *d // nolint: govet
		return nil
	})
	return deployment, err
}

func (s *DeploymentStore) updateToLatestVersion(tx *bolt.Tx) error {
	upgradeable := make([]UpgradeableVersion, 0, len(s.upgradeable)+1)
	upgradeable = append(upgradeable, s.v)
	upgradeable = append(upgradeable, s.upgradeable...)
	for i, v := range upgradeable {
		d, err := v.ReadDeployment(tx)

		if err == persistence.ErrDoesNotExist {
			continue
		}

		if err != nil {
			return err
		}

		if i == 0 {
			logrus.Info("Deployment store up-to-date")
			return nil
		}

		logrus.WithFields(logrus.Fields{
			"old-version": v.Name(),
			"new-version": s.v.Name(),
		}).Info("Updating deployment store")
		err = s.v.WriteDeployment(tx, d)
		if err != nil {
			return err
		}

		return v.Cleanup(tx)
	}
	logrus.Info("No deployments found")

	return nil
}

// WriteTo writes a backup to the given io.Writer. The backup is created inside
// a boltdb read transaction for data consistency.
func (s *DeploymentStore) WriteTo(w io.Writer) error {
	return s.db.View(func(tx *bolt.Tx) error {
		_, err := tx.WriteTo(w)
		return err
	})
}

func (s *DeploymentStore) Close() error {
	return s.db.Close()
}
