package profiles

import (
	"os"
	"path/filepath"

	"github.com/sirupsen/logrus"
	"go.uber.org/multierr"

	"github.com/chef/automate/components/compliance-service/profiles/market"
)

func (srv *PGProfileServer) migrateDiskProfiles() error {
	logrus.Infof("Migrate all profiles from Automate 1")

	if _, err := os.Stat(srv.profiles.ProfilesPath); os.IsNotExist(err) {
		logrus.Infof("Profiles path %s is missing, migration is not needed...", srv.profiles.ProfilesPath)
		return nil
	}

	namespaces, err := market.Namespaces(srv.profiles.ProfilesPath)
	if err != nil {
		logrus.WithError(err).Errorf("Could not list namespaces in %s", srv.profiles.ProfilesPath)
		return err
	}

	var finalError error
	for _, namespace := range namespaces {
		logrus.Infof("Migrate namespace %s to new profile store", namespace)

		namespaceProfilePath := filepath.Join(srv.profiles.ProfilesPath, namespace)
		profiles, err := market.ListProfiles(namespaceProfilePath)
		if err != nil {
			logrus.WithError(err).Errorf("Could not list profiles in %s", namespaceProfilePath)
			finalError = multierr.Combine(finalError, err)
			continue
		}

		for _, profile := range profiles {
			logrus.Infof("Migrate profile %s to new profile store", profile)

			pFile, err := os.Open(profile)
			if err != nil {
				logrus.WithError(err).Errorf("Could not open %q for copying", profile)
				continue
			}
			// NOTE(ssd) 2018-10-09: Copy profile into
			// temporary directory so that it can be read
			// by the inspec runner. Currently this and
			// uploads working depends on the default mode
			// of os.Create being 666 and our default
			// umask being 022.
			//
			// ListProfiles appears to only return .tar.gz
			tmpFile, err := market.TempUpload(pFile, ".tar.gz")
			if err != nil {
				logrus.WithError(err).Error("Could not copy profile into temporary directory")
				finalError = multierr.Combine(finalError, err)
				pFile.Close() // nolint: errcheck
				continue
			}

			err = pFile.Close()
			if err != nil {
				logrus.WithError(err).Error("Could not close profile after copy")
			}

			_, err = srv.storeProfile(namespace, tmpFile)
			if err != nil {
				logrus.WithError(err).Errorf("Could not store profile %q", tmpFile)
				finalError = multierr.Combine(finalError, err)
			} else {
				// archive profile, so that we do not import it during next bootup
				err := market.Archive(srv.profiles.ProfilesPath, namespace, profile)
				if err != nil {
					logrus.WithError(err).Errorf("Could not archive profile %q", profile)
					finalError = multierr.Combine(finalError, err)
				}
			}

			err = os.Remove(tmpFile)
			if err != nil {
				logrus.WithError(err).Errorf("Could not clean up temp file %q", tmpFile)
			}
		}
	}
	return finalError
}
