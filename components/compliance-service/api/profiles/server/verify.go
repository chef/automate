package profiles

import (
	"github.com/sirupsen/logrus"
)

// during bootup, we verify that all profiles from postgres are synced with elastic
func (srv *PGProfileServer) rebuildElasticProfileCache() error {
	logrus.Infof("Ensure all profiles are cached in elastic")

	// list all profile sha version
	profiles, err := srv.store.ListProfiles()
	if err != nil {
		return err
	}

	// ensure all profiles are stored in
	for _, profile := range profiles {
		esprofile, err := srv.es.GetProfile(profile.Sha256)
		if err != nil {
			logrus.Debugf("Storing profile %s with version %s in elastic", profile.Name, profile.Version)

			// store profile json in ES
			err = srv.es.StoreProfile(profile)
			if err != nil {
				logrus.Warnf("%s\n", err)
			}
		} else {
			logrus.Debugf("Profile %s with SHA %s exists in elastic already\n", esprofile.Name, esprofile.Sha256)
		}
	}

	return nil
}
