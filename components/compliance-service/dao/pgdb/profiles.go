package pgdb

import (
	"fmt"
	"net/url"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/stringutils"
)

type profile struct {
	ID        string `db:"id"`
	URL       string `db:"url"`
	Namespace string `db:"namespace"`
	Name      string `db:"name"`
}

func (trans *DBTrans) addProfilesToJob(jobID string, profileURLs []string) error {
	profileURLs = stringutils.SliceFilter(profileURLs, func(profileURL string) bool {
		return len(strings.TrimSpace(profileURL)) > 0
	})

	if len(profileURLs) == 0 {
		return nil
	}

	profiles, err := trans.addProfiles(profileURLs)
	if err != nil {
		return errors.Wrap(err, "unable to add profiles to db")
	}

	err = trans.profileJob(jobID, profiles)
	if err != nil {
		return errors.Wrap(err, "unable to associate profiles with job")
	}

	return nil
}

func (trans *DBTrans) addProfiles(profiles []string) ([]string, error) {
	// initialize map and size it to len profiles.  We are simulating a set here.
	profilesToInsert := make(map[string]struct{}, len(profiles))
	for _, profile := range profiles {
		profilesToInsert[profile] = struct{}{}
	}

	var profs []profile
	selectStmt := fmt.Sprintf("SELECT * FROM profiles WHERE url = ANY('{%s}'::VARCHAR[]);",
		strings.Join(profiles, ","))

	_, err := trans.Select(&profs, selectStmt)
	if err != nil {
		return nil, errors.Wrap(err, "addProfiles unable to find existing profiles")
	}

	profileIDs := make([]string, 0)
	// Now whatever came back from select, we may add to our list of profileIDs since we know they are in the profiles table
	// and we know what their ID is.
	// We may also remove these from our map (because we already have it in our profiles table)
	for _, p := range profs {
		profileIDs = append(profileIDs, p.ID)
		delete(profilesToInsert, p.URL)
	}

	var profilesToBeInserted []interface{}
	//iterate over what's left in the map.  this is what needs to be inserted into profiles table.
	for k := range profilesToInsert {
		namespace, name, err := parseNamespaceAndNameFromUrl(k)
		if err != nil {
			return nil, errors.Wrap(err, "addProfiles unable to parse profile URL")
		}

		profile := profile{
			ID:        createUUID(),
			URL:       k,
			Namespace: namespace,
			Name:      name,
		}
		profileIDs = append(profileIDs, profile.ID)
		profilesToBeInserted = append(profilesToBeInserted, &profile)
	}

	if len(profilesToBeInserted) > 0 {
		err = trans.Insert(profilesToBeInserted...)
		if err != nil {
			return nil, errors.Wrap(err, "addProfiles unable to add profiles to db")
		}
	}
	return profileIDs, nil
}

func (trans *DBTrans) profileJob(jobID string, profileIDs []string) error {
	links := make([]interface{}, 0)

	for _, profileID := range profileIDs {
		link := JobProfile{
			JobID:     jobID,
			ProfileID: profileID,
		}
		links = append(links, &link)
	}
	return trans.Insert(links...)
}

func parseNamespaceAndNameFromUrl(profileUrl string) (string, string, error) {
	u, err := url.Parse(profileUrl)
	if err != nil {
		return "", "", err
	}

	var namespace, name string
	if u.Scheme == "compliance" {
		namespace = u.Host
		name = u.Path

		if strings.HasPrefix(name, "/") {
			name = name[1:]
		}
	} else {
		parts := strings.Split(u.Path, "/")
		// u.Path should be something like "/mynamespace/myname/blahblah". After
		// calling Split it will look like ["", "mynamespace", "myname", "blahblah"].
		if len(parts) < 3 || parts[0] != "" {
			return "", "", fmt.Errorf("addProfiles unable to parse URL: %s", profileUrl)
		}

		namespace = parts[1]
		name = parts[2]
	}

	return namespace, name, nil
}
