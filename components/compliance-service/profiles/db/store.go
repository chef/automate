package db

import (
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"os"

	"github.com/blang/semver"
	"github.com/lib/pq"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/profiles/market"
)

type Store struct {
	DB *pgdb.DB
}

func (s *Store) GetProfileInfo(filename string) (string, []byte, []byte, error) {
	// load profile into memory
	tarContent, err := ioutil.ReadFile(filename)
	if err != nil {
		return "", nil, nil, err
	}

	var inspecProfile inspec.Profile
	var inspecJSON []byte

	// if filename.json is there, we can use that
	inspecJsonFile := filename + ".json"
	if _, err := os.Stat(inspecJsonFile); err == nil {
		// path/to/profile.tar.gz.json exists
		jsonContent, err := ioutil.ReadFile(inspecJsonFile)
		if err == nil {
			err = json.Unmarshal(jsonContent, &inspecProfile)
			if err == nil {
				logrus.Debugf("use cached inspec json for %s", filename)
				inspecJSON = jsonContent
			}
		}
	}

	// JSON file was not pregenerated or could not be read, so delegate to inspec.
	if len(inspecJSON) == 0 {
		inspecJSON, err = inspec.Json(filename)
		if err != nil {
			return "", nil, nil, err
		}
		err = json.Unmarshal(inspecJSON, &inspecProfile)
		if err != nil {
			return "", nil, nil, err
		}
	}

	// create db object
	return inspecProfile.Sha256, tarContent, inspecJSON, nil
}

func (s *Store) LoadMarketProfiles(path string) error {
	logrus.Infof("Verify that latest market profiles (%s) are stored in database", path)

	// determine all profiles in directory
	profiles, err := market.ListProfiles(path)
	if err != nil {
		logrus.Errorf("%v", err)
		return err
	}

	var shaList []string

	// filename returns an absolute path to the disk
	for _, diskProfile := range profiles {
		logrus.Debugf("Upload profile %s", diskProfile)

		// gather information that we need to store in postgres
		sha256, tar, info, err := s.GetProfileInfo(diskProfile)
		if err != nil {
			// log error, and ignore profile
			logrus.Error(err)
		} else {
			shaList = append(shaList, sha256)
			err := s.insertProfile(sha256, tar, info)
			if err != nil {
				logrus.WithError(err).Error("failed to insert profile into databases")
			}
		}
	}

	// ensure the list of market profiles is up-to-date
	cleanMarketSQL := `
		DELETE FROM store_market
		WHERE sha256 IN (
			SELECT sha256
			FROM store_market
			WHERE sha256 <> ALL ($1)
        )
	`

	_, err = s.DB.Exec(cleanMarketSQL, pq.Array(shaList))
	if err != nil {
		logrus.Errorf("could not clean up market %v", err)
	}

	return nil
}

func (s *Store) insertProfile(sha256 string, tar []byte, info []byte) error {
	tx, err := s.DB.Begin()
	if err != nil {
		return err
	}

	_, err = tx.Exec("INSERT INTO store_profiles (sha256, tar, info) VALUES ($1, $2, $3) ON CONFLICT DO NOTHING",
		sha256, tar, info)
	if err != nil {
		tx.Rollback() // nolint: errcheck
		return err
	}

	_, err = tx.Exec("INSERT INTO store_market (sha256) VALUES ($1) ON CONFLICT DO NOTHING", sha256)
	if err != nil {
		tx.Rollback() // nolint: errcheck
		return err
	}

	return tx.Commit()
}

func (s *Store) UploadProfile(sha256 string, namespace string, tar []byte, info []byte) error {
	tx, err := s.DB.Begin()
	if err != nil {
		return err
	}

	// only upload the profile if the sha does not already exists
	addStoreStmt, err := tx.Prepare("INSERT INTO store_profiles (sha256, tar, info) VALUES ($1, $2, $3) ON CONFLICT DO NOTHING")
	if err != nil {
		return err
	}

	userAssociation := `
		INSERT INTO store_namespace(
		owner, sha256)
		VALUES ($1, $2) ON CONFLICT DO NOTHING;
	`

	userStmt, err := tx.Prepare(userAssociation)
	if err != nil {
		tx.Rollback() // nolint: errcheck
		return err
	}

	_, err = addStoreStmt.Exec(sha256, tar, info)
	if err != nil {
		tx.Rollback() // nolint: errcheck
		return err
	}

	_, err = userStmt.Exec(namespace, sha256)
	if err != nil {
		tx.Rollback() // nolint: errcheck
		return err
	}

	err = tx.Commit()
	if err != nil {
		tx.Rollback() // nolint: errcheck
		return err
	}
	return nil
}

func (s *Store) InstallMarket(owner, name, version string) error {
	if len(owner) == 0 {
		return errors.New("no namespace provided")
	}

	if len(name) == 0 {
		return errors.New("no profile name provided")
	}

	if len(version) == 0 {
		return errors.New("no profile version provided")
	}

	installMarketSQL := `
		INSERT INTO store_namespace(owner, sha256)
		SELECT $1 as owner, sha256
		FROM store_profiles
		WHERE sha256 IN(SELECT sha256 FROM store_market)
		AND info->>'name' = $2
		AND info->>'version' = $3
		ON CONFLICT DO NOTHING;
	`

	installStmt, err := s.DB.Prepare(installMarketSQL)
	if err != nil {
		return err
	}

	_, err = installStmt.Exec(owner, name, version)
	if err != nil {
		return err
	}

	return nil
}

// returns data about an individual market profile
func (s *Store) ReadMarket(name string, version string) (*inspec.Profile, error) {
	selectProfileSQL := `
		SELECT info
		FROM store_profiles
		WHERE sha256 IN(SELECT sha256 FROM store_market)
		AND info->>'name' = $1
		AND info->>'version' = $2;
	`

	var infoBlob []byte
	err := s.DB.QueryRow(selectProfileSQL, name, version).Scan(&infoBlob)
	if err != nil {
		return nil, err
	}

	profile := &inspec.Profile{}
	err = profile.FromJSON(infoBlob)
	if err != nil {
		return nil, err
	}
	return profile, nil
}

// returns market tar
func (s *Store) ReadMarketTar(name string, version string) ([]byte, error) {
	selectProfileSQL := `
		SELECT tar
		FROM store_profiles
		WHERE sha256 IN(SELECT sha256 FROM store_market)
		AND info->>'name' = $1
		AND info->>'version' = $2;
	`

	var tarBlob []byte
	err := s.DB.QueryRow(selectProfileSQL, name, version).Scan(&tarBlob)
	if err != nil {
		return nil, err
	}

	return tarBlob, nil
}

func (s *Store) latestVersion(namespace string, name string) (*inspec.Metadata, error) {
	sort := "name"
	order := "ASC"
	metadataList, err := s.ListProfilesMetadata(namespace, name, sort, order)
	if err != nil {
		return nil, err
	}
	var latest inspec.Metadata
	for _, metadata := range metadataList {
		v1, _ := semver.Make(latest.Version)
		v2, _ := semver.Make(metadata.Version)
		if metadata.Name == name && v2.GT(v1) {
			latest = metadata
		}
	}

	return &latest, nil
}

// returns data about an individual profile
func (s *Store) Read(namespace string, name string, version string) (*inspec.Profile, error) {
	if len(version) == 0 {
		metadata, err := s.latestVersion(namespace, name)
		if err != nil {
			return nil, err
		}
		version = metadata.Version
	}

	selectProfileSQL := `
		SELECT info
		FROM store_profiles
		WHERE sha256 IN(SELECT sha256 FROM store_namespace WHERE owner = $1)
		AND info->>'name' = $2
		AND info->>'version' = $3;
	`

	var infoBlob []byte
	err := s.DB.QueryRow(selectProfileSQL, namespace, name, version).Scan(&infoBlob)
	if err != nil {
		return nil, err
	}

	profile := &inspec.Profile{}
	err = profile.FromJSON(infoBlob)
	if err != nil {
		return nil, err
	}
	return profile, nil
}

// returns the tar for a specific profile
func (s *Store) ReadTar(namespace string, name string, version string) ([]byte, error) {
	if len(version) == 0 {
		metadata, err := s.latestVersion(namespace, name)
		if err != nil {
			return nil, err
		}
		version = metadata.Version
	}

	selectProfileSQL := `
		SELECT tar
		FROM store_profiles
		WHERE sha256 IN(SELECT sha256 FROM store_namespace WHERE owner = $1)
		AND info->>'name' = $2
		AND info->>'version' = $3;
	`

	var tarBlob []byte
	err := s.DB.QueryRow(selectProfileSQL, namespace, name, version).Scan(&tarBlob)
	if err != nil {
		return nil, err
	}

	return tarBlob, nil
}

func (s *Store) Delete(namespace string, name string, version string) error {
	deleteProfileUser := `
		DELETE FROM store_namespace
		WHERE sha256 IN (
			SELECT sha256
			FROM store_profiles
			WHERE info->>'name' = $2
			AND info->>'version' = $3)
		AND owner = $1
	`

	stmt, err := s.DB.Prepare(deleteProfileUser)
	if err != nil {
		return err
	}
	_, err = stmt.Exec(namespace, name, version)
	if err != nil {
		return err
	}
	return nil
}

func (s *Store) parseList(rows *sql.Rows) ([]inspec.Metadata, error) {
	logrus.Debug("Parse profile list from database")
	entries := make([]inspec.Metadata, 0)
	var (
		sha256       string
		metadataBlob []byte
	)
	for rows.Next() {
		logrus.Debug("iterate over row")
		err := rows.Scan(&sha256, &metadataBlob)
		if err != nil {
			logrus.Error(err)
			return entries, err
		}
		logrus.Debug("parse metadata")
		metadata := inspec.Metadata{}
		err = metadata.ParseJSON(metadataBlob)
		if err != nil {
			logrus.Error(err)
			return entries, err
		}
		metadata.Sha256 = sha256
		entries = append(entries, metadata)
	}
	err := rows.Err()
	if err != nil {
		logrus.Error(err)
		return entries, err
	}
	return entries, nil
}

func validateSortOrder(sort string, order string) error {
	if order != "ASC" && order != "DESC" {
		return status.Errorf(codes.InvalidArgument, "order field '%s' is invalid. Use either 'ASC' or 'DESC'", sort)
	}

	if sort != "name" && sort != "title" && sort != "maintainer" {
		return status.Errorf(codes.InvalidArgument, "sort field '%s' is invalid. Use either 'name', 'title' or 'maintainer'", sort)
	}
	return nil
}

func (s *Store) marketProfilesAll(sort string, order string) (*sql.Rows, error) {
	err := validateSortOrder(sort, order)
	if err != nil {
		return nil, err
	}
	selectMarketSQL := fmt.Sprintf(`
		SELECT sha256, info
		FROM store_profiles WHERE sha256 IN
		(SELECT sha256 FROM store_market)
		ORDER BY store_profiles.info #>> '{%s}' %s, store_profiles.info #>> '{version}' DESC;
	`, sort, order)

	selectMarketStmt, err := s.DB.Prepare(selectMarketSQL)
	if err != nil {
		logrus.Error(err)
		return nil, err
	}

	return selectMarketStmt.Query()
}

func (s *Store) marketProfilesByName(name string, sort string, order string) (*sql.Rows, error) {
	err := validateSortOrder(sort, order)
	if err != nil {
		return nil, err
	}
	selectMarketSQL := fmt.Sprintf(`
		SELECT sha256, info
		FROM store_profiles WHERE sha256 IN
		(SELECT sha256 FROM store_market)
		AND info->>'name' = $1
		ORDER BY store_profiles.info #>> '{%s}' %s, store_profiles.info #>> '{version}' DESC;
	`, sort, order)

	selectMarketStmt, err := s.DB.Prepare(selectMarketSQL)
	if err != nil {
		logrus.Error(err)
		return nil, err
	}

	return selectMarketStmt.Query(name)
}

// returns array of profiles with info
func (s *Store) ListMarketProfilesMetadata(name string, sort string, order string) ([]inspec.Metadata, error) {
	logrus.Debug("Listing market profiles")

	var rows *sql.Rows
	var err error
	if name != "" {
		rows, err = s.marketProfilesByName(name, sort, order)
	} else {
		rows, err = s.marketProfilesAll(sort, order)
	}

	if err != nil {
		logrus.Error(err)
		return nil, err
	}

	defer rows.Close() // nolint: errcheck
	return s.parseList(rows)
}

// returns array of profiles with info
func (s *Store) namespaceProfilesAll(namespace string, sort string, order string) (*sql.Rows, error) {
	logrus.Debug("Listing profile per namespace")
	err := validateSortOrder(sort, order)
	if err != nil {
		return nil, err
	}
	selectProfilesSQL := fmt.Sprintf(`
		SELECT sha256, info
		FROM store_profiles WHERE
		sha256 IN (SELECT sha256 FROM store_namespace WHERE owner=$1)
		ORDER BY store_profiles.info #>> '{%s}' %s, store_profiles.info #>> '{version}' DESC;
	`, sort, order)

	selectProfilesStmt, err := s.DB.Prepare(selectProfilesSQL)
	if err != nil {
		return nil, err
	}

	return selectProfilesStmt.Query(namespace)
}

func (s *Store) namespaceProfilesByName(namespace string, name string, sort string, order string) (*sql.Rows, error) {
	err := validateSortOrder(sort, order)
	if err != nil {
		return nil, err
	}
	selectProfilesSQL := fmt.Sprintf(`
		SELECT sha256, info
		FROM store_profiles WHERE
		sha256 IN (SELECT sha256 FROM store_namespace WHERE owner=$1)
		AND info->>'name' = $2
		ORDER BY store_profiles.info #>> '{%s}' %s, store_profiles.info #>> '{version}' DESC;
	`, sort, order)

	selectProfilesStmt, err := s.DB.Prepare(selectProfilesSQL)
	if err != nil {
		return nil, err
	}

	return selectProfilesStmt.Query(namespace, name)
}

// retruns an array of all versions for a specific profile
func (s *Store) ListProfilesMetadata(namespace string, name string, sort string, order string) ([]inspec.Metadata, error) {
	logrus.Debugf("Listing profiles for namespace %s and name %s", namespace, name)

	var rows *sql.Rows
	var err error
	if name != "" {
		rows, err = s.namespaceProfilesByName(namespace, name, sort, order)
	} else {
		rows, err = s.namespaceProfilesAll(namespace, sort, order)
	}

	if err != nil {
		logrus.Error(err)
		return nil, err
	}

	defer rows.Close() // nolint: errcheck
	return s.parseList(rows)
}

func (s *Store) ListProfiles() ([]inspec.Profile, error) {
	selectProfilesSQL := `
		SELECT sha256, info FROM store_profiles;
	`

	selectProfilesStmt, err := s.DB.Prepare(selectProfilesSQL)
	if err != nil {
		return nil, err
	}

	rows, err := selectProfilesStmt.Query()

	if err != nil {
		logrus.Error(err)
		return nil, err
	}

	defer rows.Close() // nolint: errcheck

	entries := make([]inspec.Profile, 0)
	var (
		sha256   string
		infoBlob []byte
	)
	for rows.Next() {
		err := rows.Scan(&sha256, &infoBlob)
		if err != nil {
			logrus.Error(err)
			return entries, err
		}
		profile := inspec.Profile{}
		err = profile.FromJSON(infoBlob)
		if err != nil {
			logrus.Error(err)
			return entries, err
		}

		entries = append(entries, profile)
	}
	err = rows.Err()
	if err != nil {
		logrus.Error(err)
		return entries, err
	}
	return entries, nil
}
