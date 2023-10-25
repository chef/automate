package db

import (
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"strings"

	"github.com/Masterminds/squirrel"
	"github.com/blang/semver"
	"github.com/lib/pq"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/profiles/market"
	"github.com/chef/automate/lib/pgutils"
)

type Store struct {
	DB *pgdb.DB
}

func (s *Store) GetProfileInfo(filename string, firejailProfilePath string) (string, []byte, []byte, error) {
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
		inspecJSON, err = inspec.Json(filename, firejailProfilePath)
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

func (s *Store) LoadMarketProfiles(path string, firejailProfilePath string) error {
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
		sha256, tar, info, err := s.GetProfileInfo(diskProfile, firejailProfilePath)
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

func (s *Store) latestVersion(namespace string, name string) (string, error) {
	query := `
		SELECT info->>'version'
		FROM store_profiles WHERE
		sha256 IN (SELECT sha256 FROM store_namespace WHERE owner=$1)
		AND info->>'name' = $2
	`

	rows, err := s.DB.Query(query, namespace, name)
	if err != nil {
		return "", err
	}
	defer rows.Close() // nolint: errcheck

	versions := make([]semver.Version, 0)

	for rows.Next() {
		var version semver.Version
		err := rows.Scan(&version)
		if err != nil {
			return "", err
		}
		versions = append(versions, version)
	}
	err = rows.Err()
	if err != nil {
		return "", err
	}

	if len(versions) == 0 {
		return "", nil
	}

	semver.Sort(versions)
	latest := versions[len(versions)-1]

	return latest.String(), nil
}

// returns data about an individual profile
func (s *Store) Read(namespace string, name string, version string) (*inspec.Profile, error) {
	if len(version) == 0 {
		v, err := s.latestVersion(namespace, name)
		if err != nil {
			return nil, err
		}
		version = v
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
		v, err := s.latestVersion(namespace, name)
		if err != nil {
			return nil, err
		}
		version = v
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

func (s *Store) parseList(rows *sql.Rows) ([]inspec.Metadata, int, error) {
	logrus.Debug("Parse profile list from database")
	entries := make([]inspec.Metadata, 0)
	var (
		sha256       string
		metadataBlob []byte
		total        int
	)
	for rows.Next() {
		logrus.Debug("iterate over row")
		err := rows.Scan(&sha256, &metadataBlob, &total)
		if err != nil {
			return nil, 0, err
		}
		logrus.Debug("parse metadata")
		metadata := inspec.Metadata{}
		err = metadata.ParseJSON(metadataBlob)
		if err != nil {
			return nil, 0, err
		}
		metadata.Sha256 = sha256
		entries = append(entries, metadata)
	}
	err := rows.Err()
	if err != nil {
		return nil, 0, err
	}
	return entries, total, nil
}

type ProfilesListRequest struct {
	Filters   map[string][]string
	Name      string
	Namespace string
	Order     string
	Page      int
	PerPage   int
	Sort      string
}

func (s *Store) ListProfilesMetadata(req ProfilesListRequest) ([]inspec.Metadata, int, error) {
	if req.Order != "ASC" && req.Order != "DESC" {
		return nil, 0, status.Errorf(codes.InvalidArgument, "order field '%s' is invalid. Use either 'ASC' or 'DESC'", req.Order)
	}

	if req.Sort != "name" && req.Sort != "title" {
		return nil, 0, status.Errorf(codes.InvalidArgument, "sort field '%s' is invalid. Use either 'name' or 'title'", req.Sort)
	}

	sql := squirrel.
		Select("sha256, info, count(*) OVER() AS total").
		From("store_profiles")

	if len(req.Namespace) == 0 {
		sql = sql.
			Where("exists(select 1 from store_market where store_profiles.sha256 = store_market.sha256)")
	} else {
		sql = sql.
			Where("exists(select 1 from store_namespace where store_profiles.sha256 = store_namespace.sha256 and store_namespace.owner = ?)", req.Namespace)
	}

	if len(req.Name) != 0 {
		sql = sql.Where("info->>'name' = ?", req.Name)
	}

	for key, values := range req.Filters {
		terms := squirrel.Or{}
		for _, value := range values {
			field := fmt.Sprintf("info->>'%s'", key)
			v := pgutils.EscapeLiteralForPGPatternMatch(value)
			v = strings.ReplaceAll(v, "*", "%")
			terms = append(terms, squirrel.Like{field: v})
		}
		sql = sql.Where(terms)
	}

	// For backwards compatibility, pagination needs to be optional, so only paginate when either parameter has been set.
	if req.Page > 0 || req.PerPage > 0 {
		perPage := uint64(req.PerPage)
		page := uint64(req.Page) - 1

		if req.Page < 1 {
			page = 0
		}

		if req.PerPage < 1 {
			perPage = 100
		}

		sql = sql.
			Limit(perPage).
			Offset(perPage * page)
	}

	sql = sql.OrderBy(fmt.Sprintf("info->>'%s' %s", req.Sort, req.Order), "info->>'version' DESC")

	query, args, err := sql.PlaceholderFormat(squirrel.Dollar).ToSql()
	if err != nil {
		return nil, 0, err
	}

	logrus.WithFields(logrus.Fields{
		"query": query,
		"args":  args,
		"err":   err,
	}).Debugf("Querying profiles")

	rows, err := s.DB.Query(query, args...)
	if err != nil {
		return nil, 0, err
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
