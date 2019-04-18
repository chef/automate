package pg

import (
	"database/sql"

	_ "github.com/lib/pq"
	"github.com/sirupsen/logrus"
)

// A DBProvider allows you to connect to a DB
type DBProvider interface {
	Connect(ConnInfoURI, string) (DB, error)
}

// DB is a high-level abstraction over PostgreSQL database
// interactions. This interface allows us to mock database
// interactions with Automate 1 in self-test mode.
type DB interface {
	CreateDatabase(string) error
	CreateDatabaseWithOwner(string, string) error
	AlterDatabaseOwner(string, string) error
	CreateRole(string) error
	RemovePassword(string) error
	GrantAll(string, string) error
	DropDatabase(string) error
	ConnectedUserIsSuperuser() (bool, error)
	RenameDatabase(string, string) error
	DatabaseExists(string) (bool, error)
	CreateExtension(string) error

	Ping() error
	Close() error
	ExecStatement(string, ...interface{}) error
	StringQuery(string, ...interface{}) (string, error)
	BoolQuery(string, ...interface{}) (bool, error)
	BigintQuery(string, ...interface{}) (int64, error)
}

// sqlDB is a DB built on top of the database/sql library.
type sqlDB struct {
	connInfo ConnInfoURI
	db       *sql.DB
}
type sqlDBProvider struct{}

var DefaultDBProvider DBProvider = &sqlDBProvider{}
var CurrentDBProvider = DefaultDBProvider

func Connect(info ConnInfoURI, dbname string) (DB, error) {
	return CurrentDBProvider.Connect(info, dbname)
}

func (s *sqlDBProvider) Connect(info ConnInfoURI, dbname string) (DB, error) {
	db, err := sql.Open("postgres", info.ConnURI(dbname))
	if err != nil {
		return nil, err
	}
	return &sqlDB{
		connInfo: info,
		db:       db,
	}, err
}

func (s *sqlDB) Ping() error {
	return s.db.Ping()
}

func (s *sqlDB) Close() error {
	return s.db.Close()
}

func (s *sqlDB) CreateDatabase(dbname string) error {
	return s.ExecStatement(CreateDatabaseQuery(dbname))
}

func (s *sqlDB) CreateDatabaseWithOwner(dbname string, owner string) error {
	return s.ExecStatement(CreateDatabaseWithOwnerQuery(dbname, owner))
}

func (s *sqlDB) AlterDatabaseOwner(dbname string, owner string) error {
	return s.ExecStatement(AlterDatabaseOwner(dbname, owner))
}

func (s *sqlDB) CreateRole(role string) error {
	return s.ExecStatement(CreateRoleQuery(role))
}

func (s *sqlDB) ConnectedUserIsSuperuser() (bool, error) {
	return s.BoolQuery(ConnectedUserIsSuperuserQuery)
}

func (s *sqlDB) DatabaseExists(name string) (bool, error) {
	return s.BoolQuery(DatabaseExistsQuery, name)
}

func (s *sqlDB) DropDatabase(name string) error {
	return s.ExecStatement(DropDatabaseQuery(name))
}

func (s *sqlDB) RenameDatabase(oldName string, newName string) error {
	return s.ExecStatement(RenameDatabaseQuery(oldName, newName))
}

func (s *sqlDB) RemovePassword(roleName string) error {
	return s.ExecStatement(RemoveRolePasswordQuery(roleName))
}

func (s *sqlDB) GrantAll(dbName string, roleName string) error {
	return s.ExecStatement(GrantAllQuery(dbName, roleName))
}

func (s *sqlDB) CreateExtension(extName string) error {
	return s.ExecStatement(CreateExtensionQuery(extName))
}

// StringQuery performs a query that returns a single row with a
// single string column.
func (s *sqlDB) StringQuery(query string, args ...interface{}) (string, error) {
	logrus.WithFields(logrus.Fields{
		"query":     query,
		"conn_info": s.connInfo,
	}).Debug("executing database query")

	var row string
	err := s.db.QueryRow(query, args...).Scan(&row)
	if err != nil {
		return "", err
	}

	return row, nil
}

// BoolQuery performs a query that returns a bool
func (s *sqlDB) BoolQuery(query string, args ...interface{}) (bool, error) {
	logrus.WithFields(logrus.Fields{
		"query":     query,
		"conn_info": s.connInfo,
	}).Debug("executing database query")

	rows, err := s.db.Query(query, args...)
	if err != nil {
		return false, err
	}
	defer rows.Close() // nolint: errcheck

	var res bool
	rows.Next()
	err = rows.Scan(&res)
	if err != nil {
		return false, err
	}

	err = rows.Err()
	if err != nil {
		return false, err
	}

	return res, nil
}

func (s *sqlDB) BigintQuery(query string, args ...interface{}) (int64, error) {
	logrus.WithFields(logrus.Fields{
		"query":     query,
		"conn_info": s.connInfo,
	}).Debug("executing database query")

	var row int64
	err := s.db.QueryRow(query, args...).Scan(&row)
	if err != nil {
		return int64(0), err
	}

	return row, nil
}

func (s *sqlDB) ExecStatement(statement string, args ...interface{}) error { // nolint: unparam
	logrus.WithFields(logrus.Fields{
		"statement": statement,
		"conn_info": s.connInfo,
	}).Debug("executing database statement")
	_, err := s.db.Exec(statement, args...)
	return err
}
