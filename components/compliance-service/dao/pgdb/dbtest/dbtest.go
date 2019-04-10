package dbtest

import (
	"flag"
	"fmt"
	"os"
	"strings"

	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/stretchr/testify/suite"
)

// Suite defines a struct to be used with a db test suite. You should
// wrap this in your test with your own type.
// `type <your-type> dbtest.Suite`
type Suite struct {
	suite.Suite
	Database *pgdb.DB
}

var (
	database = flag.Bool("database", false, "run database integration tests")
)

//Setup connects to a database for testing purposes. The following OS vars are required:
//  POSTGRES_URI
func Setup() *pgdb.DB {
	connectionString := os.Getenv("POSTGRES_URI")

	db, err := pgdb.InitDB(connectionString)
	if err != nil {
		fmt.Println("Failed to initialize db:", err)
		os.Exit(1)
	}
	return db
}

// Run is a helper function to determine if the current test context requires
// the database tests to run. `True` indicates that that `-database` flag was set when the
// tests were run.
func Run() bool {
	flag.Parse()
	return *database
}

func ListTables(db *pgdb.DB) ([]string, error) {
	var tables []string
	query := "select table_name from information_schema.tables where table_schema = 'public' and table_name not in ('schema_migrations');"

	_, err := db.Select(&tables, query)
	return tables, err
}

func TruncateTables(db *pgdb.DB) error {
	tables, err := ListTables(db)
	if err != nil {
		return err
	}

	query := fmt.Sprintf("truncate %s cascade;", strings.Join(tables, ","))
	_, err = db.Exec(query)
	return err
}
