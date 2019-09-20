package dao

import (
	gosql "database/sql"
	"strconv"

	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	logs "github.com/sirupsen/logrus"

	datafeed "github.com/chef/automate/api/external/data_feed"
	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/errorutils"
	"github.com/chef/automate/lib/logger"
)

type DB struct {
	*gorp.DbMap
}

type DBTrans struct {
	*gorp.Transaction
}

func New(conf *config.PostgresConfig) (*DB, error) {

	db, err := initDB(conf)
	if err != nil {
		return nil, err
	}

	return &DB{
		DbMap: db,
	}, nil
}

// Transact wraps your calls in a transaction. If the call should fail with an error it will
// perform a rollback. Otherwise the transaction will be committed.
func Transact(db *DB, txFunc func(*DBTrans) error) error {
	trans, err := db.Begin()
	if err != nil {
		return errors.Wrap(err, "Unable to start transaction.")
	}
	tx := DBTrans{
		Transaction: trans,
	}
	defer func() {
		if err != nil {
			tx.Rollback() // nolint: errcheck
		} else {
			err = tx.Commit()
			if err != nil {
				tx.Rollback() // nolint: errcheck
				err = errors.Wrap(err, "Transaction failed and will be rolled back.")
			}
		}
	}()

	err = txFunc(&tx)
	return err
}

func initPostgresDB(conf *config.PostgresConfig) (*gorp.DbMap, error) {
	pgURI := conf.ConnectionString

	logs.Debugf("Use PostgreSQL backend %s", pgURI)
	db, err := gosql.Open("postgres", pgURI)
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to open database with uri: %s", pgURI)
	}

	// Check if the database exists
	err = db.Ping()
	if err != nil {
		return nil, errors.Wrapf(err, "Failed to ping database with uri: %s", pgURI)
	}

	// Run the migrations
	logs.Infof("Migration path %s", conf.MigrationsPath)
	if err := migrator.Migrate(pgURI, conf.MigrationsPath, logger.NewLogrusStandardLogger(), false); err != nil {
		return nil, errors.Wrapf(err, "Unable to complete database migrations")
	}

	return &gorp.DbMap{Db: db, Dialect: gorp.PostgresDialect{}}, nil
}

func initDB(conf *config.PostgresConfig) (*gorp.DbMap, error) {
	dbmap, err := initPostgresDB(conf)
	if err != nil {
		return nil, err
	}
	initTables(dbmap)
	return dbmap, nil
}

func initTables(db *gorp.DbMap) {
	// tell gorp about the tables.
	// set auto-increment to false and tell it about our pk.
	logs.Infof("in initTables -- data-feed-service db ")

	db.AddTableWithName(Destination{}, "destinations").SetKeys(true, "id")
	logs.Infof("completed initTables -- data-feed-service db ")
}

func (db *DB) toDBDestination(inDestination *datafeed.Destination) *Destination {
	newDestination := Destination{}
	newDestination.ID = inDestination.Id
	newDestination.Name = inDestination.Name
	newDestination.URL = inDestination.Url
	newDestination.Secret = inDestination.Secret

	return &newDestination
}

func (db *DB) fromDBDestination(inDestination *Destination) *datafeed.Destination {
	newDestination := datafeed.Destination{}
	newDestination.Id = inDestination.ID
	newDestination.Name = inDestination.Name
	newDestination.Url = inDestination.URL
	newDestination.Secret = inDestination.Secret

	return &newDestination
}

func (db *DB) idToDBDestination(inDestination *datafeed.DestinationId) *Destination {
	newDestination := Destination{}
	newDestination.ID = inDestination.Id

	return &newDestination
}

func (db *DB) AddDestination(destination *datafeed.Destination) (bool, error) {
	dbDestination := db.toDBDestination(destination)
	var err error
	err = Transact(db, func(tx *DBTrans) error {
		if err = tx.Insert(dbDestination); err != nil {
			return errors.Wrap(err, "AddDestination: unable to insert destination")
		}
		return nil
	})

	if err != nil {
		return false, err
	}
	return true, err
}

func (db *DB) DeleteDestination(id *datafeed.DestinationId) (bool, error) {

	var count int64 = 0
	var err error
	err = Transact(db, func(tx *DBTrans) error {

		count, err = tx.Delete(&Destination{ID: id.Id})
		if err != nil {
			return errorutils.ProcessSQLNotFound(err, strconv.FormatInt(id.Id, 10), "DeleteDestination")
		}

		return nil
	})

	if err != nil || count == 0 {
		return false, err
	}
	return true, err
}

func (db *DB) UpdateDestination(destination *datafeed.Destination) (bool, error) {
	dbDestination := db.toDBDestination(destination)
	var err error
	var count int64 = 0
	err = Transact(db, func(tx *DBTrans) error {
		// tx.Delete retuen count, error
		if count, err = tx.Update(dbDestination); err != nil {
			return errors.Wrap(err, "UpdateDestination: unable to update destination")
		}
		return nil
	})

	if err != nil || count == 0 {
		return false, err
	}
	return true, err
}

func (db *DB) GetDestination(destinationId *datafeed.DestinationId) (*datafeed.Destination, error) {

	var err error
	var obj interface{}
	var dest *Destination
	err = Transact(db, func(tx *DBTrans) error {
		// tx.Delete retuen count, error
		if obj, err = tx.Get(Destination{}, destinationId.Id); err != nil {
			return errors.Wrap(err, "GetDestination: unable to get destination")
		}
		if obj == nil {
			dest = &Destination{}
			err = errorutils.ProcessSQLNotFound(errors.New("Record not found"), strconv.FormatInt(destinationId.Id, 10), "GetDestination")
		} else {
			dest = obj.(*Destination)
		}
		return err
	})
	result := db.fromDBDestination(dest)
	if err != nil {
		return result, err
	}
	return result, err
}

func (db *DB) ListDestinations() (*datafeed.ListDestinationResponse, error) {

	var err error
	var destinations []Destination
	err = Transact(db, func(tx *DBTrans) error {

		_, err = tx.Select(&destinations, "select * from destinations")
		if err != nil {
			return errors.Wrap(err, "ListDestination: unable to list destinations")
		}

		return nil
	})

	if err != nil {
		return nil, err
	}
	listOfDestinations := make([]*datafeed.Destination, 0)
	for _, d := range destinations {
		listOfDestinations = append(listOfDestinations, db.fromDBDestination(&d))
	}
	return &datafeed.ListDestinationResponse{Destinations: listOfDestinations}, err
}

func (db *DB) ListDBDestinations() ([]Destination, error) {
	var err error
	var destinations []Destination
	err = Transact(db, func(tx *DBTrans) error {

		_, err = tx.Select(&destinations, "select * from destinations")
		if err != nil {
			return errors.Wrap(err, "ListDestination: unable to list destinations")
		}

		return nil
	})

	if err != nil {
		return nil, err
	}
	return destinations, nil
}
