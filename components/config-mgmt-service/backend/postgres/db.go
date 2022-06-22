package postgres

import (
	"database/sql"

	"github.com/go-gorp/gorp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/config-mgmt-service/config"
	libdb "github.com/chef/automate/lib/db"
	"github.com/chef/automate/lib/db/migrator"
	"github.com/chef/automate/lib/logger"
)

type Postgres struct {
	*config.Postgres
	db     *sql.DB
	mapper *gorp.DbMap
}

type DBTrans struct {
	*gorp.Transaction
}

const clearAllRows = `DELETE FROM rollouts;`

func Open(config *config.Postgres) (*Postgres, error) {
	pg := New(config)

	err := pg.Connect()
	if err != nil {
		return nil, err
	}
	err = pg.Ping()
	if err != nil {
		return nil, err
	}
	err = pg.Migrate()
	if err != nil {
		return nil, err
	}

	return pg, nil
}

func New(config *config.Postgres) *Postgres {
	return &Postgres{Postgres: config}
}

func (p *Postgres) Connect() error {
	log.WithFields(log.Fields{
		"uri": p.URI,
	}).Info("Connecting to PostgreSQL")

	dbMap, err := libdb.PGOpen(p.URI)
	if err != nil {
		return errors.Wrapf(err, "Failed to open database with uri: %s", p.URI)
	}
	p.db = dbMap

	if p.MaxIdleConns > 0 {
		dbMap.SetMaxIdleConns(p.MaxIdleConns)
	}
	if p.MaxOpenConns > 0 {
		dbMap.SetMaxOpenConns(p.MaxOpenConns)
	}

	p.mapper = &gorp.DbMap{Db: dbMap, Dialect: gorp.PostgresDialect{}}
	p.mapper.AddTableWithName(Rollout{}, "rollouts").SetKeys(true, "Id")
	p.mapper.AddTableWithName(NewRollout{}, "rollouts").SetKeys(true, "Id")
	p.mapper.AddTableWithName(Telemetry{}, "telemetry").SetKeys(false, "id")

	return nil
}

func (p *Postgres) Migrate() error {
	logger := logger.NewLogrusStandardLogger()
	log.SetLevel(log.DebugLevel)
	if err := migrator.Migrate(p.URI, p.SchemaPath, logger, true); err != nil {
		log.WithError(err).WithFields(log.Fields{
			"uri":         p.URI,
			"schema_path": p.SchemaPath,
		}).Error("Failed migrating database")
		return errors.Wrapf(err, "Unable to create database schema. [path:%s]", p.SchemaPath)
	}
	return nil
}

// ping will verify if the database mapped with gorp is available
func (p *Postgres) Ping() error {
	return p.db.Ping()
}

func (p *Postgres) Close() error {
	return p.db.Close()
}

func (p *Postgres) Clear() error {
	_, err := p.db.Exec(clearAllRows)
	return err
}

// Transact wraps your calls in a transaction. If the call should fail with an error it will
// perform a rollback. Otherwise the transaction will be committed.
func Transact(pg *Postgres, txFunc func(*DBTrans) error) error {
	trans, err := pg.mapper.Begin()
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
