package db

import (
	"github.com/go-gorp/gorp"
	"github.com/pkg/errors"
)

// TransactionFunc
type TransactionFunc func(*gorp.Transaction) error

// Transaction wraps your calls in a gorp.Transaction.
// If the call should fail with an error it will perform a rollback.
// Otherwise the transaction will be committed.
//
// Usage:
// ```
// import (
//   dblib "github.com/chef/automate/lib/db"
//   "github.com/go-gorp/gorp"
//   _ "github.com/lib/pq"
// )
//
// type postgres struct {
//   *gorp.DbMap
// }
//
// func (db *postgres) InsertData(data) error {
//   return dblib.Transaction(db.DbMap, func(tx *gorp.Transaction) error {
//
//     // Add here all your transactions (check all errors)
//     tx.Insert(deploy)
//     tx.Update(deploy)
//     tx.Insert(deploy)
//     tx.Delete(deploy)
//
//     return nil
//   })
// }
// ```
func Transaction(b *gorp.DbMap, f TransactionFunc) error {
	tx, err := b.Begin()
	if err != nil {
		return errors.Wrap(err, "Unable to start transaction")
	}

	defer func() {
		if err != nil {
			tx.Rollback() // nolint: errcheck
		} else {
			err = tx.Commit()
			if err != nil {
				tx.Rollback() // nolint: errcheck
				err = errors.Wrap(err, "Transaction failed and will be rolled back")
			}
		}
	}()

	err = f(tx)
	return err
}
