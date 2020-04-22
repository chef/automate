package dao

import (
	"database/sql"
	"strconv"

	"github.com/lib/pq"

	datafeed "github.com/chef/automate/api/external/data_feed"
	"github.com/chef/automate/lib/errorutils"
	"github.com/pkg/errors"
)

const uniqueViolation = "23505"

type Destination struct {
	ID     int64  `db:"id"`
	Name   string `db:"name"`
	URL    string `db:"url"`
	Secret string `db:"secret"`
}

func addToDBDestination(inDestination *datafeed.AddDestinationRequest) *Destination {
	newDestination := Destination{}
	newDestination.ID = inDestination.Id
	newDestination.Name = inDestination.Name
	newDestination.URL = inDestination.Url
	newDestination.Secret = inDestination.Secret

	return &newDestination
}

func updateToDBDestination(inDestination *datafeed.UpdateDestinationRequest) *Destination {
	newDestination := Destination{}
	newDestination.ID, _ = strconv.ParseInt(inDestination.Id, 10, 64)
	newDestination.Name = inDestination.Name
	newDestination.URL = inDestination.Url
	newDestination.Secret = inDestination.Secret

	return &newDestination
}

func dbToGetDestinationResponse(inDestination *Destination) *datafeed.GetDestinationResponse {
	newDestination := datafeed.GetDestinationResponse{}
	newDestination.Id = inDestination.ID
	newDestination.Name = inDestination.Name
	newDestination.Url = inDestination.URL
	newDestination.Secret = inDestination.Secret

	return &newDestination
}

func (db *DB) AddDestination(destination *datafeed.AddDestinationRequest) (int64, error) {
	dbDestination := addToDBDestination(destination)
	err := Transact(db, func(tx *DBTrans) error {
		if err := tx.Insert(dbDestination); err != nil {
			pgErr, ok := err.(*pq.Error)
			if ok && pgErr.Code == uniqueViolation {
				return errors.Wrap(&errorutils.InvalidError{Msg: "A data feed destination already exists with name \"" + destination.Name + "\""}, "AddDestination: unable to insert destination")
			} else {
				return errors.Wrap(err, "AddDestination: unable to insert destination")
			}
		}
		return nil

	})

	if err != nil {
		return -1, err
	}
	return dbDestination.ID, err
}

func (db *DB) DeleteDestination(delete *datafeed.DeleteDestinationRequest) error {
	var count int64 = 0
	var err error
	err = Transact(db, func(tx *DBTrans) error {
		count, err = tx.Delete(&Destination{ID: delete.Id})
		if count == 0 {
			return errorutils.ProcessSQLNotFound(sql.ErrNoRows, strconv.FormatInt(delete.Id, 10), "DeleteDestination")
		}
		if err != nil {
			return errorutils.ProcessSQLNotFound(err, strconv.FormatInt(delete.Id, 10), "DeleteDestination")
		}
		return nil
	})

	if err != nil {
		return err
	}
	return err
}

func (db *DB) UpdateDestination(destination *datafeed.UpdateDestinationRequest) error {
	dbDestination := updateToDBDestination(destination)
	var err error
	var count int64 = 0
	err = Transact(db, func(tx *DBTrans) error {
		if count, err = tx.Update(dbDestination); err != nil {
			pgErr, ok := err.(*pq.Error)
			if ok && pgErr.Code == uniqueViolation {
				return errors.Wrap(&errorutils.InvalidError{Msg: "Cannot update name, a data feed destination already exists with name \"" + destination.Name + "\""}, "UpdateDestination: unable to update destination")
			} else {
				return errors.Wrap(err, "UpdateDestination: unable to update destination")
			}
		}
		if count == 0 {
			return errorutils.ProcessSQLNotFound(sql.ErrNoRows, destination.Id, "UpdateDestination")
		}
		return nil
	})
	if err != nil {
		return err
	}
	return err
}

func (db *DB) GetDestination(get *datafeed.GetDestinationRequest) (*datafeed.GetDestinationResponse, error) {
	var err error
	var obj interface{}
	var dest *Destination
	err = Transact(db, func(tx *DBTrans) error {
		if obj, err = tx.Get(Destination{}, get.Id); err != nil {
			return errors.Wrap(err, "GetDestination: unable to get destination")
		}
		if obj == nil {
			dest = &Destination{}
			err = errorutils.ProcessSQLNotFound(sql.ErrNoRows, strconv.FormatInt(get.Id, 10), "GetDestination")
		} else {
			dest = obj.(*Destination)
		}
		return err
	})
	result := dbToGetDestinationResponse(dest)
	if err != nil {
		return result, err
	}
	return result, err
}

func (db *DB) ListDestinations() (*datafeed.ListDestinationResponse, error) {
	var destinations []Destination
	err := Transact(db, func(tx *DBTrans) error {
		_, err := tx.Select(&destinations, "select * from destinations")
		if err != nil {
			return errors.Wrap(err, "ListDestination: unable to list destinations")
		}

		return nil
	})

	if err != nil {
		return nil, err
	}
	listOfDestinations := make([]*datafeed.GetDestinationResponse, 0)
	for _, d := range destinations {
		listOfDestinations = append(listOfDestinations, dbToGetDestinationResponse(&d))
	}
	return &datafeed.ListDestinationResponse{Destinations: listOfDestinations}, err
}

func (db *DB) ListDBDestinations() ([]Destination, error) {
	var destinations []Destination
	err := Transact(db, func(tx *DBTrans) error {
		_, err := tx.Select(&destinations, "select * from destinations")
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
