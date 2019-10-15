package dao

import (
	"strconv"

	datafeed "github.com/chef/automate/api/external/data_feed"
	"github.com/chef/automate/lib/errorutils"
	"github.com/pkg/errors"
)

type Destination struct {
	ID     int64  `db:"id"`
	Name   string `db:"name"`
	URL    string `db:"url"`
	Secret string `db:"secret"`
}

func (db *DB) addToDBDestination(inDestination *datafeed.AddDestinationRequest) *Destination {
	newDestination := Destination{}
	newDestination.ID = inDestination.Id
	newDestination.Name = inDestination.Name
	newDestination.URL = inDestination.Url
	newDestination.Secret = inDestination.Secret

	return &newDestination
}

func (db *DB) updateToDBDestination(inDestination *datafeed.UpdateDestinationRequest) *Destination {
	newDestination := Destination{}
	newDestination.ID = inDestination.Id
	newDestination.Name = inDestination.Name
	newDestination.URL = inDestination.Url
	newDestination.Secret = inDestination.Secret

	return &newDestination
}

func (db *DB) dbToGetDestinationResponse(inDestination *Destination) *datafeed.GetDestinationResponse {
	newDestination := datafeed.GetDestinationResponse{}
	newDestination.Id = inDestination.ID
	newDestination.Name = inDestination.Name
	newDestination.Url = inDestination.URL
	newDestination.Secret = inDestination.Secret

	return &newDestination
}

func (db *DB) getDestinationRequestToDB(inDestination *datafeed.GetDestinationRequest) *Destination {
	newDestination := Destination{}
	newDestination.ID = inDestination.Id

	return &newDestination
}

func (db *DB) AddDestination(destination *datafeed.AddDestinationRequest) (bool, error) {
	dbDestination := db.addToDBDestination(destination)
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

func (db *DB) DeleteDestination(delete *datafeed.DeleteDestinationRequest) (bool, error) {

	var count int64 = 0
	var err error
	err = Transact(db, func(tx *DBTrans) error {

		count, err = tx.Delete(&Destination{ID: delete.Id})
		if err != nil {
			return errorutils.ProcessSQLNotFound(err, strconv.FormatInt(delete.Id, 10), "DeleteDestination")
		}

		return nil
	})

	if err != nil || count == 0 {
		return false, err
	}
	return true, err
}

func (db *DB) UpdateDestination(destination *datafeed.UpdateDestinationRequest) (bool, error) {
	dbDestination := db.updateToDBDestination(destination)
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

func (db *DB) GetDestination(get *datafeed.GetDestinationRequest) (*datafeed.GetDestinationResponse, error) {

	var err error
	var obj interface{}
	var dest *Destination
	err = Transact(db, func(tx *DBTrans) error {
		// tx.Delete retuen count, error
		if obj, err = tx.Get(Destination{}, get.Id); err != nil {
			return errors.Wrap(err, "GetDestination: unable to get destination")
		}
		if obj == nil {
			dest = &Destination{}
			err = errorutils.ProcessSQLNotFound(errors.New("Record not found"), strconv.FormatInt(get.Id, 10), "GetDestination")
		} else {
			dest = obj.(*Destination)
		}
		return err
	})
	result := db.dbToGetDestinationResponse(dest)
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
	listOfDestinations := make([]*datafeed.GetDestinationResponse, 0)
	for _, d := range destinations {
		listOfDestinations = append(listOfDestinations, db.dbToGetDestinationResponse(&d))
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
