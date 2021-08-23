package dao

import (
	"context"
	"database/sql"
	"encoding/json"
	"strconv"

	"github.com/lib/pq"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/common/query"
	datafeed "github.com/chef/automate/api/external/data_feed"
	"github.com/chef/automate/api/external/lib/errorutils"
	"github.com/pkg/errors"
)

const uniqueViolation = "23505"

type Destination struct {
	ID               int64  `db:"id"`
	Name             string `db:"name"`
	URL              string `db:"url"`
	Secret           string `db:"secret"`
	Services         string `db:"services"`
	IntegrationTypes string `db:"integration_types"`
	MetaData         string `db:"meta_data"`
	Enable           bool   `db:"enable"`
}

func addToDBDestination(inDestination *datafeed.AddDestinationRequest) (*Destination, error) {
	newDestination := Destination{}
	newDestination.ID = inDestination.Id
	newDestination.Name = inDestination.Name
	newDestination.URL = inDestination.Url
	newDestination.Secret = inDestination.Secret
	newDestination.Services = inDestination.Services
	newDestination.IntegrationTypes = inDestination.IntegrationTypes
	newDestination.Enable = inDestination.Enable
	if len(inDestination.MetaData) > 0 {
		zaMap := make(map[string]string, 0)
		for _, kv := range inDestination.MetaData {
			zaMap[kv.Key] = kv.Value
		}
		jsonMap, err := json.Marshal(zaMap)
		if err != nil {
			logrus.Println(errors.Wrap(err, "keyValueToRawMap unable to marshal map"))
			return nil, err
		}
		newDestination.MetaData = string(jsonMap)
	} else {
		newDestination.MetaData = ""
	}
	return &newDestination, nil
}

func updateToDBDestination(inDestination *datafeed.UpdateDestinationRequest) (*Destination, error) {
	newDestination := Destination{}
	newDestination.ID, _ = strconv.ParseInt(inDestination.Id, 10, 64)
	newDestination.Name = inDestination.Name
	newDestination.URL = inDestination.Url
	newDestination.Secret = inDestination.Secret
	newDestination.Services = inDestination.Services
	newDestination.IntegrationTypes = inDestination.IntegrationTypes
	newDestination.Enable = inDestination.Enable
	if len(inDestination.MetaData) > 0 {
		zaMap := make(map[string]string, 0)
		for _, kv := range inDestination.MetaData {
			zaMap[kv.Key] = kv.Value
		}
		jsonMap, err := json.Marshal(zaMap)
		if err != nil {
			logrus.Println(errors.Wrap(err, "keyValueToRawMap unable to marshal map"))
			return nil, err
		}
		newDestination.MetaData = string(jsonMap)
	} else {
		newDestination.MetaData = ""
	}

	return &newDestination, nil
}
func updateToDBDestinationEnable(inDestination *datafeed.UpdateDestinationEnableRequest) *Destination {
	newDestination := Destination{}
	newDestination.ID, _ = strconv.ParseInt(inDestination.Id, 10, 64)
	newDestination.Enable = inDestination.Enable
	return &newDestination
}

func dbToGetDestinationResponse(inDestination *Destination) (*datafeed.GetDestinationResponse, error) {
	newDestination := datafeed.GetDestinationResponse{}
	newDestination.Id = inDestination.ID
	newDestination.Name = inDestination.Name
	newDestination.Url = inDestination.URL
	newDestination.Secret = inDestination.Secret
	newDestination.Services = inDestination.Services
	newDestination.IntegrationTypes = inDestination.IntegrationTypes
	newDestination.Enable = inDestination.Enable
	if inDestination.MetaData != "" {
		var zaMap map[string]string
		err := json.Unmarshal([]byte(inDestination.MetaData), &zaMap)
		if err != nil {
			logrus.Println(errors.Wrap(err, "rawMapToKeyValue unable to unmarshal map"))
			return nil, err
		}

		zaArray := make([]*query.Kv, 0, len(zaMap))
		for k, v := range zaMap {
			zaArray = append(zaArray, &query.Kv{Key: k, Value: v})
		}
		newDestination.MetaData = zaArray
	} else {
		newDestination.MetaData = []*query.Kv{}
	}

	return &newDestination, nil
}

func (db *DB) AddDestination(destination *datafeed.AddDestinationRequest) (int64, error) {
	dbDestination, err := addToDBDestination(destination)
	if err != nil {
		return -1, err
	}
	err = Transact(db, func(tx *DBTrans) error {
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
	return err
}

func (db *DB) UpdateDestination(destination *datafeed.UpdateDestinationRequest) error {
	var err error
	dbDestination, err := updateToDBDestination(destination)
	if err != nil {
		return err
	}
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
	if err != nil {
		return nil, err
	}
	result, err := dbToGetDestinationResponse(dest)
	if err != nil {
		return nil, err
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
		result, err := dbToGetDestinationResponse(&d)
		if err != nil {
			return nil, err
		}
		listOfDestinations = append(listOfDestinations, result)
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

func (db *DB) EnableDestination(ctx context.Context, destination *datafeed.UpdateDestinationEnableRequest) error {

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	_, err := db.Db.ExecContext(ctx,
		`UPDATE destinations SET enable =$2 WHERE id = $1`,
		destination.Id, destination.Enable)
	if err != nil {
		return errors.Wrap(err, "failed to update EnableDestination")
	}
	return nil
}
