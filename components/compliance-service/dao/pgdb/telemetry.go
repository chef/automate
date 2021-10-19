package pgdb

import (
	"context"
	"time"

	"github.com/chef/automate/api/interservice/compliance/stats"
	"github.com/pkg/errors"
)

type Telemetry struct {
	ID                      string    `db:"id" json:"id"`
	LastTelemetryReportedAt time.Time `db:"last_telemetry_reported_at" json:"last_telemetry_reported_at"`
	CreatedAt               time.Time `db:"created_at" json:"created_at"`
}

//UpdateLastTelemetryReported: Upsert the last compliance telemetry reported date in postgres
func (db *DB) UpdateLastTelemetryReported(ctx context.Context, req *stats.UpdateTelemetryReportedRequest) error {
	err := Transact(db, func(tx *DBTrans) error {
		lastTelemetryReportedAt, err := time.Parse(time.RFC3339, req.LastTelemetryReportedAt)
		if err != nil {
			return err
		}

		count, err := db.DbMap.SelectInt("SELECT COUNT(*) FROM telemetry;")
		if err != nil {
			return err
		}
		if count > 0 {
			updatetelemetryDate := `update telemetry set last_telemetry_reported_at=$1`
			_, err := tx.Exec(updatetelemetryDate, lastTelemetryReportedAt)
			if err != nil {
				return err
			}
		} else {
			err = tx.StoreTelemetry(ctx, lastTelemetryReportedAt)
			if err != nil {
				return errors.Wrap(err, "Failed to insert telemetry reported date")
			}
		}

		return nil
	})
	if err != nil {
		return err
	}
	return nil
}

// store last compliance telemetry reported timestamp
func (trans *DBTrans) StoreTelemetry(ctx context.Context, lastTelemetryReportedAt time.Time) error {
	telArr := make([]interface{}, 0)

	tel := Telemetry{
		ID:                      createUUID(),
		LastTelemetryReportedAt: lastTelemetryReportedAt,
		CreatedAt:               time.Now(),
	}
	telArr = append(telArr, &tel)
	err := trans.Insert(telArr...)
	if err != nil {
		return err
	}
	return nil
}

// Get last compliance telemetry reported timestamp
func (trans *DBTrans) GetTelemetry(ctx context.Context) (Telemetry, error) {
	var t []Telemetry
	stmt := `SELECT id,last_telemetry_reported_at, created_at from telemetry`
	_, err := trans.Select(&t, stmt)
	if err != nil {
		return Telemetry{}, err
	}
	if len(t) > 0 {
		return t[0], nil
	}
	return Telemetry{}, errors.New("Telemetry last reported entries not available")
}

// Update last compliance telemetry reported timestamp
func (trans *DBTrans) UpdateTelemetry(ctx context.Context, lastTelemetryReportedAt time.Time) error {
	tel := Telemetry{
		LastTelemetryReportedAt: lastTelemetryReportedAt,
	}
	_, err := trans.Update(&tel)
	if err != nil {
		return err
	}
	return nil
}

// Delete last compliance telemetry reported timestamp
func (trans *DBTrans) DeleteTelemetry(ctx context.Context, id string) error {
	tel := Telemetry{
		ID: id,
	}
	_, err := trans.Delete(tel)
	if err != nil {
		return err
	}
	return nil
}

// Get last compliance telemetry reported timestamp
func (db *DB) GetTelemetry(ctx context.Context) (Telemetry, error) {
	var t Telemetry
	rows, err := db.Query(`SELECT id,last_telemetry_reported_at, created_at from telemetry`)
	if err != nil {
		return Telemetry{}, err
	}
	for rows.Next() {
		err = rows.Scan(&t.ID, &t.LastTelemetryReportedAt, &t.CreatedAt)
		if err != nil {
			return Telemetry{}, err
		}
	}
	return t, nil
}
