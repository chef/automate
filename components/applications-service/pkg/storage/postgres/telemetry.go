package postgres

import (
	"context"
	"time"

	"github.com/gofrs/uuid"
	"github.com/pkg/errors"
)

type Telemetry struct {
	ID                      string    `db:"id" json:"id"`
	LastTelemetryReportedAt time.Time `db:"last_telemetry_reported_at" json:"last_telemetry_reported_at"`
	CreatedAt               time.Time `db:"created_at" json:"created_at"`
}

//UpdateLastTelemetryReported Upsert the last application service telemetry reported date in postgres
func (pg *Postgres) UpdateTelemetryReported(ctx context.Context, lastTelemetryReportedTime string) error {

	err := Transact(pg, func(tx *DBTrans) error {
		lastTelemetryReportedAt, err := time.Parse(time.RFC3339, lastTelemetryReportedTime)
		if err != nil {
			return err
		}

		count, err := pg.DbMap.SelectInt("SELECT COUNT(*) FROM telemetry;")
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

// StoreTelemetry Store last application service telemetry reported timestamp
func (trans *DBTrans) StoreTelemetry(ctx context.Context, lastTelemetryReportedAt time.Time) error {
	telArr := make([]interface{}, 0)

	tel := Telemetry{
		ID:                      uuid.Must(uuid.NewV4()).String(),
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

// Get last services telemetry reported timestamp
func (db *Postgres) GetTelemetry(ctx context.Context) (Telemetry, error) {
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

// Get last 15 days services telemetry reported timestamp
func (db *Postgres) GetUniqueServicesFromPostgres(daysSinceLastPost int64, lastTelemetryReportedAt time.Time) (int64, error) {
	var count int64
	count, err := db.DbMap.SelectInt(`SELECT count (DISTINCT supervisor_id) from service_full where health_updated_at between now()::date - 16 AND now()::date - 1`)
	return int64(count), err
}
