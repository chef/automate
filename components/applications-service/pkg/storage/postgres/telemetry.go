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
