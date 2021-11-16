package postgres

import (
	"context"
	"time"

	"github.com/chef/automate/components/applications-service/pkg/storage"
	"github.com/gofrs/uuid"
	"github.com/pkg/errors"
)

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

	tel := storage.Telemetry{
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
func (db *Postgres) GetTelemetry(ctx context.Context) (storage.Telemetry, error) {
	var t storage.Telemetry
	rows, err := db.Query(`SELECT id,last_telemetry_reported_at, created_at from telemetry`)
	if err != nil {
		return storage.Telemetry{}, err
	}
	for rows.Next() {
		err = rows.Scan(&t.ID, &t.LastTelemetryReportedAt, &t.CreatedAt)
		if err != nil {
			return storage.Telemetry{}, err
		}
	}
	return t, nil
}

// Get last 15 days services telemetry reported timestamp
func (db *Postgres) GetUniqueServicesFromPostgres(daysSinceLastPost int64, lastTelemetryReportedAt time.Time) (int64, error) {
	var count int64
	var err error
	lastTelemetryReportedDate := lastTelemetryReportedAt.Format("2006-01-02")
	// if daysSinceLastPost >= three months then take the unique nodes count from last three months
	// and if daysSinceLastPost > 15 and < three months then take unique nodes count from lastTelemetryReportedDate to yesterday EOD
	// else take the unique nodes count from last 15 days
	if daysSinceLastPost >= 90 {
		count, err = db.DbMap.SelectInt(`SELECT count (DISTINCT supervisor_id) from service_full where health_updated_at > now()::date - 91 AND health_updated_at < now()::date`)
	} else if daysSinceLastPost > 15 {
		count, err = db.DbMap.SelectInt(
			`SELECT count (DISTINCT supervisor_id) from service_full where health_updated_at > $1 AND health_updated_at < now()::date`, lastTelemetryReportedDate)
	} else {
		count, err = db.DbMap.SelectInt(`SELECT count (DISTINCT supervisor_id) from service_full where health_updated_at > now()::date - 16 AND health_updated_at < now()::date`)
	}
	return count, err
}
