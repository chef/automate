package pgdb

import (
	"context"
	"time"

	"github.com/pkg/errors"
)

type Telemetry struct {
	ID                      string
	LastTelemetryReportedAt time.Time
	CreatedAt               time.Time
}

// store last compliance telemetry reported timestamp
func (trans *DBTrans) StoreTelemetry(ctx context.Context, lastTelemetryReportedAt time.Time) error {
	telArr := make([]interface{}, 0)

	tel := Telemetry{
		ID:                      createUUID(),
		LastTelemetryReportedAt: lastTelemetryReportedAt,
		CreatedAt:               time.Now(),
	}
	telArr = append(telArr, tel)
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
	_, err := trans.Update(tel)
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
