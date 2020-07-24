package message

import (
	"context"

	"time"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	reportingTypes "github.com/chef/automate/components/compliance-service/reporting"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/sirupsen/logrus"
)

type CompliancePipe func(<-chan Compliance) <-chan Compliance

func Propagate(out chan<- Compliance, msg *Compliance) {
	select {
	case out <- *msg:
	case <-msg.Ctx.Done():
		msg.FinishProcessingCompliance(msg.Ctx.Err())
	}
}

type Compliance struct {
	QueueTime      time.Time
	Report         compliance.Report
	InspecSummary  *relaxting.ESInSpecSummary
	InspecReport   *relaxting.ESInSpecReport
	InspecProfiles []*relaxting.ESInspecProfile
	Ctx            context.Context
	Done           chan<- error

	Shared struct {
		PerProfileSums        []relaxting.ESInSpecSummaryProfile
		AllProfileSums        *reportingTypes.NodeControlSummary
		EsProfilesMissingMeta map[string]interface{}
		EndTime               time.Time
		Status                string
		StatusMessage         string
	}
}

func (msg Compliance) FinishProcessingCompliance(err error) {
	msg.Done <- err

	// Adding new metric; Time for a message to go through the pipeline
	fields := logrus.Fields{
		"message": "ComplianceRun",
		"metric":  "pipeline",
		"type":    "ingest_time",
		"ms":      msg.ClockProcessingTime(),
	}
	if err != nil {
		logrus.WithFields(fields).Error("Unable to ingest message")
	} else {
		logrus.WithFields(fields).Info("Compliance ingested successfully")
	}
}

// ClockProcessingTime will return the time that has passed (in milliseconds) since
// the message got started until 'Now()'
//
// Useful to check the time that messages take to go through the pipeline
func (msg *Compliance) ClockProcessingTime() int64 {
	var (
		t       = time.Now()
		elapsed = t.Sub(msg.QueueTime)
	)

	return elapsed.Nanoseconds() / int64(time.Millisecond)
}
