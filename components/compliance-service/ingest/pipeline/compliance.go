package pipeline

import (
	"context"

	"github.com/sirupsen/logrus"

	"time"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/processor"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/publisher"
)

type Compliance struct {
	in chan<- message.Compliance
}

func NewCompliancePipeline(client *ingestic.ESClient, authzClient iam_v2.ProjectsClient) Compliance {
	in := make(chan message.Compliance, 100)
	compliancePipeline(in,
		processor.ComplianceShared,
		processor.ComplianceSummary,
		processor.ComplianceReport,
		processor.ComplianceProfile,
		processor.BundleReportProjectTagger(authzClient),
		publisher.BuildCompliance(client, 100))
	return Compliance{in: in}
}

func (s *Compliance) Run(report *compliance.Report) error {
	done := make(chan error)
	defer close(done)
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	msg := message.Compliance{
		QueueTime: time.Now(),
		Report:    *report,
		Ctx:       ctx,
		Done:      done,
	}
	var err error
	logrus.WithFields(logrus.Fields{"report_id": report.ReportUuid}).Info("Running Compliance pipeline")

	s.in <- msg
	err = <-done
	if err != nil {
		logrus.WithFields(logrus.Fields{"error": err.Error()}).Error("Message failure")
	}
	return err
}

func (s *Compliance) Close() {
	close(s.in)
}

func compliancePipeline(source <-chan message.Compliance, pipes ...message.CompliancePipe) {
	go func() {
		for _, pipe := range pipes {
			source = pipe(source)
		}

		for msg := range source {
			msg.FinishProcessingCompliance(nil)
		}
	}()
}
