package pipeline

import (
	"context"
	"errors"

	"github.com/sirupsen/logrus"

	"time"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/processor"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/publisher"
	"github.com/chef/automate/components/notifications-client/notifier"
)

var ErrQueueFull = errors.New("Report processing queue is full")

type Compliance struct {
	in chan<- message.Compliance
}

func NewCompliancePipeline(client *ingestic.ESClient, authzClient authz.ProjectsServiceClient,
	nodeMgrClient manager.NodeManagerServiceClient, messageBufferSize int, notifierClient notifier.Notifier, automateURL string) Compliance {
	in := make(chan message.Compliance, messageBufferSize)
	compliancePipeline(in,
		processor.ComplianceProfile(client),
		processor.ComplianceShared,
		processor.ComplianceSummary,
		processor.ComplianceReport(notifierClient, automateURL),
		processor.BundleReportProjectTagger(authzClient),
		publisher.BuildNodeManagerPublisher(nodeMgrClient),
		publisher.StoreCompliance(client, 100))
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
	select {
	case s.in <- msg:
	default:
		return ErrQueueFull
	}
	logrus.WithFields(logrus.Fields{"report_id": report.ReportUuid}).Debug("Running Compliance pipeline")
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
