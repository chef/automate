package publisher

import (
	"context"
	"fmt"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/processor"
	"github.com/chef/automate/lib/cereal"
	"sync"
	"time"

	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// StoreCompliance uses the publishers to save the data in ElasticSearch
func StoreCompliance(cerealService *cereal.Manager, client *ingestic.ESClient, numberOfPublishers int) message.CompliancePipe {
	logrus.Debugf("StoreCompliance started with numberOfPublishers = %d", numberOfPublishers)
	return func(in <-chan message.Compliance) <-chan message.Compliance {
		out := make(chan message.Compliance, 50)

		for i := 0; i < numberOfPublishers; i++ {
			go storeCompliance(in, out, cerealService, client, i)
		}

		return out
	}
}

func storeCompliance(in <-chan message.Compliance, out chan<- message.Compliance, cerealManager *cereal.Manager, client *ingestic.ESClient, number int) {
	for msg := range in {
		// Finish the ingestion if we have a DeadlineExceeded or Canceled context
		err := msg.Ctx.Err()
		if err != nil {
			errCode := codes.Unknown
			if err == context.DeadlineExceeded {
				errCode = codes.DeadlineExceeded
			}
			msg.FinishProcessingCompliance(status.Errorf(errCode, "Error ingesting report %s: %s", msg.Report.ReportUuid, err.Error()))
			continue
		}

		var megaErr error
		logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Debug("Publishing Compliance Report")

		errChannels := make([]<-chan error, 0)
		for _, profile := range msg.InspecProfiles {
			// Only add the profile to the metadata store if it's missing
			if _, ok := msg.Shared.EsProfilesMissingMeta[profile.Sha256]; ok {
				errChannels = append(errChannels, insertInspecProfile(msg, profile, client))
			} else {
				logrus.Debugf("storeCompliance: meta for profile %s already exists, no need to insert", profile.Sha256)
			}
		}
		errChannels = append(errChannels, insertInspecSummary(msg, client))
		errChannels = append(errChannels, insertInspecReport(msg, client, cerealManager))
		errChannels = append(errChannels, insertInspecReportRunInfo(msg, client))

		for err := range merge(errChannels...) {
			if err != nil {
				if megaErr != nil {
					megaErr = fmt.Errorf(err.Error() + " " + megaErr.Error())
				} else {
					megaErr = err
				}
			}
		}
		if megaErr != nil {
			msg.FinishProcessingCompliance(status.Errorf(codes.Internal, megaErr.Error()))
		} else {
			message.Propagate(out, &msg)
		}
		logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Debug("Published Compliance Report")

	}
	close(out)
}

func insertInspecSummary(msg message.Compliance, client *ingestic.ESClient) <-chan error {
	out := make(chan error)
	go func() {
		logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid}).Debug("Ingesting inspec_summary")
		start := time.Now()
		err := client.InsertInspecSummary(msg.Ctx, msg.Report.ReportUuid, msg.Shared.EndTime, msg.InspecSummary)
		if err != nil {
			logrus.WithFields(logrus.Fields{"error": err.Error()}).Error("Unable to ingest inspec_summary object")
			out <- err
		} else {
			out <- nil
		}
		logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid, "took": time.Since(start).Truncate(time.Millisecond)}).Debug("InsertInspecSummary")
		close(out)
	}()
	return out
}

func insertInspecReport(msg message.Compliance, client *ingestic.ESClient, cerealManager *cereal.Manager) <-chan error {
	out := make(chan error)
	go func() {
		logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid, "node_id": msg.Report.NodeUuid}).Debug("Ingesting inspec_report")
		start := time.Now()
		err := client.InsertInspecReport(msg.Ctx, msg.Report.ReportUuid, msg.Shared.EndTime, msg.InspecReport)
		if err != nil {
			logrus.WithFields(logrus.Fields{"error": err.Error()}).Error("Unable to ingest inspec_report object")
			out <- err
		} else {
			out <- nil
		}
		logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid, "took": time.Since(start).Truncate(time.Millisecond)}).Debug("InsertInspecReport")
		logrus.Infof("Enqueue workflow started at %v", time.Now())
		errWorkflow := cerealManager.EnqueueWorkflow(context.TODO(), processor.ReportWorkflowName,
			fmt.Sprintf("%s-%s", "control-workflow", msg.Report.ReportUuid),
			processor.ControlWorkflowParameters{
				ReportUuid: msg.Report.ReportUuid,
				Retries:    2,
				EndTime:    msg.Shared.EndTime,
			})

		logrus.Infof("Enqueue completed at %v", time.Now())

		if errWorkflow != nil {
			fmt.Errorf("error in enqueuing the  workflow for request id %s: %w", msg.Report.ReportUuid, err)
		}
		close(out)
	}()
	return out
}

func insertInspecReportRunInfo(msg message.Compliance, client *ingestic.ESClient) <-chan error {
	out := make(chan error)
	go func() {
		logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid, "node_id": msg.Report.NodeUuid}).Debug("Ingesting inspec_report")
		start := time.Now()

		err := client.InsertComplianceRunInfo(msg.Ctx, msg.InspecReport, msg.Shared.EndTime)
		if err != nil {
			logrus.WithFields(logrus.Fields{"error": err.Error()}).Error("Unable to ingest inspec_report object")
			out <- err
		} else {
			out <- nil
		}
		logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid, "took": time.Since(start).Truncate(time.Millisecond)}).Debug("InsertInspecReport")
		close(out)
	}()
	return out
}

func insertInspecProfile(msg message.Compliance, profile *relaxting.ESInspecProfile, client *ingestic.ESClient) <-chan error {
	out := make(chan error)
	go func() {
		logrus.WithFields(logrus.Fields{
			"report_id":  msg.Report.ReportUuid,
			"profile_id": profile.Sha256,
		}).Debug("Ingesting inspec_profile")
		start := time.Now()

		err := client.InsertInspecProfile(msg.Ctx, profile)
		if err != nil {
			logrus.WithFields(logrus.Fields{"error": err.Error()}).Error("Unable to ingest inspec_profile object")
			out <- err
		} else {
			out <- nil
		}
		logrus.WithFields(logrus.Fields{"report_id": msg.Report.ReportUuid, "took": time.Since(start).Truncate(time.Millisecond)}).Debug("InsertInspecProfile")
		close(out)
	}()
	return out
}

func merge(cs ...<-chan error) <-chan error {
	var wg sync.WaitGroup
	out := make(chan error)

	// Start an output goroutine for each input channel in cs.  output
	// copies values from c to out until c is closed, then calls wg.Done.
	output := func(c <-chan error) {
		for err := range c {
			out <- err
		}
		wg.Done()
	}
	wg.Add(len(cs))
	for _, c := range cs {
		go output(c)
	}

	// Start a goroutine to close out once all the output goroutines are
	// done.  This must start after the wg.Add call.
	go func() {
		wg.Wait()
		close(out)
	}()
	return out
}
