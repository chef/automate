package runner

import (
	"bytes"
	"context"
	"net"
	"time"

	"github.com/chef/automate/components/compliance-service/ingest/events/compliance"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/golang/protobuf/jsonpb"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

func (r *Runner) reportIt(ctx context.Context, job *types.InspecJob, content []byte, reportID string) error {
	var report compliance.Report
	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	if err := unmarshaler.Unmarshal(bytes.NewReader(content), &report); err != nil {
		return errors.Wrap(err, "reportIt was unable to unmarshal the report output into a compliance.Report struct")
	}

	report.Environment = job.InspecBaseJob.NodeEnv
	if report.Environment == "" {
		report.Environment = "unknown"
	}
	report.Type = mappings.DocType
	report.NodeName = job.InspecBaseJob.NodeName
	report.NodeUuid = job.InspecBaseJob.NodeID
	report.ReportUuid = reportID
	report.JobUuid = job.JobID
	report.EndTime = time.Now().UTC().Format(time.RFC3339)
	report.SourceId = job.SourceID
	report.SourceRegion = job.TargetConfig.TargetBaseConfig.Region
	report.SourceAccountId = job.SourceAccountID
	ipAddress := net.ParseIP(job.TargetConfig.TargetBaseConfig.Hostname)
	if ipAddress != nil {
		report.Ipaddress = ipAddress.String()
	} else {
		report.Fqdn = job.TargetConfig.TargetBaseConfig.Hostname
	}
	report.Tags = job.Tags
	logrus.Debugf("hand-over report to ingest service")

	_, err := r.ingestClient.ProcessComplianceReport(ctx, &report)
	if err != nil {
		return errors.Wrap(err, "Report processing error")
	}
	return nil
}
