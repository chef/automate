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
	"github.com/sirupsen/logrus"
)

func (r *Runner) reportIt(ctx context.Context, job *types.InspecJob, content []byte, reportID string) {
	var report compliance.Report
	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	if err := unmarshaler.Unmarshal(bytes.NewReader(content), &report); err != nil {
		logrus.Errorf("reportToGRPC was unable to unmarshal the report output into a compliance.Report struct: %s", err.Error())
		return
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
	logrus.Debugf("hand-over report to ingest service")

	_, err := r.ingestClient.ProcessComplianceReport(ctx, &report)
	if err != nil {
		logrus.Errorf("reportToGRPC error calling ProcessComplianceReport: %s", err)
	}
}
