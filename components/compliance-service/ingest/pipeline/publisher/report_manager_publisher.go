package publisher

import (
	"bytes"
	"context"
	"encoding/json"
	"io"

	"github.com/chef/automate/api/interservice/report_manager"
	manager "github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/message"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

const maxSize = 1 << 20

func ReportManagerPublisher(reportMgrClient manager.ReportManagerServiceClient) message.CompliancePipe {
	return func(in <-chan message.Compliance) <-chan message.Compliance {
		return reportManagerPublisher(in, reportMgrClient)
	}
}

func reportManagerPublisher(in <-chan message.Compliance, reportmanagerClient manager.ReportManagerServiceClient) <-chan message.Compliance {
	out := make(chan message.Compliance, 100)
	go func() {
		defer close(out)
		for msg := range in {
			var grpcErr error
			err := msg.Ctx.Err()
			if err != nil {
				errCode := codes.Unknown
				if err == context.DeadlineExceeded {
					errCode = codes.DeadlineExceeded
				}
				msg.FinishProcessingCompliance(status.Errorf(errCode, "Error ingesting report to report manager %s: %s", msg.Report.ReportUuid, err.Error()))
				continue
			}
			stream, err := reportmanagerClient.StoreReport(msg.Ctx)
			if err != nil {
				msg.FinishProcessingCompliance(status.Errorf(codes.Internal, "Unable to get report stream: %s", err))
				continue
			}
			body, err := json.Marshal(msg.Report)
			if err != nil {
				msg.FinishProcessingCompliance(status.Errorf(codes.Internal, err.Error()))
				continue
			}
			reader := bytes.NewReader(body)
			buffer := make([]byte, maxSize)

			// loop until all the data are sent until EOF reaches or if any error occur
			for {
				n, err := reader.Read(buffer)
				if err == io.EOF {
					break
				}
				if err != nil {
					grpcErr = status.Errorf(codes.Internal, "Read report buffer error: %s", err)
					break
				}
				request := &report_manager.StoreReportRequest{Content: buffer[:n]}
				logrus.Debugf("sending %d bytes", n)
				err = stream.Send(request)
				if err != nil {
					grpcErr = status.Errorf(codes.Internal, "Unable to send report stream: %s", err)
					break
				}
			}
			_, err = stream.CloseAndRecv()
			if err != nil || grpcErr != nil {
				if grpcErr != nil {
					msg.FinishProcessingCompliance(grpcErr)
					continue
				}
				msg.FinishProcessingCompliance(status.Errorf(codes.Internal, err.Error()))
				continue
			}
			message.Propagate(out, &msg)
		}
	}()
	return out
}
