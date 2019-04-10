package handler

import (
	"context"
	"net/url"

	"github.com/sirupsen/logrus"

	ingestReq "github.com/chef/automate/api/external/ingest/request"
	ingestResp "github.com/chef/automate/api/external/ingest/response"
	"github.com/chef/automate/api/external/common/version"

	"github.com/chef/automate/api/interservice/ingest"

	"github.com/chef/automate/components/notifications-client/builder"
	"github.com/chef/automate/components/notifications-client/notifier"
)

type ChefIngestServer struct {
	ingesterClient ingest.ChefIngesterClient
	notifierClient notifier.Notifier
	automateURL    string
}

func NewChefIngestServer(automateUrl *url.URL, ingesterClient ingest.ChefIngesterClient, notifierClient notifier.Notifier) *ChefIngestServer {
	return &ChefIngestServer{
		ingesterClient: ingesterClient,
		notifierClient: notifierClient,
		automateURL:    automateUrl.String(),
	}
}

func (s *ChefIngestServer) ProcessChefRun(ctx context.Context, request *ingestReq.Run) (*ingestResp.ProcessChefRunResponse, error) {
	ev, err := builder.ChefClientConverge(s.automateURL, request)
	if err != nil {
		// We treat notification errors as non fatal
		logrus.WithFields(logrus.Fields{"id": request.Id}).Warnf("Could not build Chef Client event: %v", err)
	} else {
		// This happens asynchronously
		s.notifierClient.Send(ctx, ev)
	}
	return s.ingesterClient.ProcessChefRun(ctx, request)
}

func (s *ChefIngestServer) ProcessChefAction(ctx context.Context, request *ingestReq.Action) (*ingestResp.ProcessChefActionResponse, error) {
	return s.ingesterClient.ProcessChefAction(ctx, request)
}

func (s *ChefIngestServer) ProcessLivenessPing(ctx context.Context, request *ingestReq.Liveness) (*ingestResp.ProcessLivenessResponse, error) {
	return s.ingesterClient.ProcessLivenessPing(ctx, request)
}

func (s *ChefIngestServer) ProcessMultipleNodeDeletes(ctx context.Context, request *ingestReq.MultipleNodeDeleteRequest) (*ingestResp.ProcessMultipleNodeDeleteResponse, error) {
	return s.ingesterClient.ProcessMultipleNodeDeletes(ctx, request)
}

func (s *ChefIngestServer) ProcessNodeDelete(ctx context.Context, request *ingestReq.Delete) (*ingestResp.ProcessNodeDeleteResponse, error) {
	// If service hostname is not set but remote hostname is, use remote hostname as service hostname
	// TODO @afiune Can we move this logic to the ingest service?
	if request.GetRemoteHostname() != "" && request.GetServiceHostname() == "" {
		iReq := &ingestReq.Delete{
			Id:               request.Id,
			NodeName:         request.NodeName,
			OrganizationName: request.OrganizationName,
			ServiceHostname:  request.RemoteHostname,
			NodeId:           request.NodeId,
		}
		return s.ingesterClient.ProcessNodeDelete(ctx, iReq)
	} else {
		return s.ingesterClient.ProcessNodeDelete(ctx, request)
	}
}

func (s *ChefIngestServer) GetVersion(ctx context.Context, empty *version.VersionInfoRequest) (*version.VersionInfo, error) {
	r, err := s.ingesterClient.GetVersion(ctx, &ingest.VersionRequest{})
	if err != nil {
		return nil, err
	}

	return &version.VersionInfo{
		Version: r.GetVersion(),
		Built:   r.GetBuilt(),
		Name:    r.GetName(),
		Sha:     r.GetSha(),
	}, nil
}
