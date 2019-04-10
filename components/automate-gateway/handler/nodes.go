package handler

import (
	"context"
	"fmt"
	"strconv"

	"github.com/chef/automate/components/automate-gateway/api/nodes"
	"github.com/chef/automate/components/automate-gateway/protobuf"
	jobsService "github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	nodesService "github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/golang/protobuf/proto"
	gp "github.com/golang/protobuf/ptypes/empty"
	"github.com/sirupsen/logrus"
)

type Nodes struct {
	client nodesService.NodesServiceClient
	jobs   jobsService.JobsServiceClient
}

func NewNodesHandler(nodesClient nodesService.NodesServiceClient, jobsClient jobsService.JobsServiceClient) *Nodes {
	return &Nodes{
		client: nodesClient,
		jobs:   jobsClient,
	}
}

// handleAutoDetectTags takes a node object and examines its tags.
// If a tag of '_no_auto_detect' is found with value 'true', this means we should not
// trigger a detect job on this node.  This tag is used for testing purposes, as well
// as by advanced scanner users who do not wish to spend the time running detect jobs
// on all their nodes. If the _no_auto_detect value is true, the function will return
// the node without the auto detect tag and a value of false
func handleAutoDetectTags(in *nodes.Node) (*nodes.Node, bool) {
	noAutoDetect := pgdb.FindKeyValue(in.Tags, "_no_auto_detect").Value
	noAutoDetectBool, err := strconv.ParseBool(noAutoDetect)
	if err != nil {
		return in, true
	}
	in.Tags = pgdb.RemoveKeyValue(in.Tags, "_no_auto_detect")
	return in, !noAutoDetectBool
}

func (a *Nodes) Create(ctx context.Context, in *nodes.Node) (*nodes.Id, error) {
	inDomain := &nodesService.Node{}
	out := &nodes.Id{}
	f := func() (proto.Message, error) {
		return a.client.Create(ctx, inDomain)
	}
	node, autoDetect := handleAutoDetectTags(in)
	err := protobuf.CallDomainService(node, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	if autoDetect {
		a.runDetectJob(ctx, in.Id)
	}
	return out, nil
}

func (a *Nodes) BulkCreate(ctx context.Context, in *nodes.Nodes) (*nodes.Ids, error) {
	inDomain := &nodesService.Nodes{}
	detectIds, noDetectIds := &nodes.Ids{}, &nodes.Ids{}
	f := func() (proto.Message, error) {
		return a.client.BulkCreate(ctx, inDomain)
	}
	// we need two arrays to differentiate between nodes we want to
	// run a detect job on and nodes we don't
	nodesListDetect := make([]*nodes.Node, 0)
	nodesListNoDetect := make([]*nodes.Node, 0)
	for _, node := range in.GetNodes() {
		node, autoDetect := handleAutoDetectTags(node)
		node.Manager = "automate"
		if autoDetect {
			nodesListDetect = append(nodesListDetect, node)
		} else {
			nodesListNoDetect = append(nodesListNoDetect, node)
		}
	}

	if len(nodesListDetect) > 0 {
		err := protobuf.CallDomainService(&nodes.Nodes{Nodes: nodesListDetect}, inDomain, f, detectIds)
		if err != nil {
			return nil, err
		}
		for _, id := range detectIds.GetIds() {
			a.runDetectJob(ctx, id)
		}
	}
	if len(nodesListNoDetect) > 0 {
		err := protobuf.CallDomainService(&nodes.Nodes{Nodes: nodesListNoDetect}, inDomain, f, noDetectIds)
		if err != nil {
			return nil, err
		}
	}
	ids := detectIds.GetIds()
	ids = append(ids, noDetectIds.GetIds()...)
	return &nodes.Ids{Ids: ids}, nil
}

func (a *Nodes) Read(ctx context.Context, in *nodes.Id) (*nodes.Node, error) {
	inDomain := &nodesService.Id{}
	out := &nodes.Node{}
	f := func() (proto.Message, error) {
		return a.client.Read(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	// need to get the job result for the node by calling the jobs service
	if len(out.GetLastJob().GetJobId()) > 0 {
		resultsRow, err := a.jobs.GetJobResultByNodeId(ctx, &jobsService.GetJobResultByNodeIdRequest{
			JobId:  out.GetLastJob().GetJobId(),
			NodeId: out.Id,
		})
		if err != nil {
			return nil, err
		}
		// append the job result for the node to the node
		out.LastJob = &nodes.ResultsRow{
			Result:    resultsRow.Result,
			JobId:     resultsRow.JobId,
			NodeId:    resultsRow.NodeId,
			Status:    resultsRow.Status,
			ReportId:  resultsRow.ReportId,
			StartTime: resultsRow.StartTime,
			EndTime:   resultsRow.EndTime,
		}
	}
	return out, nil
}

func (a *Nodes) Update(ctx context.Context, in *nodes.Node) (*gp.Empty, error) {
	inDomain := &nodesService.Node{}
	out := &gp.Empty{}
	f := func() (proto.Message, error) {
		return a.client.Update(ctx, inDomain)
	}
	node, autoDetect := handleAutoDetectTags(in)
	err := protobuf.CallDomainService(node, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	if autoDetect {
		a.runDetectJob(ctx, in.Id)
	}
	return out, nil
}

func (a *Nodes) Delete(ctx context.Context, in *nodes.Id) (*gp.Empty, error) {
	inDomain := &nodesService.Id{}
	out := &gp.Empty{}
	f := func() (proto.Message, error) {
		return a.client.Delete(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Nodes) List(ctx context.Context, in *nodes.Query) (*nodes.Nodes, error) {
	inDomain := &nodesService.Query{}
	out := &nodes.Nodes{}
	f := func() (proto.Message, error) {
		return a.client.List(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Nodes) BulkDeleteById(ctx context.Context, in *nodes.Ids) (*nodes.BulkDeleteResponse, error) {
	inDomain := &nodesService.Ids{}
	out := &nodes.BulkDeleteResponse{}
	f := func() (proto.Message, error) {
		return a.client.BulkDeleteById(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Nodes) Rerun(ctx context.Context, in *nodes.Id) (*nodes.RerunResponse, error) {
	// trigger a detect job. here we ignore any _no_auto_detect tags that are sent in
	// because the point of a rerun is to check if the node is reachable
	a.runDetectJob(ctx, in.Id)
	return &nodes.RerunResponse{}, nil
}

func (a *Nodes) BulkDelete(ctx context.Context, in *nodes.Query) (*nodes.BulkDeleteResponse, error) {
	inDomain := &nodesService.Query{}
	out := &nodes.BulkDeleteResponse{}
	f := func() (proto.Message, error) {
		return a.client.BulkDelete(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Nodes) runDetectJob(ctx context.Context, id string) {
	_, err := a.jobs.Create(ctx, &jobsService.Job{
		Name:   fmt.Sprintf("Automatic detect for new node: %s", id),
		Type:   "detect",
		Nodes:  []string{id},
		Status: types.StatusNew,
	})
	if err != nil {
		logrus.WithError(err).Errorf("unable to create detect job for manager %s", id)
	}
}
