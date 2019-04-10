package handler

import (
	"context"
	"fmt"

	"github.com/chef/automate/components/automate-gateway/api/nodes/manager"
	"github.com/chef/automate/components/automate-gateway/protobuf"
	jobsService "github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	nodeManagerService "github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/golang/protobuf/proto"
	gp "github.com/golang/protobuf/ptypes/empty"
	"github.com/sirupsen/logrus"
)

type NodeManager struct {
	client nodeManagerService.NodeManagerServiceClient
	jobs   jobsService.JobsServiceClient
}

func NewNodeManagerHandler(nodeManagerClient nodeManagerService.NodeManagerServiceClient, jobsClient jobsService.JobsServiceClient) *NodeManager {
	return &NodeManager{
		client: nodeManagerClient,
		jobs:   jobsClient,
	}
}

func (a *NodeManager) Create(ctx context.Context, in *manager.NodeManager) (*manager.Ids, error) {
	inDomain := &nodeManagerService.NodeManager{}
	out := &manager.Ids{}
	f := func() (proto.Message, error) {
		return a.client.Create(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	// create a detect job on each manager id
	for _, id := range out.Ids {
		_, err := a.jobs.Create(ctx, &jobsService.Job{
			Name: fmt.Sprintf("Automatic detect for new node manager nodes: %s", id.Id),
			Type: "detect",
			NodeSelectors: []*jobsService.ManagerFilter{
				{ManagerId: id.Id},
			},
			Status: types.StatusNew,
		})
		if err != nil {
			logrus.WithError(err).Errorf("unable to create detect job for manager %s", id.Id)
		}
	}

	return out, nil
}

func (a *NodeManager) Read(ctx context.Context, in *manager.Id) (*manager.NodeManager, error) {
	inDomain := &nodeManagerService.Id{}
	out := &manager.NodeManager{}
	f := func() (proto.Message, error) {
		return a.client.Read(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *NodeManager) Update(ctx context.Context, in *manager.NodeManager) (*gp.Empty, error) {
	inDomain := &nodeManagerService.NodeManager{}
	out := &gp.Empty{}
	f := func() (proto.Message, error) {
		return a.client.Update(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *NodeManager) Delete(ctx context.Context, in *manager.Id) (*gp.Empty, error) {
	inDomain := &nodeManagerService.Id{}
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

func (a *NodeManager) DeleteWithNodes(ctx context.Context, in *manager.Id) (*manager.Ids, error) {
	inDomain := &nodeManagerService.Id{}
	out := &manager.Ids{}
	f := func() (proto.Message, error) {
		return a.client.DeleteWithNodes(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *NodeManager) DeleteWithNodeStateStopped(ctx context.Context, in *manager.Id) (*gp.Empty, error) {
	inDomain := &nodeManagerService.Id{}
	out := &gp.Empty{}
	f := func() (proto.Message, error) {
		return a.client.DeleteWithNodeStateStopped(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *NodeManager) DeleteWithNodeStateTerminated(ctx context.Context, in *manager.Id) (*gp.Empty, error) {
	inDomain := &nodeManagerService.Id{}
	out := &gp.Empty{}
	f := func() (proto.Message, error) {
		return a.client.DeleteWithNodeStateTerminated(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *NodeManager) List(ctx context.Context, in *manager.Query) (*manager.NodeManagers, error) {
	inDomain := &nodeManagerService.Query{}
	out := &manager.NodeManagers{}
	f := func() (proto.Message, error) {
		return a.client.List(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *NodeManager) SearchNodeFields(ctx context.Context, in *manager.FieldQuery) (*manager.Fields, error) {
	inDomain := &nodeManagerService.FieldQuery{}
	out := &manager.Fields{}
	f := func() (proto.Message, error) {
		return a.client.SearchNodeFields(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *NodeManager) SearchNodes(ctx context.Context, in *manager.NodeQuery) (*manager.Nodes, error) {
	inDomain := &nodeManagerService.NodeQuery{}
	out := &manager.Nodes{}
	f := func() (proto.Message, error) {
		return a.client.SearchNodes(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *NodeManager) Connect(ctx context.Context, in *manager.Id) (*manager.ConnectResponse, error) {
	inDomain := &nodeManagerService.Id{}
	out := &manager.ConnectResponse{}
	f := func() (proto.Message, error) {
		return a.client.ConnectManager(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}
