package compliance

import (
	"context"

	"github.com/chef/automate/components/automate-gateway/api/compliance/scanner/jobs"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
	"github.com/chef/automate/components/automate-gateway/protobuf"
	jobsService "github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/golang/protobuf/proto"
	gp "github.com/golang/protobuf/ptypes/empty"
)

type Jobs struct {
	client jobsService.JobsServiceClient
}

// asserts that we satisfy the correct interface here -- it's a safeguard
var _ middleware.AuthContextReader = (*Jobs)(nil)

func (*Jobs) AuthContextRead() {}

func NewJobsHandler(jobsClient jobsService.JobsServiceClient) *Jobs {
	return &Jobs{
		client: jobsClient,
	}
}

func (a *Jobs) Create(ctx context.Context, in *jobs.Job) (*jobs.Id, error) {
	inDomain := &jobsService.Job{}
	out := &jobs.Id{}
	f := func() (proto.Message, error) {
		return a.client.Create(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Jobs) Read(ctx context.Context, in *jobs.Id) (*jobs.Job, error) {
	inDomain := &jobsService.Id{}
	out := &jobs.Job{}
	f := func() (proto.Message, error) {
		return a.client.Read(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Jobs) Update(ctx context.Context, in *jobs.Job) (*gp.Empty, error) {
	inDomain := &jobsService.Job{}
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

func (a *Jobs) Delete(ctx context.Context, in *jobs.Id) (*gp.Empty, error) {
	inDomain := &jobsService.Id{}
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

func (a *Jobs) List(ctx context.Context, in *jobs.Query) (*jobs.Jobs, error) {
	inDomain := &jobsService.Query{}
	out := &jobs.Jobs{}
	f := func() (proto.Message, error) {
		return a.client.List(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (a *Jobs) Rerun(ctx context.Context, in *jobs.Id) (*jobs.RerunResponse, error) {
	inDomain := &jobsService.Id{}
	out := &jobs.RerunResponse{}
	f := func() (proto.Message, error) {
		return a.client.Rerun(ctx, inDomain)
	}
	err := protobuf.CallDomainService(in, inDomain, f, out)
	if err != nil {
		return nil, err
	}
	return out, nil
}
