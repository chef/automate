package grpc

import (
	"context"
	"errors"
	"io"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/golang/protobuf/ptypes/wrappers"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	grpccereal "github.com/chef/automate/api/interservice/cereal"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/backend"
)

var _ backend.Driver = &GrpcBackend{}

var errUnknownMessage = errors.New("Unknown message received")

type GrpcBackend struct {
	domain string
	client grpccereal.CerealClient
}

func NewGrpcBackend(domain string, client grpccereal.CerealClient) *GrpcBackend {
	return &GrpcBackend{
		domain: domain,
		client: client,
	}
}
func NewGrpcBackendFromConn(domain string, conn *grpc.ClientConn) *GrpcBackend {
	return &GrpcBackend{
		domain: domain,
		client: grpccereal.NewCerealClient(conn),
	}
}

func (g *GrpcBackend) DefaultTaskPollInterval() time.Duration {
	return 10 * time.Second
}

func (g *GrpcBackend) DefaultWorkflowPollInterval() time.Duration {
	return 2 * time.Second
}

func (g *GrpcBackend) EnqueueWorkflow(ctx context.Context, workflow *backend.WorkflowInstance) error {
	if _, err := g.client.EnqueueWorkflow(ctx, &grpccereal.EnqueueWorkflowRequest{
		Domain:       g.domain,
		InstanceName: workflow.InstanceName,
		WorkflowName: workflow.WorkflowName,
		Parameters:   workflow.Parameters,
	}); err != nil {
		if s, ok := status.FromError(err); ok {
			if s.Code() == codes.FailedPrecondition {
				return cereal.ErrWorkflowInstanceExists
			}
		}
		return err
	}
	return nil
}

type workflowCompleter struct {
	s     grpccereal.Cereal_DequeueWorkflowClient
	tasks []*grpccereal.Task
}

var _ backend.WorkflowCompleter = &workflowCompleter{}

func (c *workflowCompleter) EnqueueTask(task *backend.Task, opts backend.TaskEnqueueOpts) error {
	t := &grpccereal.Task{
		Name:       task.Name,
		Parameters: task.Parameters,
	}
	if !opts.StartAfter.IsZero() {
		ts, err := ptypes.TimestampProto(opts.StartAfter)
		if err != nil {
			return err
		}
		t.StartAfter = ts
	}
	c.tasks = append(c.tasks, t)
	return nil
}
func (c *workflowCompleter) finish(err error) error {
	defer c.s.CloseSend() // nolint: errcheck

	if err != nil {
		return err
	}

	if err := c.s.CloseSend(); err != nil {
		logrus.WithError(err).Error("Failed to continue workflow")
		return err
	}
	committedMsg, err := c.s.Recv()
	if err != nil {
		logrus.WithError(err).Error("Did not get committed message")
		return err
	}
	if committedMsg.GetCommitted() == nil {
		return errUnknownMessage
	}

	return nil
}

func (c *workflowCompleter) Continue(payload []byte) error {
	err := c.s.Send(&grpccereal.DequeueWorkflowRequest{
		Cmd: &grpccereal.DequeueWorkflowRequest_Continue_{
			Continue: &grpccereal.DequeueWorkflowRequest_Continue{
				Payload: payload,
				Tasks:   c.tasks,
			},
		},
	})
	return c.finish(err)
}

func (c *workflowCompleter) Fail(errMsg error) error {
	err := c.s.Send(&grpccereal.DequeueWorkflowRequest{
		Cmd: &grpccereal.DequeueWorkflowRequest_Fail_{
			Fail: &grpccereal.DequeueWorkflowRequest_Fail{
				Err: errMsg.Error(),
			},
		},
	})
	return c.finish(err)
}

func (c *workflowCompleter) Done(result []byte) error {
	err := c.s.Send(&grpccereal.DequeueWorkflowRequest{
		Cmd: &grpccereal.DequeueWorkflowRequest_Done_{
			Done: &grpccereal.DequeueWorkflowRequest_Done{
				Result: result,
			},
		},
	})
	return c.finish(err)
}

func (c *workflowCompleter) Close() error {
	return c.s.CloseSend()
}

func (g *GrpcBackend) DequeueWorkflow(ctx context.Context, workflowNames []string) (*backend.WorkflowEvent, backend.WorkflowCompleter, error) {
	s, err := g.client.DequeueWorkflow(ctx)
	if err != nil {
		return nil, nil, err
	}

	if err := s.Send(&grpccereal.DequeueWorkflowRequest{
		Cmd: &grpccereal.DequeueWorkflowRequest_Dequeue_{
			Dequeue: &grpccereal.DequeueWorkflowRequest_Dequeue{
				Domain:        g.domain,
				WorkflowNames: workflowNames,
			},
		},
	}); err != nil {
		return nil, nil, err
	}

	resp, err := s.Recv()
	if err != nil {
		if st, ok := status.FromError(err); ok {
			if st.Code() == codes.NotFound {
				return nil, nil, cereal.ErrNoWorkflowInstances
			}
		}
		return nil, nil, err
	}

	deq := resp.GetDequeue()
	if deq == nil {
		return nil, nil, errors.New("unexpected")
	}

	var taskResult *backend.TaskResult
	if tr := deq.GetEvent().GetTaskResult(); tr != nil {
		taskResult = &backend.TaskResult{
			TaskName:   tr.GetTaskName(),
			Parameters: tr.GetParameters(),
			Status:     backend.TaskStatusType(tr.GetStatus()),
			ErrorText:  tr.GetErrorText(),
			Result:     tr.GetResult(),
		}
	}
	backendInstance := grpcWorkflowInstanceToBackend(deq.GetInstance())
	wevt := &backend.WorkflowEvent{
		Instance:           backendInstance,
		Type:               backend.WorkflowEventType(deq.GetEvent().GetType()),
		EnqueuedTaskCount:  int(deq.GetEvent().GetEnqueuedTaskCount()),
		CompletedTaskCount: int(deq.GetEvent().GetCompletedTaskCount()),
		TaskResult:         taskResult,
	}

	return wevt, &workflowCompleter{
		s: s,
	}, nil
}

func grpcWorkflowInstanceToBackend(grpcInstance *grpccereal.WorkflowInstance) backend.WorkflowInstance {
	var err error
	if grpcInstance.Err != "" {
		err = errors.New(grpcInstance.Err)
	}
	return backend.WorkflowInstance{
		InstanceName: grpcInstance.GetInstanceName(),
		WorkflowName: grpcInstance.GetWorkflowName(),
		Status:       backend.WorkflowInstanceStatus(grpcInstance.GetStatus()),
		Parameters:   grpcInstance.GetParameters(),
		Payload:      grpcInstance.GetPayload(),
		Err:          err,
		Result:       grpcInstance.Result,
	}
}

func (g *GrpcBackend) CancelWorkflow(ctx context.Context, instanceName string, workflowName string) error {
	_, err := g.client.CancelWorkflow(ctx, &grpccereal.CancelWorkflowRequest{
		Domain:       g.domain,
		InstanceName: instanceName,
		WorkflowName: workflowName,
	})
	if err != nil {
		if st, ok := status.FromError(err); ok {
			if st.Code() == codes.NotFound {
				return cereal.ErrWorkflowInstanceNotFound
			}
		}
		return err
	}
	return nil
}

type taskCompleter struct {
	s        grpccereal.Cereal_DequeueTaskClient
	ctx      context.Context
	doneChan chan error
}

func (c *taskCompleter) Context() context.Context {
	return c.ctx
}

func (c *taskCompleter) Fail(errMsg string) error {
	defer c.s.CloseSend() // nolint: errcheck
	transformErr := func(err error) error {
		if err != nil {
			if st, ok := status.FromError(err); ok {
				if st.Code() == codes.FailedPrecondition {
					return cereal.ErrTaskLost
				}
			}
			return err
		}
		return nil
	}
	err := c.s.Send(&grpccereal.DequeueTaskRequest{
		Cmd: &grpccereal.DequeueTaskRequest_Fail_{
			Fail: &grpccereal.DequeueTaskRequest_Fail{
				Error: errMsg,
			},
		},
	})
	if err != nil {
		return transformErr(err)
	}

	// The stream will be closed once the server has processed the
	// request.
	// We must wait to figure out if it was successful
	err = <-c.doneChan
	if err != nil {
		return transformErr(err)
	}
	return nil
}

func (c *taskCompleter) Succeed(result []byte) error {
	defer c.s.CloseSend() // nolint: errcheck
	transformErr := func(err error) error {
		if err != nil {
			if st, ok := status.FromError(err); ok {
				if st.Code() == codes.FailedPrecondition {
					return cereal.ErrTaskLost
				}
			}
			return err
		}
		return nil
	}
	err := c.s.Send(&grpccereal.DequeueTaskRequest{
		Cmd: &grpccereal.DequeueTaskRequest_Succeed_{
			Succeed: &grpccereal.DequeueTaskRequest_Succeed{
				Result: result,
			},
		},
	})
	if err != nil {
		return transformErr(err)
	}

	// The stream will be closed once the server has processed the
	// request.
	// We must wait to figure out if it was successful
	err = <-c.doneChan
	if err == nil || err == io.EOF {
		return nil
	} else {
		return transformErr(err)
	}
}

func (g *GrpcBackend) DequeueTask(ctx context.Context, taskName string) (*backend.Task, backend.TaskCompleter, error) {
	s, err := g.client.DequeueTask(ctx)
	if err != nil {
		return nil, nil, err
	}

	if err := s.Send(&grpccereal.DequeueTaskRequest{
		Cmd: &grpccereal.DequeueTaskRequest_Dequeue_{
			Dequeue: &grpccereal.DequeueTaskRequest_Dequeue{
				Domain:   g.domain,
				TaskName: taskName,
			},
		},
	}); err != nil {
		return nil, nil, err
	}

	resp, err := s.Recv()
	if err != nil {
		if st, ok := status.FromError(err); ok {
			if st.Code() == codes.NotFound {
				return nil, nil, cereal.ErrNoTasks
			}
		}
		return nil, nil, err
	}

	deq := resp.GetDequeue()
	if deq == nil {
		return nil, nil, errors.New("invalid msg")
	}

	taskCtx, cancel := context.WithCancel(ctx)
	doneChan := make(chan error, 1)
	go func() {
		// This goroutine will read the next, and what must be the last
		// message.
		var errOut error
		msg, err := s.Recv()
		if err != nil {
			logrus.WithError(err).Debug("Received error while waiting for commited message")
			errOut = err
		} else if c := msg.GetCancel(); c != nil {
			logrus.Debug("Received cancel while waiting for commited message")
			errOut = context.Canceled
		} else if c := msg.GetCommitted(); c != nil {
			logrus.Debug("Received comitted")
			errOut = nil
		} else {
			errOut = errUnknownMessage
		}
		cancel()
		doneChan <- errOut
	}()
	return &backend.Task{
			Name:       deq.GetTask().GetName(),
			Parameters: deq.GetTask().GetParameters(),
		}, &taskCompleter{
			s:        s,
			ctx:      taskCtx,
			doneChan: doneChan,
		}, nil
}

func (g *GrpcBackend) CreateWorkflowSchedule(ctx context.Context, instanceName string, workflowName string, parameters []byte, enabled bool, recurrence string, nextRunAt time.Time) error {
	nextRunAtProto, err := ptypes.TimestampProto(nextRunAt)
	if err != nil {
		return err
	}
	_, err = g.client.CreateWorkflowSchedule(ctx, &grpccereal.CreateWorkflowScheduleRequest{
		Domain:       g.domain,
		InstanceName: instanceName,
		WorkflowName: workflowName,
		Parameters:   parameters,
		Enabled:      enabled,
		Recurrence:   recurrence,
		NextRunAt:    nextRunAtProto,
	})
	if err != nil {
		if st, ok := status.FromError(err); ok {
			if st.Code() == codes.FailedPrecondition {
				return cereal.ErrWorkflowScheduleExists
			}
		}
		return err
	}
	return nil
}

func (g *GrpcBackend) ListWorkflowSchedules(ctx context.Context) ([]*backend.Schedule, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	s, err := g.client.ListWorkflowSchedules(ctx, &grpccereal.ListWorkflowSchedulesRequest{
		Domain: g.domain,
	})
	if err != nil {
		return nil, err
	}
	var schedules []*backend.Schedule

	for {
		resp, err := s.Recv()
		if err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}
		if resp.NumSchedules > 0 && schedules == nil {
			schedules = make([]*backend.Schedule, 0, resp.NumSchedules)
		}
		if resp.Schedule != nil {
			schedules = append(schedules, grpcSchedToBackend(resp.Schedule))
		}
	}
	return schedules, nil
}

func (g *GrpcBackend) GetWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string) (*backend.Schedule, error) {
	resp, err := g.client.GetWorkflowScheduleByName(ctx, &grpccereal.GetWorkflowScheduleByNameRequest{
		Domain:       g.domain,
		InstanceName: instanceName,
		WorkflowName: workflowName,
	})
	if err != nil {
		if st, ok := status.FromError(err); ok {
			if st.Code() == codes.NotFound {
				return nil, cereal.ErrWorkflowScheduleNotFound
			}
		}
		return nil, err
	}
	if resp.Schedule == nil {
		logrus.Error("Missing schedule")
		return nil, cereal.ErrWorkflowScheduleNotFound
	}
	return grpcSchedToBackend(resp.Schedule), nil
}

func grpcSchedToBackend(grpcSched *grpccereal.Schedule) *backend.Schedule {
	schedule := &backend.Schedule{
		InstanceName: grpcSched.InstanceName,
		WorkflowName: grpcSched.WorkflowName,
		Parameters:   grpcSched.Parameters,
		Recurrence:   grpcSched.Recurrence,
		Enabled:      grpcSched.Enabled,
	}
	if grpcSched.NextDueAt != nil {
		ts, err := ptypes.Timestamp(grpcSched.NextDueAt)
		if err != nil {
			logrus.WithError(err).Warn("Could not decode NextDueAt timestamp")
		}
		schedule.NextDueAt = ts
	}

	if grpcSched.LastEnqueuedAt != nil {
		ts, err := ptypes.Timestamp(grpcSched.LastEnqueuedAt)
		if err != nil {
			logrus.WithError(err).Warn("Could not decode LastEnqueuedAt timestamp")
		} else {
			schedule.LastEnqueuedAt = ts
		}
	}

	if grpcSched.LastStart != nil {
		ts, err := ptypes.Timestamp(grpcSched.LastStart)
		if err != nil {
			logrus.WithError(err).Warn("Could not decode LastStart timestamp")
		} else {
			schedule.LastStart = &ts
		}
	}

	if grpcSched.LastEnd != nil {
		ts, err := ptypes.Timestamp(grpcSched.LastEnd)
		if err != nil {
			logrus.WithError(err).Warn("Could not decode LastEnd timestamp")
		} else {
			schedule.LastEnd = &ts
		}
	}
	return schedule
}

func (g *GrpcBackend) UpdateWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string, opts backend.WorkflowScheduleUpdateOpts) error {
	req := grpccereal.UpdateWorkflowScheduleByNameRequest{
		Domain:       g.domain,
		InstanceName: instanceName,
		WorkflowName: workflowName,
	}

	if opts.UpdateEnabled {
		req.Enabled = &wrappers.BoolValue{
			Value: opts.Enabled,
		}
	}

	if opts.UpdateParameters {
		req.Parameters = &wrappers.BytesValue{
			Value: opts.Parameters,
		}
	}

	if opts.UpdateRecurrence {
		req.Recurrence = &wrappers.StringValue{
			Value: opts.Recurrence,
		}
		ts, err := ptypes.TimestampProto(opts.NextRunAt)
		if err != nil {
			return err
		}
		req.NextRunAt = ts
	}

	_, err := g.client.UpdateWorkflowScheduleByName(ctx, &req)
	if err != nil {
		if st, ok := status.FromError(err); ok {
			if st.Code() == codes.NotFound {
				return cereal.ErrWorkflowScheduleNotFound
			}
		}
		return err
	}

	return nil
}

func (g *GrpcBackend) GetWorkflowInstanceByName(ctx context.Context, instanceName string, workflowName string) (*backend.WorkflowInstance, error) {
	resp, err := g.client.GetWorkflowInstanceByName(ctx, &grpccereal.GetWorkflowInstanceByNameRequest{
		Domain:       g.domain,
		InstanceName: instanceName,
		WorkflowName: workflowName,
	})
	if err != nil {
		if st, ok := status.FromError(err); ok {
			if st.Code() == codes.NotFound {
				return nil, cereal.ErrWorkflowInstanceNotFound
			}
		}
		return nil, err
	}
	if resp.WorkflowInstance == nil {
		logrus.Error("Missing workflow instance")
		return nil, cereal.ErrWorkflowInstanceNotFound
	}
	backendInstance := grpcWorkflowInstanceToBackend(resp.WorkflowInstance)
	return &backendInstance, nil
}

func (g *GrpcBackend) ListWorkflowInstances(ctx context.Context, opts backend.ListWorkflowOpts) ([]*backend.WorkflowInstance, error) {
	req := grpccereal.ListWorkflowInstancesRequest{
		Domain: g.domain,
	}
	if opts.WorkflowName != nil {
		req.WorkflowName = &wrappers.StringValue{
			Value: *opts.WorkflowName,
		}
	}
	if opts.InstanceName != nil {
		req.InstanceName = &wrappers.StringValue{
			Value: *opts.InstanceName,
		}
	}
	if opts.IsRunning != nil {
		req.IsRunning = &wrappers.BoolValue{
			Value: *opts.IsRunning,
		}
	}

	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	stream, err := g.client.ListWorkflowInstances(ctx, &req)
	if err != nil {
		return nil, err
	}

	instances := []*backend.WorkflowInstance{}
	for {
		instance, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}
		if instance.WorkflowInstance == nil {
			logrus.Error("Received nil workflow instance")
		}
		backendInstance := grpcWorkflowInstanceToBackend(instance.WorkflowInstance)
		instances = append(instances, &backendInstance)
	}
	return instances, nil
}

func (*GrpcBackend) Init() error {
	return nil
}

func (g *GrpcBackend) Close() error {
	return nil
}
