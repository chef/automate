package grpc

import (
	"context"
	"errors"
	"io"
	"time"

	"github.com/golang/protobuf/ptypes/wrappers"

	"github.com/golang/protobuf/ptypes"

	"github.com/chef/automate/lib/cereal"
	"github.com/sirupsen/logrus"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"

	"google.golang.org/grpc/status"

	grpccereal "github.com/chef/automate/api/interservice/cereal"
	"github.com/chef/automate/lib/cereal/backend"
)

var _ backend.Driver = &GrpcBackend{}

type GrpcBackend struct {
	client grpccereal.CerealClient
}

func NewGrpcBackend(client grpccereal.CerealClient) *GrpcBackend {
	return &GrpcBackend{
		client: client,
	}
}
func NewGrpcBackendFromConn(conn *grpc.ClientConn) *GrpcBackend {
	return &GrpcBackend{
		client: grpccereal.NewCerealClient(conn),
	}
}
func (g *GrpcBackend) EnqueueWorkflow(ctx context.Context, workflow *backend.WorkflowInstance) error {
	if _, err := g.client.EnqueueWorkflow(ctx, &grpccereal.EnqueueWorkflowRequest{
		InstanceName: workflow.InstanceName,
		WorkflowName: workflow.WorkflowName,
		Parameters:   workflow.Parameters,
	}); err != nil {
		if s, ok := status.FromError(err); ok {
			if s.Code() == codes.FailedPrecondition {
				return cereal.ErrWorkflowInstanceExists
			}
		} else {
			return err
		}
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

func (c *workflowCompleter) Continue(payload []byte) error {
	defer c.s.CloseSend() // nolint: errcheck
	err := c.s.Send(&grpccereal.DequeueWorkflowRequest{
		Cmd: &grpccereal.DequeueWorkflowRequest_Continue_{
			Continue: &grpccereal.DequeueWorkflowRequest_Continue{
				Payload: payload,
				Tasks:   c.tasks,
			},
		},
	})
	if err != nil {
		return err
	}
	if err := c.s.CloseSend(); err != nil {
		logrus.WithError(err).Error("failed to continue workflow")
		return err
	}
	_, _ = c.s.Recv()
	return nil
}

func (c *workflowCompleter) Fail(errMsg error) error {
	defer c.s.CloseSend() // nolint: errcheck
	err := c.s.Send(&grpccereal.DequeueWorkflowRequest{
		Cmd: &grpccereal.DequeueWorkflowRequest_Fail_{
			Fail: &grpccereal.DequeueWorkflowRequest_Fail{
				Err: errMsg.Error(),
			},
		},
	})
	if err != nil {
		return err
	}
	if err := c.s.CloseSend(); err != nil {
		logrus.WithError(err).Error("failed to fail workflow")
		return err
	}
	_, _ = c.s.Recv()
	return nil
}

func (c *workflowCompleter) Done(result []byte) error {
	defer c.s.CloseSend() // nolint: errcheck
	err := c.s.Send(&grpccereal.DequeueWorkflowRequest{
		Cmd: &grpccereal.DequeueWorkflowRequest_Done_{
			Done: &grpccereal.DequeueWorkflowRequest_Done{
				Result: result,
			},
		},
	})
	if err != nil {
		return err
	}
	if err := c.s.CloseSend(); err != nil {
		logrus.WithError(err).Error("failed to complete workflow")
		return err
	}
	_, _ = c.s.Recv()
	return nil
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
	s   grpccereal.Cereal_DequeueTaskClient
	ctx context.Context
}

func (c *taskCompleter) Context() context.Context {
	return c.ctx
}

func (c *taskCompleter) Fail(errMsg string) error {
	defer c.s.CloseSend() // nolint: errcheck
	err := c.s.Send(&grpccereal.DequeueTaskRequest{
		Cmd: &grpccereal.DequeueTaskRequest_Fail_{
			Fail: &grpccereal.DequeueTaskRequest_Fail{
				Error: errMsg,
			},
		},
	})
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

func (c *taskCompleter) Succeed(result []byte) error {
	defer c.s.CloseSend() // nolint: errcheck
	err := c.s.Send(&grpccereal.DequeueTaskRequest{
		Cmd: &grpccereal.DequeueTaskRequest_Succeed_{
			Succeed: &grpccereal.DequeueTaskRequest_Succeed{
				Result: result,
			},
		},
	})
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

func (g *GrpcBackend) DequeueTask(ctx context.Context, taskName string) (*backend.Task, backend.TaskCompleter, error) {
	s, err := g.client.DequeueTask(ctx)
	if err != nil {
		return nil, nil, err
	}

	if err := s.Send(&grpccereal.DequeueTaskRequest{
		Cmd: &grpccereal.DequeueTaskRequest_Dequeue_{
			Dequeue: &grpccereal.DequeueTaskRequest_Dequeue{
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
	go func() {
		for {
			msg, err := s.Recv()
			if err != nil {
				logrus.WithError(err).Debug("received error: canceling task context")
				cancel()
				return
			}
			if c := msg.GetCancel(); c != nil {
				logrus.Debug("received cancel: canceling task context")
				cancel()
				return
			}
		}

	}()
	return &backend.Task{
			Name:       deq.GetTask().GetName(),
			Parameters: deq.GetTask().GetParameters(),
		}, &taskCompleter{
			s:   s,
			ctx: taskCtx,
		}, nil
}

func (g *GrpcBackend) CreateWorkflowSchedule(ctx context.Context, instanceName string, workflowName string, parameters []byte, enabled bool, recurrence string, nextRunAt time.Time) error {
	nextRunAtProto, err := ptypes.TimestampProto(nextRunAt)
	if err != nil {
		return err
	}
	_, err = g.client.CreateWorkflowSchedule(ctx, &grpccereal.CreateWorkflowScheduleRequest{
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

	s, err := g.client.ListWorkflowSchedules(ctx, &grpccereal.ListWorkflowSchedulesRequest{})
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
		logrus.Error("missing schedule")
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
			logrus.WithError(err).Warn("could not decode NextDueAt timestamp")
		}
		schedule.NextDueAt = ts
	}

	if grpcSched.LastEnqueuedAt != nil {
		ts, err := ptypes.Timestamp(grpcSched.LastEnqueuedAt)
		if err != nil {
			logrus.WithError(err).Warn("could not decode LastEnqueuedAt timestamp")
		} else {
			schedule.LastEnqueuedAt = ts
		}
	}

	if grpcSched.LastStart != nil {
		ts, err := ptypes.Timestamp(grpcSched.LastStart)
		if err != nil {
			logrus.WithError(err).Warn("could not decode LastStart timestamp")
		} else {
			schedule.LastStart = &ts
		}
	}

	if grpcSched.LastEnd != nil {
		ts, err := ptypes.Timestamp(grpcSched.LastEnd)
		if err != nil {
			logrus.WithError(err).Warn("could not decode LastEnd timestamp")
		} else {
			schedule.LastEnd = &ts
		}
	}
	return schedule
}

func (g *GrpcBackend) UpdateWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string, opts backend.WorkflowScheduleUpdateOpts) error {
	req := grpccereal.UpdateWorkflowScheduleByNameRequest{
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
		logrus.Error("missing workflow instance")
		return nil, cereal.ErrWorkflowInstanceNotFound
	}
	backendInstance := grpcWorkflowInstanceToBackend(resp.WorkflowInstance)
	return &backendInstance, nil
}

func (g *GrpcBackend) ListWorkflowInstances(ctx context.Context, opts backend.ListWorkflowOpts) ([]*backend.WorkflowInstance, error) {
	req := grpccereal.ListWorkflowInstancesRequest{}
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
			logrus.Error("received nil workflow instance")
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
