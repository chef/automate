package grpc

import (
	"context"
	"encoding/json"
	"io"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/golang/protobuf/ptypes/wrappers"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	grpccereal "github.com/chef/automate/api/interservice/cereal"
	"github.com/chef/automate/lib/cereal"
)

var _ cereal.Driver = &GrpcBackend{}

var errUnknownMessage = errors.New("Unknown message received")

type GrpcBackend struct {
	domain string
	client grpccereal.CerealServiceClient
}

func NewGrpcBackend(domain string, client grpccereal.CerealServiceClient) *GrpcBackend {
	return &GrpcBackend{
		domain: domain,
		client: client,
	}
}
func NewGrpcBackendFromConn(domain string, conn *grpc.ClientConn) *GrpcBackend {
	return &GrpcBackend{
		domain: domain,
		client: grpccereal.NewCerealServiceClient(conn),
	}
}

func (g *GrpcBackend) DefaultTaskPollInterval() time.Duration {
	return 10 * time.Second
}

func (g *GrpcBackend) DefaultWorkflowPollInterval() time.Duration {
	return 2 * time.Second
}

func (g *GrpcBackend) EnqueueWorkflow(ctx context.Context, workflow *cereal.WorkflowInstanceData) error {
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
	s      grpccereal.CerealService_DequeueWorkflowClient
	tasks  []*grpccereal.Task
	cancel context.CancelFunc
}
type workflowCompleterChunk struct {
	s      grpccereal.CerealService_DequeueWorkflowChunkClient
	tasks  []*grpccereal.Task
	cancel context.CancelFunc
}

var _ cereal.WorkflowCompleter = &workflowCompleter{}

func (c *workflowCompleter) EnqueueTask(task *cereal.TaskData, opts cereal.TaskEnqueueOptions) error {
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
	defer c.Close() // nolint: errcheck

	if err != nil {
		return err
	}

	if err := c.s.CloseSend(); err != nil {
		logrus.WithError(err).Error("Failed to continue workflow!!!")
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
	defer c.cancel()
	return c.s.CloseSend()
}
func (c *workflowCompleterChunk) EnqueueTask(task *cereal.TaskData, opts cereal.TaskEnqueueOptions) error {
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

func (c *workflowCompleterChunk) finish(err error) error {
	defer c.Close() // nolint: errcheck

	if err != nil {
		return err
	}

	if err := c.s.CloseSend(); err != nil {
		logrus.WithError(err).Error("Failed to continue chunk workflow!")
		return err
	}

	blob, err := recieveChunk(c.s)
	if err != nil {
		logrus.WithError(err).Error("Did not get committed message in chunk workflow")
		return err
	}

	deq := &grpccereal.DequeueWorkflowResponse_Committed{}
	err = json.Unmarshal(blob, deq)
	if err != nil {
		return err
	}
	if deq == nil {
		return errUnknownMessage
	}
	return nil
}

func (c *workflowCompleterChunk) Continue(payload []byte) error {
	err := c.s.Send(&grpccereal.DequeueWorkflowChunkRequest{
		Cmd: &grpccereal.DequeueWorkflowChunkRequest_Continue_{
			Continue: &grpccereal.DequeueWorkflowChunkRequest_Continue{
				Payload: payload,
				Tasks:   c.tasks,
			},
		},
	})
	return c.finish(err)
}

func (c *workflowCompleterChunk) Fail(errMsg error) error {
	err := c.s.Send(&grpccereal.DequeueWorkflowChunkRequest{
		Cmd: &grpccereal.DequeueWorkflowChunkRequest_Fail_{
			Fail: &grpccereal.DequeueWorkflowChunkRequest_Fail{
				Err: errMsg.Error(),
			},
		},
	})
	return c.finish(err)
}

func (c *workflowCompleterChunk) Done(result []byte) error {
	err := c.s.Send(&grpccereal.DequeueWorkflowChunkRequest{
		Cmd: &grpccereal.DequeueWorkflowChunkRequest_Done_{
			Done: &grpccereal.DequeueWorkflowChunkRequest_Done{
				Result: result,
			},
		},
	})
	return c.finish(err)
}

func (c *workflowCompleterChunk) Close() error {
	defer c.cancel()
	return c.s.CloseSend()
}

func (g *GrpcBackend) DequeueWorkflow(ctx context.Context, workflowNames []string) (*cereal.WorkflowEvent, cereal.WorkflowCompleter, error) {
	ctx, cancel := context.WithCancel(ctx)
	s, err := g.client.DequeueWorkflowChunk(ctx)
	if err != nil {
		cancel()
		return nil, nil, err
	}

	if err := s.Send(&grpccereal.DequeueWorkflowChunkRequest{
		Cmd: &grpccereal.DequeueWorkflowChunkRequest_Dequeue_{
			Dequeue: &grpccereal.DequeueWorkflowChunkRequest_Dequeue{
				Domain:        g.domain,
				WorkflowNames: workflowNames,
			},
		},
	}); err != nil {
		cancel()
		return nil, nil, err
	}

	blob, err := recieveChunk(s)
	if err != nil {
		cancel()
		return nil, nil, err
	}

	deq := &grpccereal.DequeueWorkflowResponse_Dequeue{}
	err = json.Unmarshal(blob, deq)
	if err != nil {
		cancel()
		return nil, nil, err
	}

	tsProto := deq.GetEvent().GetEnqueuedAt()
	ts := time.Time{}
	if tsProto != nil {
		ts, err = ptypes.Timestamp(tsProto)
		if err != nil {
			cancel()
			return nil, nil, errors.Wrap(err, "invalid enqueued_at")
		}
	}

	var taskResult *cereal.TaskResultData
	if tr := deq.GetEvent().GetTaskResult(); tr != nil {
		taskResult = &cereal.TaskResultData{
			TaskName:   tr.GetTaskName(),
			Parameters: tr.GetParameters(),
			Status:     cereal.TaskStatusType(tr.GetStatus()),
			ErrorText:  tr.GetErrorText(),
			Result:     tr.GetResult(),
		}
	}
	backendInstance := grpcWorkflowInstanceToBackend(deq.GetInstance())
	wevt := &cereal.WorkflowEvent{
		Instance:           backendInstance,
		Type:               cereal.WorkflowEventType(deq.GetEvent().GetType()),
		EnqueuedTaskCount:  int(deq.GetEvent().GetEnqueuedTaskCount()),
		CompletedTaskCount: int(deq.GetEvent().GetCompletedTaskCount()),
		TaskResult:         taskResult,
		EnqueuedAt:         ts,
	}

	return wevt, &workflowCompleterChunk{
		s:      s,
		cancel: cancel,
	}, nil
}

func recieveChunk(s grpccereal.CerealService_DequeueWorkflowChunkClient) ([]byte, error) {
	var blob []byte
	var resp *grpccereal.DequeueWorkflowChunkResponse
	var err error
	for {
		resp, err = s.Recv()
		chunk := resp.GetChunk()
		if len(chunk) == 3 && string(chunk) == "EOF" {
			logrus.Debugln("Got EOF")
			break
		}
		if err != nil {
			if st, ok := status.FromError(err); ok {
				if st.Code() == codes.NotFound {
					return nil, cereal.ErrNoWorkflowInstances
				}
			}
			return nil, err
		}
		blob = append(blob, chunk...)
	}
	return blob, nil
}

func grpcWorkflowInstanceToBackend(grpcInstance *grpccereal.WorkflowInstance) cereal.WorkflowInstanceData {
	var err error
	if grpcInstance.Err != "" {
		err = errors.New(grpcInstance.Err)
	}
	return cereal.WorkflowInstanceData{
		InstanceName: grpcInstance.GetInstanceName(),
		WorkflowName: grpcInstance.GetWorkflowName(),
		Status:       cereal.WorkflowInstanceStatus(grpcInstance.GetStatus()),
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

func (g *GrpcBackend) KillWorkflow(ctx context.Context, instanceName string, workflowName string) error {
	_, err := g.client.KillWorkflow(ctx, &grpccereal.KillWorkflowRequest{
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
	s        grpccereal.CerealService_DequeueTaskClient
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

func (g *GrpcBackend) DequeueTask(ctx context.Context, taskName string) (*cereal.TaskData, cereal.TaskCompleter, error) {
	ctx, cancel := context.WithCancel(ctx)
	s, err := g.client.DequeueTask(ctx)
	if err != nil {
		cancel()
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
		cancel()
		return nil, nil, err
	}

	resp, err := s.Recv()
	if err != nil {
		cancel()
		if st, ok := status.FromError(err); ok {
			if st.Code() == codes.NotFound {
				return nil, nil, cereal.ErrNoTasks
			}
		}
		return nil, nil, err
	}

	deq := resp.GetDequeue()
	if deq == nil {
		cancel()
		return nil, nil, errors.New("invalid msg")
	}

	tsProto := deq.GetTask().GetMetadata().GetEnqueuedAt()

	ts := time.Time{}
	if tsProto != nil {
		ts, err = ptypes.Timestamp(tsProto)
		if err != nil {
			cancel()
			return nil, nil, errors.Wrap(err, "invalid enqueued_at")
		}
	}

	doneChan := make(chan error, 1)
	go func() {
		// This goroutine will read the next, and what must be the last
		// message.
		var errOut error
		msg, err := s.Recv()
		if err != nil {
			logrus.WithError(err).Debug("Received error while waiting for committed message")
			errOut = err
		} else if c := msg.GetCancel(); c != nil {
			logrus.Debug("Received cancel while waiting for committed message")
			errOut = context.Canceled
		} else if c := msg.GetCommitted(); c != nil {
			logrus.Debug("Received committed")
			errOut = nil
		} else {
			errOut = errUnknownMessage
		}
		cancel()
		doneChan <- errOut
	}()

	return &cereal.TaskData{
			Name:       deq.GetTask().GetName(),
			Parameters: deq.GetTask().GetParameters(),
			Metadata: cereal.TaskMetadata{
				EnqueuedAt: ts,
			},
		}, &taskCompleter{
			s:        s,
			ctx:      ctx,
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

func (g *GrpcBackend) ListWorkflowSchedules(ctx context.Context) ([]*cereal.Schedule, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	s, err := g.client.ListWorkflowSchedules(ctx, &grpccereal.ListWorkflowSchedulesRequest{
		Domain: g.domain,
	})
	if err != nil {
		return nil, err
	}
	var schedules []*cereal.Schedule

	for {
		resp, err := s.Recv()
		if err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}
		if resp.NumSchedules > 0 && schedules == nil {
			schedules = make([]*cereal.Schedule, 0, resp.NumSchedules)
		}
		if resp.Schedule != nil {
			schedules = append(schedules, grpcSchedToBackend(resp.Schedule))
		}
	}
	return schedules, nil
}

func (g *GrpcBackend) GetWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string) (*cereal.Schedule, error) {
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

func grpcSchedToBackend(grpcSched *grpccereal.Schedule) *cereal.Schedule {
	schedule := &cereal.Schedule{
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

func (g *GrpcBackend) UpdateWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string, opts cereal.WorkflowScheduleUpdateOptions) error {
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

func (g *GrpcBackend) GetWorkflowInstanceByName(ctx context.Context, instanceName string, workflowName string) (*cereal.WorkflowInstanceData, error) {
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

func (g *GrpcBackend) ListWorkflowInstances(ctx context.Context, opts cereal.ListWorkflowOpts) ([]*cereal.WorkflowInstanceData, error) {
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

	instances := []*cereal.WorkflowInstanceData{}
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
