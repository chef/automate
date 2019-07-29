package grpc

import (
	"context"
	"errors"
	"time"

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
	c.tasks = append(c.tasks, &grpccereal.Task{
		Name:       task.Name,
		Parameters: task.Parameters,
	})
	return nil
}

func (c *workflowCompleter) Continue(payload []byte) error {
	defer c.s.CloseSend()
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
	c.s.CloseSend()
	_, _ = c.s.Recv()
	return nil
}

func (c *workflowCompleter) Fail(errMsg error) error {
	defer c.s.CloseSend()
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
	c.s.CloseSend()
	_, _ = c.s.Recv()
	return nil
}

func (c *workflowCompleter) Done(result []byte) error {
	defer c.s.CloseSend()
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
	c.s.CloseSend()
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
			ErrorText:  tr.GetErrorText(),
			Result:     tr.GetResult(),
		}
	}
	wevt := &backend.WorkflowEvent{
		Instance: backend.WorkflowInstance{
			InstanceName: deq.GetInstance().GetInstanceName(),
			WorkflowName: deq.GetInstance().GetWorkflowName(),
			Status:       backend.WorkflowInstanceStatus(deq.GetInstance().GetStatus()),
			Parameters:   deq.GetInstance().GetParameters(),
			Payload:      deq.GetInstance().GetPayload(),
		},
		Type:               backend.WorkflowEventType(deq.GetEvent().GetType()),
		EnqueuedTaskCount:  int(deq.GetEvent().GetEnqueuedTaskCount()),
		CompletedTaskCount: int(deq.GetEvent().GetCompletedTaskCount()),
		TaskResult:         taskResult,
	}

	return wevt, &workflowCompleter{
		s: s,
	}, nil
}

func (g *GrpcBackend) CancelWorkflow(ctx context.Context, instanceName string, workflowName string) error {
	return nil
}

type taskCompleter struct {
	s      grpccereal.Cereal_DequeueTaskClient
	ctx    context.Context
	cancel context.CancelFunc
}

func (c *taskCompleter) Context() context.Context {
	return c.ctx
}

func (c *taskCompleter) Fail(errMsg string) error {
	defer c.s.CloseSend()
	logrus.Info("Failing")
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
	defer c.s.CloseSend()
	logrus.Info("Succeeding")
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
	ctx, cancel := context.WithCancel(ctx)
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

	logrus.Info("Dequeued task")
	go func() {
		for {
			msg, err := s.Recv()
			if err != nil {
				logrus.WithError(err).Info("DOING THE CANCEL")
				cancel()
				return
			}
			if c := msg.GetCancel(); c != nil {
				logrus.Info("DOING THE CANCEL")
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
			ctx: ctx,
		}, nil
}

func (*GrpcBackend) CreateWorkflowSchedule(ctx context.Context, instanceName string, workflowName string, parameters []byte, enabled bool, recurrence string, nextRunAt time.Time) error {
	return nil
}

func (*GrpcBackend) ListWorkflowSchedules(ctx context.Context) ([]*backend.Schedule, error) {
	return nil, nil
}
func (*GrpcBackend) GetWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string) (*backend.Schedule, error) {
	return nil, nil
}
func (*GrpcBackend) UpdateWorkflowScheduleByName(ctx context.Context, instanceName string, workflowName string, opts backend.WorkflowScheduleUpdateOpts) error {
	return nil
}

func (*GrpcBackend) GetWorkflowInstanceByName(ctx context.Context, instanceName string, workflowName string) (*backend.WorkflowInstance, error) {
	return nil, nil
}

func (*GrpcBackend) Init() error {
	return nil
}

func (*GrpcBackend) Close() error {
	return nil
}
