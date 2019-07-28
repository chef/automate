package server

import (
	"context"
	"errors"
	"time"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/interservice/cereal"
	libcereal "github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/backend"
)

type CerealService struct {
	backend backend.Driver
}

func NewCerealService(b backend.Driver) *CerealService {
	return &CerealService{
		backend: b,
	}
}

func (s *CerealService) EnqueueWorkflow(ctx context.Context, req *cereal.EnqueueWorkflowRequest) (*cereal.EnqueueWorkflowResponse, error) {
	if err := s.backend.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: req.InstanceName,
		WorkflowName: req.WorkflowName,
		Parameters:   req.Parameters,
	}); err != nil {
		if err == libcereal.ErrWorkflowInstanceExists {
			return nil, status.Error(codes.FailedPrecondition, err.Error())
		}
	}
	return &cereal.EnqueueWorkflowResponse{}, nil
}

func (s *CerealService) DequeueWorkflow(req cereal.Cereal_DequeueWorkflowServer) error {
	ctx, cancel := context.WithTimeout(req.Context(), time.Minute)
	defer cancel()

	in := make(chan *cereal.DequeueWorkflowRequest)
	out := make(chan *cereal.DequeueWorkflowResponse)

	// receiver
	go func() {
		defer close(in)
		for {
			select {
			case <-ctx.Done():
				return
			default:
			}
			msg, err := req.Recv()
			if err != nil {
				return
			}
			select {
			case <-ctx.Done():
				return
			case in <- msg:
			}
		}
	}()

	// sender
	go func() {
		for {
			select {
			case <-ctx.Done():
				return
			case msg := <-out:
				if err := req.Send(msg); err != nil {
					logrus.WithError(err).Error("failed to send msg")
					cancel()
					return
				}
			}
		}
	}()

	var deqMsg *cereal.DequeueWorkflowRequest_Dequeue
	select {
	case <-ctx.Done():
		return ctx.Err()
	case msg, ok := <-in:
		if !ok {
			return errors.New("problem")
		}
		deqMsg = msg.GetDequeue()
		if deqMsg == nil {
			return errors.New("invalid msg")
		}
	}

	evt, completer, err := s.backend.DequeueWorkflow(ctx, deqMsg.GetWorkflowNames())
	if err != nil {
		if err == libcereal.ErrNoWorkflowInstances {
			return status.Error(codes.NotFound, err.Error())
		}
		return err
	}
	defer completer.Close() // nolint: errcheck

	logrus.Info("Dequeued workflow")

	var taskResult *cereal.TaskResult
	if evt.TaskResult != nil {
		taskResult = &cereal.TaskResult{
			TaskName:   evt.TaskResult.TaskName,
			Parameters: evt.TaskResult.Parameters,
			Status:     string(evt.TaskResult.Status),
			ErrorText:  evt.TaskResult.ErrorText,
			Result:     evt.TaskResult.Result,
		}
	}
	select {
	case <-ctx.Done():
		return ctx.Err()
	case out <- &cereal.DequeueWorkflowResponse{
		Cmd: &cereal.DequeueWorkflowResponse_Dequeue_{
			Dequeue: &cereal.DequeueWorkflowResponse_Dequeue{
				Instance: &cereal.WorkflowInstance{
					InstanceName: evt.Instance.InstanceName,
					WorkflowName: evt.Instance.WorkflowName,
					Status:       string(evt.Instance.Status),
					Parameters:   evt.Instance.Parameters,
					Payload:      evt.Instance.Payload,
				},
				Event: &cereal.WorkflowEvent{
					Type:               string(evt.Type),
					EnqueuedTaskCount:  int64(evt.EnqueuedTaskCount),
					CompletedTaskCount: int64(evt.CompletedTaskCount),
					TaskResult:         taskResult,
				},
			},
		},
	}:
	}

	select {
	case <-ctx.Done():
		return ctx.Err()
	case msg, ok := <-in:
		if !ok {
			logrus.Error("problem")
			return errors.New("problem")
		}
		logrus.Info("got msg")
		if done := msg.GetDone(); done != nil {
			if err := completer.Done(done.GetResult()); err != nil {
				return err
			}
		} else if cont := msg.GetContinue(); cont != nil {
			for _, task := range cont.GetTasks() {
				opts := backend.TaskEnqueueOpts{}
				opts.StartAfter = time.Time{}
				err := completer.EnqueueTask(&backend.Task{
					Name:       task.Name,
					Parameters: task.Parameters,
				}, opts)

				if err != nil {
					return err
				}
			}

			if err := completer.Continue(cont.GetPayload()); err != nil {
				return err
			}
		} else if fail := msg.GetFail(); fail != nil {
			if err := completer.Fail(errors.New(fail.Err)); err != nil {
				return err
			}
		}
	}

	return nil
}

func (s *CerealService) DequeueTask(req cereal.Cereal_DequeueTaskServer) error {
	ctx, cancel := context.WithCancel(req.Context())
	defer cancel()
	in := make(chan *cereal.DequeueTaskRequest)
	out := make(chan *cereal.DequeueTaskResponse)

	// receiver
	go func() {
		defer close(in)
		for {
			select {
			case <-ctx.Done():
				return
			default:
			}
			msg, err := req.Recv()
			if err != nil {
				return
			}
			in <- msg
		}
	}()

	// sender
	go func() {
		for {
			select {
			case <-ctx.Done():
				return
			case msg := <-out:
				if err := req.Send(msg); err != nil {
					logrus.WithError(err).Error("failed to send msg")
					cancel()
					return
				}
			}
		}
	}()

	var deqMsg *cereal.DequeueTaskRequest_Dequeue
	select {
	case <-ctx.Done():
		return ctx.Err()
	case msg, ok := <-in:
		if !ok {
			return errors.New("problem")
		}
		deqMsg = msg.GetDequeue()
		if deqMsg == nil {
			return errors.New("invalid msg")
		}
	}

	task, completer, err := s.backend.DequeueTask(ctx, deqMsg.TaskName)
	if err != nil {
		if err == libcereal.ErrNoTasks {
			return status.Error(codes.NotFound, err.Error())
		}
		return err
	}

	select {
	case <-ctx.Done():
		return ctx.Err()
	case <-completer.Context().Done():
		select {
		case <-ctx.Done():
			return ctx.Err()
		case out <- &cereal.DequeueTaskResponse{
			Cmd: &cereal.DequeueTaskResponse_Cancel_{
				Cancel: &cereal.DequeueTaskResponse_Cancel{},
			},
		}:
			return completer.Context().Err()
		}

	case out <- &cereal.DequeueTaskResponse{
		Cmd: &cereal.DequeueTaskResponse_Dequeue_{
			Dequeue: &cereal.DequeueTaskResponse_Dequeue{
				Task: &cereal.Task{
					Name:       task.Name,
					Parameters: task.Parameters,
				},
			},
		},
	}:
	}

	logrus.Info("waiting for task complete")
	select {
	case <-ctx.Done():
		return ctx.Err()
	case <-completer.Context().Done():
		logrus.WithError(completer.Context().Err()).Info("Trying cancel")
		select {
		case <-ctx.Done():
			logrus.WithError(ctx.Err()).Info("context is done")
			return ctx.Err()
		case out <- &cereal.DequeueTaskResponse{
			Cmd: &cereal.DequeueTaskResponse_Cancel_{
				Cancel: &cereal.DequeueTaskResponse_Cancel{},
			},
		}:
			logrus.Info("Sending cancel")
			<-ctx.Done()
			return completer.Context().Err()
		}

	case msg, ok := <-in:
		if !ok {
			return errors.New("problem")
		}
		if fail := msg.GetFail(); fail != nil {
			if err := completer.Fail(fail.GetError()); err != nil {
				if err == libcereal.ErrTaskLost {
					return status.Error(codes.FailedPrecondition, err.Error())
				}
				return err
			}
		} else if succeed := msg.GetSucceed(); succeed != nil {
			if err := completer.Succeed(succeed.GetResult()); err != nil {
				if err == libcereal.ErrTaskLost {
					return status.Error(codes.FailedPrecondition, err.Error())
				}
				return err
			}
		} else {
			return errors.New("invalid msg")
		}
	}

	return nil
}
