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

func readDeqWorkReqMsg(ctx context.Context, s cereal.Cereal_DequeueWorkflowServer) (*cereal.DequeueWorkflowRequest, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	type chanMsg struct {
		Msg *cereal.DequeueWorkflowRequest
		Err error
	}

	in := make(chan chanMsg)
	go func() {
		defer close(in)
		// s.Recv will return once it receives a message or its underlying
		// context is canceled
		msg, err := s.Recv()
		select {
		case <-ctx.Done():
		case in <- chanMsg{Msg: msg, Err: err}:
		}
	}()

	select {
	case <-ctx.Done():
		return nil, ctx.Err()
	case msg, ok := <-in:
		if !ok {
			return nil, errors.New("message not received")
		}
		return msg.Msg, msg.Err
	}
}

func writeDeqWorkRespMsg(ctx context.Context, s cereal.Cereal_DequeueWorkflowServer, msg *cereal.DequeueWorkflowResponse) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	out := make(chan error)
	go func() {
		defer close(out)
		// s.Send will return once it sends the message or its underlying context
		// is closed
		err := s.Send(msg)
		select {
		case <-ctx.Done():
		case out <- err:
		}
	}()

	select {
	case <-ctx.Done():
		return ctx.Err()
	case err := <-out:
		return err
	}
}

func (s *CerealService) DequeueWorkflow(req cereal.Cereal_DequeueWorkflowServer) error {
	ctx, cancel := context.WithTimeout(req.Context(), time.Minute)
	defer cancel()

	// read dequeue message
	var deqMsg *cereal.DequeueWorkflowRequest_Dequeue
	msg, err := readDeqWorkReqMsg(ctx, req)
	if err != nil {
		return err
	}
	deqMsg = msg.GetDequeue()
	if deqMsg == nil {
		return errors.New("invalid msg")
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
	err = writeDeqWorkRespMsg(ctx, req, &cereal.DequeueWorkflowResponse{
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
	})
	if err != nil {
		return err
	}

	// read workflow completion message
	msg, err = readDeqWorkReqMsg(ctx, req)
	if err != nil {
		return err
	}
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
	} else {
		return errors.New("invalid msg")
	}

	return nil
}

func readDeqTaskReqMsg(ctx context.Context, s cereal.Cereal_DequeueTaskServer) (*cereal.DequeueTaskRequest, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	type chanMsg struct {
		Msg *cereal.DequeueTaskRequest
		Err error
	}

	in := make(chan chanMsg)
	go func() {
		defer close(in)
		// s.Recv will return once it receives a message or its underlying
		// context is canceled
		msg, err := s.Recv()
		select {
		case <-ctx.Done():
		case in <- chanMsg{Msg: msg, Err: err}:
		}
	}()

	select {
	case <-ctx.Done():
		return nil, ctx.Err()
	case msg, ok := <-in:
		if !ok {
			return nil, errors.New("message not received")
		}
		return msg.Msg, msg.Err
	}
}

func writeDeqTaskRespMsg(ctx context.Context, s cereal.Cereal_DequeueTaskServer, msg *cereal.DequeueTaskResponse) error {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	out := make(chan error)
	go func() {
		defer close(out)
		// s.Send will return once it sends the message or its underlying context
		// is closed
		err := s.Send(msg)
		select {
		case <-ctx.Done():
		case out <- err:
		}
	}()

	select {
	case <-ctx.Done():
		return ctx.Err()
	case err := <-out:
		return err
	}
}

func (s *CerealService) DequeueTask(req cereal.Cereal_DequeueTaskServer) error {
	ctx, cancel := context.WithCancel(req.Context())
	defer cancel()

	msg, err := readDeqTaskReqMsg(ctx, req)
	if err != nil {
		return err
	}
	deqMsg := msg.GetDequeue()
	if deqMsg == nil {
		return errors.New("invalid msg")
	}

	task, completer, err := s.backend.DequeueTask(ctx, deqMsg.TaskName)
	if err != nil {
		if err == libcereal.ErrNoTasks {
			return status.Error(codes.NotFound, err.Error())
		}
		return err
	}

	err = writeDeqTaskRespMsg(ctx, req, &cereal.DequeueTaskResponse{
		Cmd: &cereal.DequeueTaskResponse_Dequeue_{
			Dequeue: &cereal.DequeueTaskResponse_Dequeue{
				Task: &cereal.Task{
					Name:       task.Name,
					Parameters: task.Parameters,
				},
			},
		},
	})
	if err != nil {
		return err
	}

	logrus.Info("waiting for task complete")
	msg, err = readDeqTaskReqMsg(completer.Context(), req)
	if err != nil {
		if completer.Context().Err() != nil {
			logrus.WithError(completer.Context().Err()).Info("Trying cancel")
			err = writeDeqTaskRespMsg(ctx, req, &cereal.DequeueTaskResponse{
				Cmd: &cereal.DequeueTaskResponse_Cancel_{
					Cancel: &cereal.DequeueTaskResponse_Cancel{},
				},
			})
			if err != nil {
				return err
			}
			return completer.Context().Err()
		}
		return err
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

	return nil
}
