package server

import (
	"context"
	"errors"
	"time"

	"github.com/golang/protobuf/ptypes"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/sirupsen/logrus"
	"github.com/teambition/rrule-go"

	"github.com/chef/automate/api/interservice/cereal"
	libcereal "github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/backend"
)

type CerealService struct {
	workflowScheduler *libcereal.WorkflowScheduler
	backend           backend.Driver
}

func NewCerealService(ctx context.Context, b backend.Driver) *CerealService {
	cs := &CerealService{
		backend: b,
	}

	if v, ok := b.(backend.SchedulerDriver); ok {
		cs.workflowScheduler = libcereal.NewWorkflowScheduler(v)
		cs.workflowScheduler.Run(ctx)
	}

	return cs
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
	grpcInstance := cerealWorkflowInstanceToGrpc(&evt.Instance)
	err = writeDeqWorkRespMsg(ctx, req, &cereal.DequeueWorkflowResponse{
		Cmd: &cereal.DequeueWorkflowResponse_Dequeue_{
			Dequeue: &cereal.DequeueWorkflowResponse_Dequeue{
				Instance: grpcInstance,
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

func (s *CerealService) CreateWorkflowSchedule(ctx context.Context, req *cereal.CreateWorkflowScheduleRequest) (*cereal.CreateWorkflowScheduleResponse, error) {
	_, err := rrule.StrToRRule(req.GetRecurrence())
	if err != nil {
		logrus.WithError(err).Error("recurrence rule not valid")
		return nil, status.Error(codes.InvalidArgument, "recurrence rule not valid")
	}
	nextRun, err := ptypes.Timestamp(req.NextRunAt)
	if err != nil {
		logrus.WithError(err).Error("nextRun not valid")
		return nil, status.Error(codes.InvalidArgument, "nextRun not valid")
	}
	err = s.backend.CreateWorkflowSchedule(ctx, req.InstanceName, req.WorkflowName, req.Parameters, req.Enabled, req.Recurrence, nextRun)
	if err != nil {
		if err == libcereal.ErrWorkflowScheduleExists {
			return nil, status.Error(codes.FailedPrecondition, err.Error())
		}
		return nil, err
	}
	return &cereal.CreateWorkflowScheduleResponse{}, nil
}

func (s *CerealService) ListWorkflowSchedules(ctx context.Context, _ *cereal.ListWorkflowSchedulesRequest) (*cereal.ListWorkflowSchedulesResponse, error) {
	schedules, err := s.backend.ListWorkflowSchedules(ctx)
	if err != nil {
		return nil, err
	}

	if len(schedules) > 0 {
		grpcSchedules := make([]*cereal.Schedule, len(schedules))
		for i, schedule := range schedules {
			grpcSchedules[i] = cerealScheduleToGrpcSchedule(schedule)
		}
		return &cereal.ListWorkflowSchedulesResponse{
			Schedules: grpcSchedules,
		}, nil
	}
	return &cereal.ListWorkflowSchedulesResponse{}, nil
}

func (s *CerealService) GetWorkflowScheduleByName(ctx context.Context, req *cereal.GetWorkflowScheduleByNameRequest) (*cereal.GetWorkflowScheduleByNameResponse, error) {
	schedule, err := s.backend.GetWorkflowScheduleByName(ctx, req.InstanceName, req.WorkflowName)
	if err != nil {
		if err == libcereal.ErrWorkflowScheduleNotFound {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, err
	}
	return &cereal.GetWorkflowScheduleByNameResponse{
		Schedule: cerealScheduleToGrpcSchedule(schedule),
	}, nil
}

func cerealScheduleToGrpcSchedule(schedule *backend.Schedule) *cereal.Schedule {
	grpcSchedule := &cereal.Schedule{
		InstanceName: schedule.InstanceName,
		WorkflowName: schedule.WorkflowName,
		Parameters:   schedule.Parameters,
		Recurrence:   schedule.Recurrence,
	}
	var err error
	grpcSchedule.NextDueAt, err = ptypes.TimestampProto(schedule.NextDueAt)
	if err != nil {
		logrus.WithError(err).Warn("invalid timestamp")
	}
	if schedule.LastStart != nil {
		grpcSchedule.LastEnd, err = ptypes.TimestampProto(*schedule.LastStart)
		logrus.WithError(err).Warn("invalid timestamp")
	}
	if schedule.LastEnd != nil {
		grpcSchedule.LastEnd, err = ptypes.TimestampProto(*schedule.LastEnd)
		logrus.WithError(err).Warn("invalid timestamp")

	}
	return grpcSchedule
}

func (s *CerealService) UpdateWorkflowScheduleByName(ctx context.Context, req *cereal.UpdateWorkflowScheduleByNameRequest) (*cereal.UpdateWorkflowScheduleByNameResponse, error) {
	opts := backend.WorkflowScheduleUpdateOpts{}

	if req.GetEnabled() != nil {
		opts.UpdateEnabled = true
		opts.Enabled = req.GetEnabled().GetValue()
	}

	if req.GetParameters() != nil {
		opts.UpdateParameters = true
		opts.Parameters = req.GetParameters().GetValue()
	}

	if req.GetRecurrence() != nil {
		opts.UpdateRecurrence = true
		_, err := rrule.StrToRRule(req.GetRecurrence().GetValue())
		if err != nil {
			logrus.WithError(err).Error("recurrence rule not valid")
			return nil, status.Error(codes.InvalidArgument, "recurrence rule not valid")
		}
		if req.GetNextRunAt() == nil {
			return nil, status.Error(codes.InvalidArgument, "expected next run at with recurrence update")
		}
		ts, err := ptypes.Timestamp(req.GetNextRunAt())
		if err != nil {
			return nil, status.Error(codes.InvalidArgument, "invalid timestamp")
		}
		opts.NextRunAt = ts
	}

	err := s.backend.UpdateWorkflowScheduleByName(ctx, req.InstanceName, req.WorkflowName, opts)
	if err != nil {
		if err == libcereal.ErrWorkflowScheduleNotFound {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, err
	}
	return &cereal.UpdateWorkflowScheduleByNameResponse{}, nil
}

func (s *CerealService) GetWorkflowInstanceByName(ctx context.Context, req *cereal.GetWorkflowInstanceByNameRequest) (*cereal.GetWorkflowInstanceByNameResponse, error) {
	workflowInstance, err := s.backend.GetWorkflowInstanceByName(ctx, req.GetInstanceName(), req.GetWorkflowName())
	if err != nil {
		if err == libcereal.ErrWorkflowInstanceNotFound {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, err
	}
	grpcInstance := cerealWorkflowInstanceToGrpc(workflowInstance)

	return &cereal.GetWorkflowInstanceByNameResponse{
		WorkflowInstance: grpcInstance,
	}, nil
}

func cerealWorkflowInstanceToGrpc(workflowInstance *backend.WorkflowInstance) *cereal.WorkflowInstance {
	grpcInstance := &cereal.WorkflowInstance{
		InstanceName: workflowInstance.InstanceName,
		WorkflowName: workflowInstance.WorkflowName,
		Status:       string(workflowInstance.Status),
		Parameters:   workflowInstance.Parameters,
		Payload:      workflowInstance.Payload,
		Result:       workflowInstance.Result,
	}
	if workflowInstance.Err != nil {
		grpcInstance.Err = workflowInstance.Err.Error()
	}

	return grpcInstance
}
