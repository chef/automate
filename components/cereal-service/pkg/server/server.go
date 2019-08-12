package server

import (
	"context"
	"errors"
	"fmt"
	"regexp"
	"strings"
	"time"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/sirupsen/logrus"
	"github.com/teambition/rrule-go"

	"github.com/chef/automate/api/interservice/cereal"
	libcereal "github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/backend"
)

var errInvalidMsg = errors.New("invalid msg")
var domainRegex = regexp.MustCompile("^[a-zA-Z0-9_\\-\\.]+$")

type CerealService struct {
	workflowScheduler *libcereal.WorkflowScheduler
	backend           backend.Driver
}

func NewCerealService(ctx context.Context, b backend.Driver) *CerealService {
	cs := &CerealService{
		backend: b,
	}

	if v, ok := b.(backend.SchedulerDriver); ok {
		cs.workflowScheduler = libcereal.NewWorkflowScheduler(v, func() {})
		go cs.workflowScheduler.Run(ctx)
	}

	return cs
}

func generateRequestID() string {
	id, err := uuid.NewV4()
	if err != nil {
		return "unknown"
	}
	return id.String()
}

func namespace(domain string, name string) string {
	return fmt.Sprintf("%s/%s", domain, name)
}

func unnamespace(domainAndName string) (domain string, name string) {
	splits := strings.SplitN(domainAndName, "/", 2)
	if len(splits) != 2 {
		return "", domainAndName
	} else {
		return splits[0], splits[1]
	}
}

func validateDomain(domain string) error {
	if domain == "" {
		return status.Error(codes.InvalidArgument, "must specify domain")
	}
	if !domainRegex.MatchString(domain) {
		return status.Errorf(codes.InvalidArgument, "you specified an invalid domain. must match %q", domainRegex.String())
	}

	return nil
}

func (s *CerealService) EnqueueWorkflow(ctx context.Context, req *cereal.EnqueueWorkflowRequest) (*cereal.EnqueueWorkflowResponse, error) {
	logctx := logrus.WithFields(logrus.Fields{
		"id":            generateRequestID(),
		"method":        "EnqueueWorkflow",
		"domain":        req.Domain,
		"workflow_name": req.WorkflowName,
		"instance_name": req.InstanceName,
	})
	if err := validateDomain(req.Domain); err != nil {
		return nil, err
	}
	logctx.Info("enqueuing workflow")
	if err := s.backend.EnqueueWorkflow(ctx, &backend.WorkflowInstance{
		InstanceName: req.InstanceName,
		WorkflowName: namespace(req.Domain, req.WorkflowName),
		Parameters:   req.Parameters,
	}); err != nil {
		logctx.WithError(err).Error("failed to enqueue workflow")
		if err == libcereal.ErrWorkflowInstanceExists {
			return nil, status.Error(codes.FailedPrecondition, err.Error())
		}
		return nil, err
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

	logctx := logrus.WithFields(logrus.Fields{
		"id":     generateRequestID(),
		"method": "DequeueWorkflow",
	})
	// read dequeue message
	var deqMsg *cereal.DequeueWorkflowRequest_Dequeue
	msg, err := readDeqWorkReqMsg(ctx, req)
	if err != nil {
		return err
	}
	deqMsg = msg.GetDequeue()
	if deqMsg == nil {
		logctx.WithError(errInvalidMsg).Error("failed to get dequeue msg")
		return errInvalidMsg
	}
	logctx.WithFields(
		logrus.Fields{
			"workflow_names": deqMsg.WorkflowNames,
			"domain":         deqMsg.Domain,
		}).Debug("got dequeue msg")

	if err := validateDomain(deqMsg.Domain); err != nil {
		return err
	}

	workflowNames := make([]string, len(deqMsg.WorkflowNames))
	for _, workflowName := range deqMsg.WorkflowNames {
		workflowNames = append(workflowNames, namespace(deqMsg.Domain, workflowName))
	}
	evt, completer, err := s.backend.DequeueWorkflow(ctx, workflowNames)
	if err != nil {
		if err == libcereal.ErrNoWorkflowInstances {
			return status.Error(codes.NotFound, err.Error())
		}
		return err
	}
	defer completer.Close() // nolint: errcheck

	domain, workflowName := unnamespace(evt.Instance.WorkflowName)
	evt.Instance.WorkflowName = workflowName
	logctx = logctx.WithFields(logrus.Fields{
		"domain_recv":     domain,
		"workflow_name":   evt.Instance.WorkflowName,
		"instance_name":   evt.Instance.InstanceName,
		"workflow_status": evt.Instance.Status,
		"event_type":      evt.Type,
	})
	logctx.Info("workflow dequeued")

	var taskResult *cereal.TaskResult
	if evt.TaskResult != nil {
		_, evt.TaskResult.TaskName = unnamespace(evt.TaskResult.TaskName)
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
		logctx.WithError(err).Error("failed to respond with workflow event")
		return err
	}

	// read workflow completion message
	msg, err = readDeqWorkReqMsg(ctx, req)
	if err != nil {
		logctx.WithError(err).Error("failed to read workflow event completion response")
		return err
	}
	if done := msg.GetDone(); done != nil {
		logctx.Info("completing workflow instance")
		if err := completer.Done(done.GetResult()); err != nil {
			return err
		}
	} else if cont := msg.GetContinue(); cont != nil {
		for _, task := range cont.GetTasks() {
			opts := backend.TaskEnqueueOpts{}
			opts.StartAfter = time.Time{}
			if task.StartAfter != nil {
				ts, err := ptypes.Timestamp(task.StartAfter)
				if err != nil {
					logctx.WithError(err).Error("invalid timestamp")
					return err
				} else {
					opts.StartAfter = ts
				}
			}

			err := completer.EnqueueTask(&backend.Task{
				Name:       namespace(deqMsg.Domain, task.Name),
				Parameters: task.Parameters,
			}, opts)

			if err != nil {
				return err
			}
		}
		logctx.WithField("enqueued_tasks", len(cont.GetTasks())).Info("continuing workflow instance")
		if err := completer.Continue(cont.GetPayload()); err != nil {
			return err
		}
	} else if fail := msg.GetFail(); fail != nil {
		logctx.Info("failing workflow instance")
		if err := completer.Fail(errors.New(fail.Err)); err != nil {
			return err
		}
	} else {
		logctx.WithError(errInvalidMsg).Error("failed to process workflow instance")
		return errInvalidMsg
	}

	return nil
}

func (s *CerealService) CancelWorkflow(ctx context.Context, req *cereal.CancelWorkflowRequest) (*cereal.CancelWorkflowResponse, error) {
	logctx := logrus.WithFields(logrus.Fields{
		"id":            generateRequestID(),
		"method":        "CancelWorkflow",
		"domain":        req.Domain,
		"workflow_name": req.WorkflowName,
		"instance_name": req.InstanceName,
	})
	if err := validateDomain(req.Domain); err != nil {
		return nil, err
	}
	logctx.Info("canceling workflow")
	err := s.backend.CancelWorkflow(ctx, req.InstanceName, namespace(req.Domain, req.WorkflowName))
	if err != nil {
		logctx.WithError(err).Error("failed to cancel workflow")
		if err == libcereal.ErrWorkflowInstanceNotFound {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, err
	}
	return &cereal.CancelWorkflowResponse{}, nil
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

	logctx := logrus.WithFields(logrus.Fields{
		"id":     generateRequestID(),
		"method": "DequeueTask",
	})

	msg, err := readDeqTaskReqMsg(ctx, req)
	if err != nil {
		logctx.WithError(err).Error("failed to get dequeue message")
		return err
	}
	deqMsg := msg.GetDequeue()
	if deqMsg == nil {
		err := errors.New("invalid msg")
		logctx.WithError(err).Error("failed to get dequeue message")
		return err
	}
	if err := validateDomain(deqMsg.Domain); err != nil {
		return err
	}

	logctx = logctx.WithFields(logrus.Fields{
		"domain":    deqMsg.Domain,
		"task_name": deqMsg.TaskName,
	})

	logctx.Debug("dequeuing task")
	task, completer, err := s.backend.DequeueTask(ctx, namespace(deqMsg.Domain, deqMsg.TaskName))
	if err != nil {
		if err == libcereal.ErrNoTasks {
			logctx.WithError(err).Debug("failed to dequeue task")
			return status.Error(codes.NotFound, err.Error())
		}
		logctx.WithError(err).Error("failed to dequeue task")
		return err
	}

	logctx.Info("dequeued task")
	_, task.Name = unnamespace(task.Name)
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
		logctx.WithError(err).Error("failed to send dequeued task")
		return err
	}

	logctx.Debug("waiting for task complete")
	msg, err = readDeqTaskReqMsg(completer.Context(), req)
	if err != nil {
		logctx.WithError(err).Error("failed to get task completion message")
		if completer.Context().Err() != nil {
			logctx.WithError(completer.Context().Err()).Info("sending task cancellation message")
			err = writeDeqTaskRespMsg(ctx, req, &cereal.DequeueTaskResponse{
				Cmd: &cereal.DequeueTaskResponse_Cancel_{
					Cancel: &cereal.DequeueTaskResponse_Cancel{},
				},
			})
			if err != nil {
				logctx.WithError(err).Error("failed to send task cancellation message")
				return err
			}
			return completer.Context().Err()
		}
		return err
	}

	if fail := msg.GetFail(); fail != nil {
		logctx.Info("failing task")
		if err := completer.Fail(fail.GetError()); err != nil {
			logctx.WithError(err).Error("failed to fail task")
			if err == libcereal.ErrTaskLost {
				return status.Error(codes.FailedPrecondition, err.Error())
			}
			return err
		}
	} else if succeed := msg.GetSucceed(); succeed != nil {
		logctx.Info("succeeding task")
		if err := completer.Succeed(succeed.GetResult()); err != nil {
			logrus.WithError(err).Error("failed to succeed task")
			if err == libcereal.ErrTaskLost {
				return status.Error(codes.FailedPrecondition, err.Error())
			}
			return err
		}
	} else {
		logctx.Error("invalid message")
		return errInvalidMsg
	}

	err = writeDeqTaskRespMsg(ctx, req, &cereal.DequeueTaskResponse{
		Cmd: &cereal.DequeueTaskResponse_Committed_{
			Committed: &cereal.DequeueTaskResponse_Committed{},
		},
	})
	return err
}

func (s *CerealService) CreateWorkflowSchedule(ctx context.Context, req *cereal.CreateWorkflowScheduleRequest) (*cereal.CreateWorkflowScheduleResponse, error) {
	logctx := logrus.WithFields(logrus.Fields{
		"id":            generateRequestID(),
		"method":        "CreateWorkflowSchedule",
		"domain":        req.Domain,
		"workflow_name": req.WorkflowName,
		"instance_name": req.InstanceName,
		"recurrence":    req.Recurrence,
		"next_run_at":   req.NextRunAt,
		"enabled":       req.Enabled,
	})

	if err := validateDomain(req.Domain); err != nil {
		return nil, err
	}

	_, err := rrule.StrToRRule(req.GetRecurrence())
	if err != nil {
		logctx.WithError(err).Error("recurrence rule not valid")
		return nil, status.Error(codes.InvalidArgument, "recurrence rule not valid")
	}
	nextRun, err := ptypes.Timestamp(req.NextRunAt)
	if err != nil {
		logctx.WithError(err).Error("nextRun not valid")
		return nil, status.Error(codes.InvalidArgument, "nextRun not valid")
	}
	logctx.Info("creating workflow schedule")
	err = s.backend.CreateWorkflowSchedule(ctx, req.InstanceName, namespace(req.Domain, req.WorkflowName), req.Parameters, req.Enabled, req.Recurrence, nextRun)
	if err != nil {
		logctx.WithError(err).Error("failed to create workflow schedule")
		if err == libcereal.ErrWorkflowScheduleExists {
			return nil, status.Error(codes.FailedPrecondition, err.Error())
		}
		return nil, err
	}
	return &cereal.CreateWorkflowScheduleResponse{}, nil
}

func (s *CerealService) ListWorkflowSchedules(req *cereal.ListWorkflowSchedulesRequest, out cereal.Cereal_ListWorkflowSchedulesServer) error {
	logctx := logrus.WithFields(logrus.Fields{
		"id":     generateRequestID(),
		"method": "ListWorkflowSchedules",
		"domain": req.Domain,
	})

	if err := validateDomain(req.Domain); err != nil {
		return err
	}

	logctx.Debug("listing workflows")
	schedules, err := s.backend.ListWorkflowSchedules(out.Context())
	if err != nil {
		logctx.WithError(err).Error("failed to list workflow schedules")
		return err
	}

	if len(schedules) > 0 {
		err := out.Send(&cereal.ListWorkflowSchedulesResponse{
			NumSchedules: int32(len(schedules)),
		})
		if err != nil {
			logctx.WithError(err).Error("failed to send message")
		}
		for _, schedule := range schedules {
			var domain string
			domain, schedule.WorkflowName = unnamespace(schedule.WorkflowName)
			if domain != req.Domain {
				continue
			}
			grpcSchedule := cerealScheduleToGrpcSchedule(logctx, schedule)
			err := out.Send(&cereal.ListWorkflowSchedulesResponse{
				Schedule: grpcSchedule,
			})
			if err != nil {
				logctx.WithError(err).Error("failed to send message")
			}
		}

	}
	return nil
}

func (s *CerealService) GetWorkflowScheduleByName(ctx context.Context, req *cereal.GetWorkflowScheduleByNameRequest) (*cereal.GetWorkflowScheduleByNameResponse, error) {
	logctx := logrus.WithFields(logrus.Fields{
		"id":            generateRequestID(),
		"method":        "GetWorkflowScheduleByName",
		"domain":        req.Domain,
		"workflow_name": req.WorkflowName,
		"instance_name": req.InstanceName,
	})
	if err := validateDomain(req.Domain); err != nil {
		return nil, err
	}
	logctx.Debug("getting workflow schedule")
	schedule, err := s.backend.GetWorkflowScheduleByName(ctx, req.InstanceName, namespace(req.Domain, req.WorkflowName))
	if err != nil {
		logctx.WithError(err).Error("failed to get workflow schedule")
		if err == libcereal.ErrWorkflowScheduleNotFound {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, err
	}
	_, schedule.WorkflowName = unnamespace(schedule.WorkflowName)
	return &cereal.GetWorkflowScheduleByNameResponse{
		Schedule: cerealScheduleToGrpcSchedule(logctx, schedule),
	}, nil
}

func cerealScheduleToGrpcSchedule(logctx *logrus.Entry, schedule *backend.Schedule) *cereal.Schedule {
	grpcSchedule := &cereal.Schedule{
		InstanceName: schedule.InstanceName,
		WorkflowName: schedule.WorkflowName,
		Parameters:   schedule.Parameters,
		Recurrence:   schedule.Recurrence,
		Enabled:      schedule.Enabled,
	}
	var err error
	grpcSchedule.NextDueAt, err = ptypes.TimestampProto(schedule.NextDueAt)
	if err != nil {
		logctx.WithError(err).Warn("invalid timestamp")
	}
	grpcSchedule.LastEnqueuedAt, err = ptypes.TimestampProto(schedule.LastEnqueuedAt)
	if err != nil {
		logctx.WithError(err).Warn("invalid timestamp")
	}
	if schedule.LastStart != nil {
		grpcSchedule.LastStart, err = ptypes.TimestampProto(*schedule.LastStart)
		if err != nil {
			logctx.WithError(err).Warn("invalid timestamp")
		}
	}
	if schedule.LastEnd != nil {
		grpcSchedule.LastEnd, err = ptypes.TimestampProto(*schedule.LastEnd)
		if err != nil {
			logctx.WithError(err).Warn("invalid timestamp")

		}
	}
	return grpcSchedule
}

func (s *CerealService) UpdateWorkflowScheduleByName(ctx context.Context, req *cereal.UpdateWorkflowScheduleByNameRequest) (*cereal.UpdateWorkflowScheduleByNameResponse, error) {
	logctx := logrus.WithFields(logrus.Fields{
		"id":            generateRequestID(),
		"method":        "UpdateWorkflowScheduleByName",
		"domain":        req.Domain,
		"workflow_name": req.WorkflowName,
		"instance_name": req.InstanceName,
	})

	if err := validateDomain(req.Domain); err != nil {
		return nil, err
	}

	opts := backend.WorkflowScheduleUpdateOpts{}

	if req.GetEnabled() != nil {
		logctx.Info("updating enabled")
		opts.UpdateEnabled = true
		opts.Enabled = req.GetEnabled().GetValue()
	}

	if req.GetParameters() != nil {
		logctx.Info("updating parameters")
		opts.UpdateParameters = true
		opts.Parameters = req.GetParameters().GetValue()
	}

	if req.GetRecurrence() != nil {
		logctx.Info("updating recurrence")
		opts.UpdateRecurrence = true
		opts.Recurrence = req.GetRecurrence().GetValue()
		_, err := rrule.StrToRRule(req.GetRecurrence().GetValue())
		if err != nil {
			logctx.WithError(err).Error("recurrence rule not valid")
			return nil, status.Error(codes.InvalidArgument, "recurrence rule not valid")
		}
		if req.GetNextRunAt() == nil {
			err := status.Error(codes.InvalidArgument, "expected next run at with recurrence update")
			logctx.WithError(err).Error("next run at  not valid")
			return nil, err
		}
		ts, err := ptypes.Timestamp(req.GetNextRunAt())
		if err != nil {
			logctx.WithError(err).Error("timestamp not valid")
			return nil, status.Error(codes.InvalidArgument, "invalid timestamp")
		}
		opts.NextRunAt = ts
	}

	logctx.Info("updating workflow schedule")
	err := s.backend.UpdateWorkflowScheduleByName(ctx, req.InstanceName, namespace(req.Domain, req.WorkflowName), opts)
	if err != nil {
		logctx.WithError(err).Error("failed to update workflow schedule")
		if err == libcereal.ErrWorkflowScheduleNotFound {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, err
	}
	return &cereal.UpdateWorkflowScheduleByNameResponse{}, nil
}

func (s *CerealService) GetWorkflowInstanceByName(ctx context.Context, req *cereal.GetWorkflowInstanceByNameRequest) (*cereal.GetWorkflowInstanceByNameResponse, error) {
	logctx := logrus.WithFields(logrus.Fields{
		"id":            generateRequestID(),
		"method":        "GetWorkflowInstanceByName",
		"domain":        req.Domain,
		"workflow_name": req.WorkflowName,
		"instance_name": req.InstanceName,
	})
	if err := validateDomain(req.Domain); err != nil {
		return nil, err
	}
	logctx.Debug("getting workflow instance")
	workflowInstance, err := s.backend.GetWorkflowInstanceByName(ctx, req.GetInstanceName(), namespace(req.GetDomain(), req.GetWorkflowName()))
	if err != nil {
		logctx.WithError(err).Error("failed to get workflow instance")
		if err == libcereal.ErrWorkflowInstanceNotFound {
			return nil, status.Error(codes.NotFound, err.Error())
		}
		return nil, err
	}
	_, workflowInstance.WorkflowName = unnamespace(workflowInstance.WorkflowName)
	grpcInstance := cerealWorkflowInstanceToGrpc(workflowInstance)

	return &cereal.GetWorkflowInstanceByNameResponse{
		WorkflowInstance: grpcInstance,
	}, nil
}

func (s *CerealService) ListWorkflowInstances(req *cereal.ListWorkflowInstancesRequest, resp cereal.Cereal_ListWorkflowInstancesServer) error {
	logctx := logrus.WithFields(logrus.Fields{
		"id":     generateRequestID(),
		"domain": req.Domain,
		"method": "ListWorkflowInstances",
	})

	if err := validateDomain(req.Domain); err != nil {
		return err
	}

	opts := backend.ListWorkflowOpts{}

	if workflowName := req.GetWorkflowName().GetValue(); workflowName != "" {
		workflowName = namespace(req.Domain, workflowName)
		opts.WorkflowName = &workflowName
		logctx = logctx.WithField("workflow_name", workflowName)
	}

	if instanceName := req.GetInstanceName().GetValue(); instanceName != "" {
		opts.InstanceName = &instanceName
		logctx = logctx.WithField("instance_name", instanceName)
	}

	if req.GetIsRunning() != nil {
		isRunning := req.GetIsRunning().GetValue()
		opts.IsRunning = &isRunning
		logctx = logctx.WithField("isRunning", isRunning)
	}

	logctx.Debug("listing workflow instances")
	instances, err := s.backend.ListWorkflowInstances(resp.Context(), opts)
	if err != nil {
		logctx.WithError(err).Error("failed to list workflow instances")
		return err
	}

	logctx.Debugf("found %d matching instances", len(instances))
	for _, instance := range instances {
		domain, workflowName := unnamespace(instance.WorkflowName)
		if domain != req.Domain {
			continue
		}
		instance.WorkflowName = workflowName
		err := resp.Send(&cereal.ListWorkflowInstancesResponse{
			WorkflowInstance: cerealWorkflowInstanceToGrpc(instance),
		})
		if err != nil {
			logctx.WithError(err).Error("failed to send workflow instance")
			return err
		}
	}
	return nil
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
