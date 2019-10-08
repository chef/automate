package main

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/cereal"
)

// EchoTask and EchoWorkflow
//
// This file contains the implementation of a simple task and workflow
// that trivially prints to the log.
//
// The point of the example is to show the various parts of the cereal
// API.
//

// EchoTask
//
// A "Task" is a unit of work you need to do. For example, running a
// script against Elasticsearch, running an inspec scan, or scanning a
// database for old nodes might all be "Tasks". Tasks are defined in
// code by implementing the TaskExecutor interface:
//
// type TaskExecutor interface {
// 	Run(ctx context.Context, task Task) (result interface{}, err error)
// }
//
// Real-world tasks can now be found throughout our codebase as we
// have integrated cereal into a number of services. For example, here
// is a task that is part of compliance:
//
// https://github.com/chef/automate/blob/master/components/compliance-service/inspec-agent/runner/runner.go#L543-L558
//
//

// EchoTask is the struct that we will implement TaskExecutor for.
//
// Note that we don't keep much state on this struct. In a more real
// example, we might keep API clients or other resources needed to
// perform the task, but we wouldn't keep _data_ related to the task
type EchoTask struct{}

// EchoTaskParams holds our "parametesrs." Parameters are the
// arguments to our Task executor. Parameters mush be serializable as
// JSON.
type EchoTaskParams struct {
	Msg string
}

// EchoTaskResults holds our "results". This is the "return value" of
// our task. We can return whatever we want as long as it is
// serializable to JSON.
type EchoTaskResults struct {
	Status string
}

func (e *EchoTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	params := EchoTaskParams{}
	err := task.GetParameters(&params)
	if err != nil {
		// Because our parameters need to be deserialized, we
		// need to handle the case where the deserialization
		// fails. In most cases, we just return the error.
		return nil, err
	}

	// Now, we might do some work in our parameters. Here let's
	// just echo it.
	logrus.Infof("echoTask: %s", params.Msg)

	// Now, let's return some information about the task we just
	// completed.
	return EchoTaskResults{
		Status: "echoed!",
	}, nil
}

// Echo Workflow
//
// A "workflow" is used to coordinate tasks. Unlike tasks, workflows
// should not contain any heavy-weight work but should rather be
// composed of simple, idempotent code.
//
// A Workflow is defined by implementing the cereal.WorkflowExecutor
// interface.
//
// type WorkflowExecutor interface {
//	OnStart(w WorkflowInstance, ev StartEvent) Decision
// 	OnTaskComplete(w WorkflowInstance, ev TaskCompleteEvent) Decision
// 	OnCancel(w WorkflowInstance, ev CancelEvent) Decision
// }
//
// Each interface method is called in response to defined events in
// the lifecycle of a workflow.

// EchoWorkflow is the struct that we are going to define the workflow
// on.
//
// Similar to Tasks, a workflow's state shouldn't be stored in the
// struct that implements the workflow executor since different parts
// of the workflow's execution might happen on different
// machines.
type EchoWorkflow struct{}

var _ cereal.WorkflowExecutor = &EchoWorkflow{}

// EchoWorkflowParameters holds our workflow parameters. Like task
// parameters, these are the arguments to our workflow. Parameters are
// set at the time that the workflow is enqueued to run. These
// parameters will be the same for the lifetime of the workflow
// instance. The parameters must be serializable to JSON.
type EchoWorkflowParameters struct {
	Count int
}

// EchoWorkflowPayload holds our payload. The payload allows us store
// arbitrary state that can be access during the lifetime of the
// workflow. The payload is updated at the end of each callback. The
// payload must be serializable to JSON.
//
// Since a workflow might have a number of steps, the payload is how
// you can maintain state between each of those steps.
type EchoWorkflowPayload struct {
	Done int
}

// EchoWorkflow WorkflowExecutor Implementation
//
// Each of these callbacks are called by cereal in respond to
// events. Events are generated either by code in your application or
// by previous decisions made by the workflow itself.
//

// OnStart is called in response to the StartEvent. This is the entry
// point to your workflow. A StartEvent is produced when you tell
// cereal that you want to run this workflow.
//
// In each callback, we are going to take our parameters, our payload,
// and the event, and make a decision about what to do next.
//
// We signal this decision to cereal by returning a
// cereal.Decision. The possible decisions you can return at the
// moment are:
//
// - `w.Continue(payload)` which signals that the workflow has more
//   work to do and should continue listening for new events.
//
// - `w.Complete()` which signals that the workflow has completed
//   successfully. No further events will be processed.
//
// - `w.Fail(err)` which signals that the workflow has experienced an
//   error and cannot proceed further. No further events will be
//   processed.
//
func (m *EchoWorkflow) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	// IGNORE ME (this is here to get some basic timing info since
	// we also use this for development)
	startTime := time.Now()
	echoWorkflowStartTime = &startTime

	// We can get the parameters that this workflow instance was
	// started with.
	params := EchoWorkflowParameters{}
	// Because these parameters are serialized and persisted to a
	// backend data store, we need to deal with the possibility
	// that the deserialization fails.
	err := w.GetParameters(&params)
	if err != nil {
		return w.Fail(err)
	}

	logrus.WithFields(logrus.Fields{
		"params": params,
	}).Info("EchoWorkflow got StartEvent")

	// We aren't worrying about payload here, because this is our
	// entry point. We don't expect to have a meaningful payload.

	// The main purpose of a workflow is to organize the execution
	// of tasks to achieve some overall affect. We can start new
	// tasks by calling EnqueueTask on the WorkflowInstance we
	// received as a parameter.
	//
	// Let's enqueue a number of echo tasks based on our
	// parameters. Tasks aren't enqueued until the end of the
	// currently running callback. All of these tasks will be
	// enqueued after we return from this function.
	if params.Count < 1 {
		return w.Fail(errors.New("invalid echo count"))
	}

	for i := 0; i < params.Count; i++ {
		// We identify tasks to enqueue using the name that we
		// registered them with.
		//
		// Note, we can enqueue the same named task multiple
		// times with different parameters.
		err := w.EnqueueTask(EchoTaskName, EchoTaskParams{
			Msg: fmt.Sprintf("I'm agent 00%d", i),
		})
		if err != nil {
			// The kinds of errors that you would see here are
			// related to the options you can pass as an optional
			// 3rd parameter. The actual task is not enqueued for
			// running until this function returns.
			return w.Fail(err)
		}
	}

	// Payloads are how a workflow keeps state. Typically in
	// OnStart we will initialize the payload.
	initialPayload := EchoWorkflowPayload{
		Done: 0,
	}
	// We return a Continue decision allowing the workflow to
	// continue to process the next workflow event. We pass the
	// payload to Continue so that it will be available on the
	// next interation.
	return w.Continue(&initialPayload)
}

// OnTaskComplete runs whenever a Task is completed. This same
// callback is called both for successfully completed Tasks and Tasks
// that failed either because of an error in the TaskExecutor or
// because a a problem in cereal.
func (m *EchoWorkflow) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
	// Again, if we need them, the parameters are always available to us.
	params := EchoWorkflowParameters{}
	err := w.GetParameters(&params)
	if err != nil {
		return w.Fail(err)
	}

	// We can also get the current state of the Payload. Similar
	// to the parameters, we need to account for possible
	// deserialization errors.
	payload := EchoWorkflowPayload{}
	err = w.GetPayload(&payload)
	if err != nil {
		return w.Fail(err)
	}

	// Information about the task that is complete is available to
	// us via the TaskCompleteEvent. We can get the name of the
	// task via ev.TaskName.
	//
	// ev.Result has the starting parameters, returned results, and
	// returned error from the task.
	//
	// We can check if this task failed by checking for the error.
	err = ev.Result.Err()
	if err != nil {
		// Here we could decide what to do based on the exact
		// error message we received. For example, we could
		// retry this particular task by re-enqueing it. Here,
		// we will simply fail the entire workflow with the
		// same error.
		return w.Fail(err)
	}

	// If we needed them for making decisions, we can extract the
	// parameters that this task ran with.
	//
	// We don't really need them in this case, but we include this
	// for completeness.
	taskParams := EchoTaskParams{}
	err = ev.Result.GetParameters(&taskParams)
	if err != nil {
		return w.Fail(err)
	}

	// We can also extract any results that the task saved for us.
	taskResults := EchoTaskResults{}
	if err := ev.Result.Get(&taskResults); err != nil {
		return w.Fail(err)
	}

	logrus.WithFields(logrus.Fields{
		"task_name":  ev.TaskName,
		"payload":    payload,
		"params":     params,
		"taskParams": taskParams,
		"taskResult": taskResults,
	}).Info("EchoWorkflow got TaskCompleted")

	// This workflow wants to keep running until all of our tasks
	// our done. Thus we keep a count of the done task and only
	// call w.Complete() when we've seen enough done tasks.
	//
	// A couple things to note here:
	//
	// 1) We can do this counting without any locks because the
	// framework ensures that even though all those tasks might be
	// completing concurrently, the processing of workflow events
	// happens one at a time.
	//
	// 2) Unlike tasks, if we follow the rules for writing
	// workflows, then even if we lose the worker executing this
	// callback, we can resume on another node which will start
	// processing the same event with the same payload, producing
	// the same end result.
	payload.Done++
	if payload.Done >= params.Count {
		return w.Complete()
	} else {
		return w.Continue(&payload)
	}
}

// OnCancel responds to the CancelEvent which allows you to run code
// if a cancel signal get sent to the workflow. Right now, you are
// fairly limited in terms of what you can do during a OnCancel.
//
// Like any other callback, you can enqueue tasks (say, to do some
// cleanup), but we don't have a way to explicitly cancel running
// tasks.  Rather when you return w.Complete() any still-enqueued
// tasks will immediately be dropped and any running tasks will
// eventually have their context canceled.
func (m *EchoWorkflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	return w.Complete()
}
