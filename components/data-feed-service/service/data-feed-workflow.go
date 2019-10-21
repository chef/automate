package service

import (
	"context"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/data-feed-service/dao"
	"github.com/chef/automate/lib/cereal"
)

var (
	dataFeedWorkflowName = cereal.NewWorkflowName("data-feed-workflow")
	dataFeedScheduleName = "periodic-data-feed-workflow"
)

type DataFeedWorkflowExecutor struct {
	workflowName cereal.WorkflowName
}

type DataFeedWorkflowParams struct {
	PollTaskParams       DataFeedPollTaskParams
	ClientTaskParams     DataFeedClientTaskParams
	ComplianceTaskParams DataFeedComplianceTaskParams
	NotifierTaskParams   DataFeedNotifierTaskParams
}

type DataFeedWorkflowPayload struct {
	TasksComplete          bool
	PollTaskComplete       bool
	ClientTaskComplete     bool
	ComplianceTaskComplete bool
	NotifierTaskComplete   bool
	NodeIDs                []string
	ComplianceTaskParams   DataFeedComplianceTaskParams
}

func (e *DataFeedWorkflowExecutor) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	params := DataFeedWorkflowParams{}
	log.Infof("OnStart params %v", params)
	err := w.GetParameters(&params)
	if err != nil {
		return w.Fail(err)
	}

	err = w.EnqueueTask(dataFeedPollTaskName, params)
	if err != nil {
		return w.Fail(err)
	}

	initialPayload := DataFeedWorkflowPayload{}
	return w.Continue(&initialPayload)
}

func (e *DataFeedWorkflowExecutor) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
	params := DataFeedWorkflowParams{}
	err := w.GetParameters(&params)
	if err != nil {
		return w.Fail(err)
	}

	log.Infof("OnTaskComplete params: %v", params)
	payload := DataFeedWorkflowPayload{}
	err = w.GetPayload(&payload)
	if err != nil {
		return w.Fail(err)
	}

	switch ev.TaskName.String() {
	case dataFeedPollTaskName.String():
		taskResults, err := getPollTaskResults(ev)
		if err != nil {
			return w.Fail(err)
		}
		payload.NodeIDs = taskResults.NodeIDs
		payload.PollTaskComplete = true
		params.ClientTaskParams.NodeIDs = payload.NodeIDs
		params.ComplianceTaskParams.FeedStart = taskResults.FeedStart
		params.ComplianceTaskParams.FeedEnd = taskResults.FeedEnd
		payload.ComplianceTaskParams = params.ComplianceTaskParams
		err = w.EnqueueTask(dataFeedClientTaskName, params)
		if err != nil {
			return w.Fail(err)
		}
		log.Infof("PollTaskComplete %v, complete: %v, time: %v", dataFeedPollTaskName, payload.PollTaskComplete, time.Now())

	case dataFeedClientTaskName.String():
		if payload.PollTaskComplete == false {
			log.Infof("data-feed-poll has not completed yet, are we called first? enqueue data-feed-client again")
			err = w.EnqueueTask(dataFeedClientTaskName, params)
			if err != nil {
				return w.Fail(err)
			}
		} else {
			log.Infof("data-feed-poll has completed get client results %v", params)
			taskResults, err := getClientTaskResults(ev)
			if err != nil {
				return w.Fail(err)
			}
			payload.ComplianceTaskParams.DataFeedMessages = taskResults.DataFeedMessages
			payload.ClientTaskComplete = true
			params.ComplianceTaskParams = payload.ComplianceTaskParams
			err = w.EnqueueTask(dataFeedComplianceTaskName, params)
			if err != nil {
				return w.Fail(err)
			}
		}
		log.Infof("ClientTaskComplete %v, complete: %v, time: %v", dataFeedClientTaskName, payload.ClientTaskComplete, time.Now())
	case dataFeedComplianceTaskName.String():
		log.Infof("data-feed-compliance task in OnTaskComplete")
		taskResults, err := getComplianceTaskResults(ev)
		if err != nil {
			return w.Fail(err)
		}
		payload.ComplianceTaskComplete = true
		params.NotifierTaskParams.DataFeedMessages = taskResults.DataFeedMessages
		err = w.EnqueueTask(dataFeedNotifierTaskName, params)
		if err != nil {
			return w.Fail(err)
		}
		log.Infof("ComplianceTaskComplete %v, complete: %v, time: %v", dataFeedComplianceTaskName, payload.ComplianceTaskComplete, time.Now())
	case dataFeedNotifierTaskName.String():
		payload.NotifierTaskComplete = true
		log.Infof("NotifierTaskComplete %v, complete: %v, time: %v", dataFeedNotifierTaskName, payload.NotifierTaskComplete, time.Now())
	}

	payload.TasksComplete = payload.PollTaskComplete && payload.ClientTaskComplete && payload.ComplianceTaskComplete && payload.NotifierTaskComplete
	log.Infof("TasksComplete %v, complete: %v", ev.TaskName, payload.TasksComplete)
	if payload.TasksComplete {
		return w.Complete()
	}
	return w.Continue(&payload)

}

func (e *DataFeedWorkflowExecutor) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	return w.Complete()
}

func getPollTaskResults(ev cereal.TaskCompleteEvent) (DataFeedPollTaskResults, error) {
	taskResults := DataFeedPollTaskResults{}
	err := ev.Result.Get(&taskResults)
	log.Infof("Task results %v", taskResults)
	return taskResults, err
}

func getClientTaskResults(ev cereal.TaskCompleteEvent) (DataFeedClientTaskResults, error) {
	taskResults := DataFeedClientTaskResults{}
	err := ev.Result.Get(&taskResults)
	log.Infof("Client task result %v", taskResults)
	return taskResults, err

}

func getComplianceTaskResults(ev cereal.TaskCompleteEvent) (DataFeedComplianceTaskResults, error) {
	taskResults := DataFeedComplianceTaskResults{}
	err := ev.Result.Get(&taskResults)
	log.Infof("Compliance task result %v", taskResults)
	return taskResults, err
}

func (d *DataFeedComplianceTask) Run2(ctx context.Context, task cereal.Task) (interface{}, error) {
	params := DataFeedWorkflowParams{}
	err := task.GetParameters(&params)
	if err != nil {
		return nil, errors.Wrap(err, "failed to parse task parameters")
	}
	log.Infof("Run params %v", params)
	now := time.Now()

	feedStartTime := now
	feedEndTime := now
	destinations := []dao.Destination{}
	nodeIDs := []string{}
	// build messages for any nodes which have had a CCR in last window, include last compliance report
	datafeedMessages := map[string]datafeedMessage{}
	// build messages for any reports in last window include last CCR and OHAI
	d.buildReportFeed(ctx, params.PollTaskParams.ReportsPageSize, feedStartTime, feedEndTime, datafeedMessages)
	// get the valus from this ipaddress:datafeedMessage map
	data := make([]datafeedMessage, 0, len(datafeedMessages))
	for _, value := range datafeedMessages {
		data = append(data, value)
	}

	if len(datafeedMessages) == 0 {
		return nil, nil
	}
	for destination := range destinations {
		log.Debugf("Destination name %v", destinations[destination].Name)
		log.Debugf("Destination url %v", destinations[destination].URL)
		log.Debugf("Destination secret %v", destinations[destination].Secret)

		username, password, err := GetCredentials(ctx, d.secrets, destinations[destination].Secret)

		if err != nil {
			log.Errorf("Error retrieving credentials, cannot send asset notification: %v", err)
			// TODO error handling - need some form of report in automate that indicates when data was sent and if it was successful
		} else {
			// build and send notification for this rule
			notification := datafeedNotification{username: username, password: password, url: destinations[destination].URL, data: data}

			client := NewDataClient()
			err = send(client, notification)
			if err != nil {
				handleSendErr(notification, feedStartTime, feedEndTime, err)
			}
		}
	}

	return &DataFeedPollTaskResults{
		Destinations: destinations,
		NodeIDs:      nodeIDs,
	}, nil
}
