package service

import (
	"time"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/cereal"
)

var (
	dataFeedWorkflowName = cereal.NewWorkflowName("data-feed-workflow")
	dataFeedScheduleName = "periodic-data-feed-workflow"
)

type DataFeedWorkflowExecutor struct {
	workflowName cereal.WorkflowName
}

// DataFeedWorkflowParams the params for the workflow and all the tasks in the workflow
type DataFeedWorkflowParams struct {
	NodeIDs          map[string]NodeIDs
	FeedStart        time.Time
	FeedEnd          time.Time
	PollTaskParams   DataFeedPollTaskParams
	DataFeedMessages map[string]datafeedMessage
}

type DataFeedWorkflowPayload struct {
	TasksComplete           bool
	PollTaskComplete        bool
	ListReportsTaskComplete bool
	ClientTaskComplete      bool
	ComplianceTaskComplete  bool
	NotifierTaskComplete    bool
	NodeIDs                 map[string]NodeIDs
	DataFeedMessages        map[string]datafeedMessage
}

func (e *DataFeedWorkflowExecutor) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	params := DataFeedWorkflowParams{}
	log.Debugf("OnStart params %v", params)
	err := w.GetParameters(&params)
	if err != nil {
		return w.Fail(err)
	}

	/*
	 * Start the workflow with the poll task to get any nodes that have had client runs
	 */
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

	log.Debugf("OnTaskComplete params: %v", params)
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
		/*
		 * Update the workflow params
		 */
		payload.NodeIDs = taskResults.NodeIDs
		params.NodeIDs = payload.NodeIDs
		params.FeedStart = taskResults.FeedStart
		params.FeedEnd = taskResults.FeedEnd
		payload.PollTaskComplete = true
		/*
		 * Next we get the report ID's of any node that has had a compliance report
		 */
		err = w.EnqueueTask(dataFeedListReportsTaskName, params)
		if err != nil {
			return w.Fail(err)
		}
		log.Debugf("PollTaskComplete %v, complete: %v, time: %v", dataFeedPollTaskName, payload.PollTaskComplete, time.Now())
	case dataFeedListReportsTaskName.String():
		taskResults, err := getListReportsTaskResults(ev)
		if err != nil {
			return w.Fail(err)
		}
		payload.NodeIDs = taskResults.NodeIDs
		params.NodeIDs = payload.NodeIDs
		payload.PollTaskComplete = true
		// should only enqueue if len nodes > 0
		if len(payload.NodeIDs) > 0 {
			// the client task will get the data for nodes with a client run in the interval
			err = w.EnqueueTask(dataFeedClientTaskName, params)
			if err != nil {
				return w.Fail(err)
			}
		} else {
			// no nodes have been updated by client run and
			// no compliance reports have been run in the interval
			// the workflow is therefore complete
			payload.PollTaskComplete = true
			payload.ListReportsTaskComplete = true
			payload.ClientTaskComplete = true
			payload.ComplianceTaskComplete = true
			payload.NotifierTaskComplete = true
			log.Debugf("No new client data or reports found")
		}
		payload.ListReportsTaskComplete = true
		log.Debugf("ListReportsTaskComplete %v, complete: %v, time: %v", dataFeedListReportsTaskName, payload.ListReportsTaskComplete, time.Now())
	case dataFeedClientTaskName.String():
		taskResults, err := getClientTaskResults(ev)
		if err != nil {
			return w.Fail(err)
		}
		payload.NodeIDs = taskResults.NodeIDs
		params.NodeIDs = payload.NodeIDs
		payload.DataFeedMessages = taskResults.DataFeedMessages
		payload.ClientTaskComplete = true
		// if there are still node IDs in the map we must get the compliance reports for them
		if len(payload.NodeIDs) > 0 {
			err = w.EnqueueTask(dataFeedComplianceTaskName, params)
			if err != nil {
				return w.Fail(err)
			}
		} else {
			// no reports to aggregate from the interval send the feed
			payload.ComplianceTaskComplete = true
			params.DataFeedMessages = payload.DataFeedMessages
			err = w.EnqueueTask(dataFeedNotifierTaskName, params)
			if err != nil {
				return w.Fail(err)
			}
		}
		log.Debugf("ClientTaskComplete %v, complete: %v, time: %v", dataFeedClientTaskName, payload.ClientTaskComplete, time.Now())
	case dataFeedComplianceTaskName.String():
		log.Debugf("data-feed-compliance task in OnTaskComplete")
		taskResults, err := getComplianceTaskResults(ev)
		if err != nil {
			return w.Fail(err)
		}
		payload.ComplianceTaskComplete = true
		for ip, dataFeedMessage := range taskResults.DataFeedMessages {
			payload.DataFeedMessages[ip] = dataFeedMessage
		}
		params.DataFeedMessages = payload.DataFeedMessages
		err = w.EnqueueTask(dataFeedNotifierTaskName, params)
		if err != nil {
			return w.Fail(err)
		}
		log.Debugf("ComplianceTaskComplete %v, complete: %v, time: %v", dataFeedComplianceTaskName, payload.ComplianceTaskComplete, time.Now())
	case dataFeedNotifierTaskName.String():
		payload.NotifierTaskComplete = true
		log.Debugf("NotifierTaskComplete %v, complete: %v, time: %v", dataFeedNotifierTaskName, payload.NotifierTaskComplete, time.Now())
	}

	payload.TasksComplete = payload.PollTaskComplete && payload.ListReportsTaskComplete && payload.ClientTaskComplete && payload.ComplianceTaskComplete && payload.NotifierTaskComplete
	log.Debugf("TasksComplete %v, complete: %v", ev.TaskName, payload.TasksComplete)
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
	log.Debugf("Poll task results %v", taskResults)
	return taskResults, err
}

func getListReportsTaskResults(ev cereal.TaskCompleteEvent) (DataFeedListReportsTaskResults, error) {
	taskResults := DataFeedListReportsTaskResults{}
	err := ev.Result.Get(&taskResults)
	log.Debugf("List Reports Task results %v", taskResults)
	return taskResults, err
}

func getClientTaskResults(ev cereal.TaskCompleteEvent) (DataFeedClientTaskResults, error) {
	taskResults := DataFeedClientTaskResults{}
	err := ev.Result.Get(&taskResults)
	log.Debugf("Client task result %v", taskResults)
	return taskResults, err

}

func getComplianceTaskResults(ev cereal.TaskCompleteEvent) (DataFeedComplianceTaskResults, error) {
	taskResults := DataFeedComplianceTaskResults{}
	err := ev.Result.Get(&taskResults)
	log.Debugf("Compliance task result %v", taskResults)
	return taskResults, err
}
