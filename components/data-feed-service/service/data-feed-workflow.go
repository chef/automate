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
	UpdatedNodesOnly bool
	NodeBatchSize    int
	NodeIDs          map[string]NodeIDs
	FeedStart        time.Time
	FeedEnd          time.Time
	PollTaskParams   DataFeedPollTaskParams
	DataFeedMessages map[string]map[string]interface{}
}

type DataFeedWorkflowPayload struct {
	TasksComplete    bool
	NodeIDs          []map[string]NodeIDs
	NodeIDsIndex     int
	DataFeedMessages map[string]map[string]interface{}
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
		params.NodeIDs = taskResults.NodeIDs
		params.FeedStart = taskResults.FeedStart
		params.FeedEnd = taskResults.FeedEnd
		/*
		 * Next we get the report ID's of any node that has had a compliance report
		 */
		err = w.EnqueueTask(dataFeedListReportsTaskName, params)
		if err != nil {
			return w.Fail(err)
		}
		log.Debugf("PollTask %v, tasks complete: %v, time: %v", dataFeedPollTaskName, payload.TasksComplete, time.Now())
	case dataFeedListReportsTaskName.String():
		taskResults, err := getListReportsTaskResults(ev)
		if err != nil {
			return w.Fail(err)
		}
		// should only enqueue if len nodes > 0
		if len(taskResults.NodeIDs) > 0 {
			payload.NodeIDs = batchNodeIDs(params.NodeBatchSize, taskResults.NodeIDs)
			log.Debugf("payload.NodeIDs %v payload.NodeIDsIndex %v", payload.NodeIDs, payload.NodeIDsIndex)
			params.NodeIDs = payload.NodeIDs[payload.NodeIDsIndex]
			err = w.EnqueueTask(dataFeedAggregateTaskName, params)
			if err != nil {
				return w.Fail(err)
			}
		} else {
			// no nodes have been updated by client run and
			// no compliance reports have been run in the interval
			// the workflow is therefore complete
			payload.TasksComplete = true
			log.Debugf("No new client data or reports found")
		}
		log.Debugf("ListReports %v, tasks complete: %v, time: %v", dataFeedListReportsTaskName, payload.TasksComplete, time.Now())
	case dataFeedAggregateTaskName.String():
		taskResults, err := getAggregateTaskResults(ev)
		if err != nil {
			return w.Fail(err)
		}
		payload.NodeIDs[payload.NodeIDsIndex] = taskResults.NodeIDs
		params.NodeIDs = payload.NodeIDs[payload.NodeIDsIndex]
		payload.DataFeedMessages = taskResults.DataFeedMessages

		// no reports to aggregate from the interval send the feed
		params.DataFeedMessages = payload.DataFeedMessages
		err = w.EnqueueTask(dataFeedNotifierTaskName, params)
		if err != nil {
			return w.Fail(err)
		}

		log.Debugf("AggregateTask %v, tasks complete: %v, time: %v", dataFeedAggregateTaskName, payload.TasksComplete, time.Now())
	case dataFeedNotifierTaskName.String():
		payload.NodeIDsIndex++
		if payload.NodeIDsIndex != len(payload.NodeIDs) {
			params.NodeIDs = payload.NodeIDs[payload.NodeIDsIndex]
			err = w.EnqueueTask(dataFeedAggregateTaskName, params)
			if err != nil {
				return w.Fail(err)
			}
		} else {
			payload.DataFeedMessages = make(map[string]map[string]interface{})
			payload.TasksComplete = true
		}
		log.Debugf("NotifierTask %v, tasks complete: %v, time: %v", dataFeedNotifierTaskName, payload.TasksComplete, time.Now())
	}

	log.Debugf("TasksComplete %v, complete: %v", ev.TaskName, payload.TasksComplete)
	if payload.TasksComplete {
		return w.Complete()
	}
	return w.Continue(&payload)

}

func (e *DataFeedWorkflowExecutor) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	return w.Complete()
}

func batchNodeIDs(batchSize int, nodeIDs map[string]NodeIDs) []map[string]NodeIDs {
	numNodes := len(nodeIDs)
	log.Debugf("number of nodes %v", numNodes)
	var numBatches int
	if numNodes < batchSize {
		numBatches = 1
	} else {
		numBatches = numNodes / batchSize
	}
	if numNodes%1 > 0 {
		numBatches++
	}
	log.Debugf("number of node batches required %v", numBatches)
	nodeBatches := make([]map[string]NodeIDs, numBatches)
	batch := 0
	count := 0
	// split the nodeIDs map into a batches of smaller maps of size limit
	for k, v := range nodeIDs {
		if count%batchSize == 0 {
			if count != 0 {
				batch++
			}
			nodeBatches[batch] = make(map[string]NodeIDs, batchSize)
		}
		count++
		nodeBatches[batch][k] = v
	}
	return nodeBatches
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

func getAggregateTaskResults(ev cereal.TaskCompleteEvent) (DataFeedAggregateTaskResults, error) {
	taskResults := DataFeedAggregateTaskResults{}
	err := ev.Result.Get(&taskResults)
	log.Debugf("Client task result %v", taskResults)
	return taskResults, err
}
