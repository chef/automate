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
}

type DataFeedWorkflowPayload struct {
	TasksComplete bool
	NodeIDs       []map[string]NodeIDs
	NodeIDsIndex  int
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

	switch ev.TaskName {
	case dataFeedPollTaskName:
		taskResults, err := getPollTaskResults(ev)
		if err != nil {
			return w.Fail(err)
		}
		/*
		 * Update the workflow params
		 */
		params.NodeIDs = taskResults.NodeIDs
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
		log.Debugf("PollTask %v, tasks complete: %v, time: %v", dataFeedPollTaskName, payload.TasksComplete, time.Now())
	case dataFeedAggregateTaskName:
		payload.NodeIDsIndex++
		if payload.NodeIDsIndex != len(payload.NodeIDs) {
			params.NodeIDs = payload.NodeIDs[payload.NodeIDsIndex]
			err = w.EnqueueTask(dataFeedAggregateTaskName, params)
			if err != nil {
				return w.Fail(err)
			}
		} else {
			payload.TasksComplete = true
		}
		log.Debugf("AggregateTask %v, tasks complete: %v, time: %v", dataFeedAggregateTaskName, payload.TasksComplete, time.Now())
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
	numBatches := numNodes / batchSize
	if numNodes%batchSize > 0 {
		numBatches++
	}
	log.WithFields(log.Fields{
		"number of nodes":            numNodes,
		"batch size":                 batchSize,
		"number of batches required": numBatches,
	}).Debug("batchNodeIDs()")
	nodeBatches := make([]map[string]NodeIDs, numBatches)
	batch := 0
	count := 0
	// split the nodeIDs map into a batches of smaller maps of batchSize
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
