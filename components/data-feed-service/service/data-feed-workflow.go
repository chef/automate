package service

import (
	"context"
	"time"

	"github.com/chef/automate/components/data-feed-service/config"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/cereal"
)

var (
	dataFeedWorkflowName = cereal.NewWorkflowName("data-feed-workflow")
	dataFeedScheduleName = "periodic-data-feed-workflow"
)

type DataFeedWorkflowExecutor struct {
	workflowName   cereal.WorkflowName
	dataFeedConfig *config.DataFeedConfig
	manager        *cereal.Manager
}

// DataFeedWorkflowParams the params for the workflow and all the tasks in the workflow
type DataFeedWorkflowParams struct {
	FeedStart time.Time
	FeedEnd   time.Time
}

type DataFeedWorkflowPayload struct {
	NodeIDs      []map[string]NodeIDs
	NodeIDsIndex int
	FeedStart    time.Time
	FeedEnd      time.Time
}

func (e *DataFeedWorkflowExecutor) OnStart(w cereal.WorkflowInstance, ev cereal.StartEvent) cereal.Decision {
	params := DataFeedWorkflowParams{}
	log.Debugf("OnStart params %v", params)
	err := w.GetParameters(&params)
	if err != nil {
		return w.Fail(err)
	}

	dataFeedTaskParams := DataFeedPollTaskParams{
		FeedInterval:    e.dataFeedConfig.ServiceConfig.FeedInterval,
		AssetPageSize:   e.dataFeedConfig.ServiceConfig.AssetPageSize,
		ReportsPageSize: e.dataFeedConfig.ServiceConfig.ReportsPageSize,
		FeedStart:       params.FeedStart,
		FeedEnd:         params.FeedEnd,
	}
	/*
	 * Start the workflow with the poll task to get any nodes that have had client runs
	 */
	err = w.EnqueueTask(dataFeedPollTaskName, dataFeedTaskParams)
	if err != nil {
		return w.Fail(err)
	}

	initialPayload := DataFeedWorkflowPayload{}
	return w.Continue(&initialPayload)
}

func (e *DataFeedWorkflowExecutor) OnTaskComplete(w cereal.WorkflowInstance, ev cereal.TaskCompleteEvent) cereal.Decision {
	workflowParams := DataFeedWorkflowParams{}
	err := w.GetParameters(&workflowParams)
	if err != nil {
		return w.Fail(err)
	}
	tasksComplete := false
	log.Debugf("OnTaskComplete workflowParams: %v", workflowParams)
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
		payload.FeedStart = taskResults.FeedStart
		payload.FeedEnd = taskResults.FeedEnd
		workflowParams.FeedStart = taskResults.FeedEnd
		workflowParams.FeedEnd = workflowParams.FeedStart.Add(e.dataFeedConfig.ServiceConfig.FeedInterval)

		// we must update the workflow workflowParams to ensure the nest schedule has the update interval times
		err = e.manager.UpdateWorkflowScheduleByName(context.Background(), dataFeedScheduleName, dataFeedWorkflowName,
			cereal.UpdateParameters(workflowParams),
			cereal.UpdateEnabled(true))
		if err != nil {
			return w.Fail(err)
		}

		if len(taskResults.NodeIDs) > 0 {
			payload.NodeIDs = batchNodeIDs(e.dataFeedConfig.ServiceConfig.NodeBatchSize, taskResults.NodeIDs)
			log.Debugf("payload.NodeIDs %v payload.NodeIDsIndex %v", payload.NodeIDs, payload.NodeIDsIndex)
			aggregateParams := DataFeedAggregateTaskParams{
				NodeIDs:          payload.NodeIDs[payload.NodeIDsIndex],
				UpdatedNodesOnly: e.dataFeedConfig.ServiceConfig.UpdatedNodesOnly,
				FeedStart:        payload.FeedStart,
				FeedEnd:          payload.FeedEnd,
			}
			err = w.EnqueueTask(dataFeedAggregateTaskName, aggregateParams)
			if err != nil {
				return w.Fail(err)
			}
		} else {
			// no nodes have been updated by client run and
			// no compliance reports have been run in the interval
			// the workflow is therefore complete
			tasksComplete = true
			log.Debugf("No new client data or reports found")
		}
		log.Debugf("PollTask %v, tasks complete: %v, time: %v", dataFeedPollTaskName, tasksComplete, time.Now())
	case dataFeedAggregateTaskName:
		payload.NodeIDsIndex++
		if payload.NodeIDsIndex != len(payload.NodeIDs) {
			aggregateParams := DataFeedAggregateTaskParams{
				NodeIDs:          payload.NodeIDs[payload.NodeIDsIndex],
				UpdatedNodesOnly: e.dataFeedConfig.ServiceConfig.UpdatedNodesOnly,
				FeedStart:        payload.FeedStart,
				FeedEnd:          payload.FeedEnd,
			}
			err = w.EnqueueTask(dataFeedAggregateTaskName, aggregateParams)
			if err != nil {
				return w.Fail(err)
			}
		} else {
			tasksComplete = true
		}
		log.Debugf("AggregateTask %v, tasks complete: %v, time: %v", dataFeedAggregateTaskName, tasksComplete, time.Now())
	}

	log.Debugf("TasksComplete %v, complete: %v", ev.TaskName, tasksComplete)
	if tasksComplete {
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
