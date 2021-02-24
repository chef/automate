package migration

import (
	"context"
	"fmt"
	"sort"
	"time"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/components/ingest-service/backend"
)

func (ms *Status) migrateAction() {
	ms.total = 1
	err := ms.SendAllActionsThroughPipeline()
	if err != nil {
		ms.updateErr(err.Error(), "Unable to re-insert actions")
	}
	ms.taskCompleted()

	ms.finish("Migration Actions to current finished successfully")
}

func (ms *Status) SendAllActionsThroughPipeline() error {
	ms.update("loop through each action and send it though the pipeline")
	// Get all actions-* indexes
	actionIndices, _ := ms.client.GetAllTimeseriesIndiceNames(ms.ctx, actionPrefixIndexName)
	ctx := context.Background()

	// sort the actionIndices to have the eariest first
	sort.Sort(sort.Reverse(sort.StringSlice(actionIndices)))

	// loop through each index and re-ingest all the actions then delete the index.
	for _, fullActionIndexName := range actionIndices {
		ms.update(fmt.Sprintf("loop through each action in the %q index", fullActionIndexName))
		pager := &actionsPaginationContext{
			pageSize:  300,
			client:    ms.client,
			indexName: fullActionIndexName,
		}

		for err := pager.Start(); !pager.IsDone(); err = pager.Next() {
			if err != nil {
				ms.updateErr(err.Error(), "Unable to retrieve actions")
				return err
			}

			for _, action := range pager.results {
				err = ms.sendActionThroughPipeline(action)
				if err != nil {
					logWarning(err.Error(), fmt.Sprintf("Unable re-insert actions: %v", action))
				}
			}
		}

		ms.update(fmt.Sprintf("Deleting the %q index", fullActionIndexName))
		err := ms.client.DeleteIndex(ctx, fullActionIndexName)
		if err != nil {
			logWarning(err.Error(), fmt.Sprintf("Unable to delete index: %v", fullActionIndexName))
		}
	}

	return nil
}

func (ms *Status) sendActionThroughPipeline(internalAction backend.InternalChefAction) error {
	errc := make(chan error)
	defer close(errc)

	action := internalChefActionToRawAction(internalAction)
	var err error
	if errQ := ms.actionPipeline.Run(ms.ctx, action, errc); errQ != nil {
		err = errQ
	} else {
		err = <-errc
	}

	return err
}

func internalChefActionToRawAction(internalAction backend.InternalChefAction) *chef.Action {
	return &chef.Action{
		Id:               internalAction.Id,
		MessageType:      internalAction.MessageType,
		MessageVersion:   internalAction.MessageVersion,
		EntityName:       internalAction.EntityName,
		EntityType:       internalAction.EntityType,
		Task:             internalAction.Task,
		OrganizationName: internalAction.OrganizationName,
		RemoteHostname:   internalAction.RemoteHostname,
		RunId:            internalAction.RunId,
		NodeId:           internalAction.NodeId,
		RecordedAt:       internalAction.RecordedAt.Format(time.RFC3339),
		RemoteRequestId:  internalAction.RemoteRequestId,
		RequestId:        internalAction.RequestId,
		RequestorName:    internalAction.RequestorName,
		RequestorType:    internalAction.RequestorType,
		ServiceHostname:  internalAction.ServiceHostname,
		UserAgent:        internalAction.UserAgent,
		ParentName:       internalAction.ParentName,
		ParentType:       internalAction.ParentType,
		RevisionId:       internalAction.RevisionId,
	}
}

type actionsPaginationContext struct {
	cursorDate time.Time
	cursorID   string
	pageSize   int
	results    []backend.InternalChefAction
	client     backend.Client
	indexName  string
}

func (p *actionsPaginationContext) Start() error {
	actions, _, err := p.client.GetActions(p.indexName, p.pageSize, time.Time{}, "", false)
	if err != nil {
		return err
	}

	p.HandleResults(actions)
	return nil
}

func (p *actionsPaginationContext) Next() error {
	actions, _, err := p.client.GetActions(p.indexName, p.pageSize, p.cursorDate, p.cursorID, true)
	if err != nil {
		return err
	}

	p.HandleResults(actions)
	return nil
}

func (p *actionsPaginationContext) HandleResults(actions []backend.InternalChefAction) {
	p.results = actions
	if len(actions) > 0 {
		lastAction := actions[len(actions)-1]
		p.cursorID = lastAction.Id
		p.cursorDate = lastAction.RecordedAt
	}
}

func (p *actionsPaginationContext) IsDone() bool {
	return len(p.results) == 0
}
