package migration

import (
	"time"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/components/ingest-service/backend"
)

func (ms *Status) migrateAction() error {
	ms.total = 2

	err := ms.SendAllActionsThroughPipeline()
	if err != nil {
		ms.updateErr(err.Error(), "Unable to re-insert actions")
		return err
	}
	ms.taskCompleted()

	err = ms.DeleteAllActionsIndexes()
	if err != nil {
		ms.updateErr(err.Error(), "Unable to delete action indexes")
		return err
	}
	ms.taskCompleted()

	ms.finish("Migration Actions to current finished successfully")

	return nil
}

func (ms *Status) DeleteAllActionsIndexes() error {
	ms.update("Delete all the actions indices")
	return ms.client.DeleteAllIndexesWithPrefix(actionPrefixIndexName, ms.ctx)
}

func (ms *Status) SendAllActionsThroughPipeline() error {
	ms.update("loop through each action and send it though the pipeline")
	pager := &actionsPaginationContext{
		pageSize: 100,
		client:   ms.client,
	}

	for err := pager.Start(); !pager.IsDone(); err = pager.Next() {
		if err != nil {
			ms.updateErr(err.Error(), "Unable to retrieve actions")
			return err
		}

		for _, action := range pager.results {
			err = ms.sendActionThroughPipeline(action)
			if err != nil {
				ms.updateErr(err.Error(), "Unable to retrieve actions")
				return err
			}
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
}

func (p *actionsPaginationContext) Start() error {
	actions, _, err := p.client.GetActions(actionsIndexName, p.pageSize, time.Time{}, "", false)
	if err != nil {
		return err
	}

	p.HandleResults(actions)
	return nil
}

func (p *actionsPaginationContext) Next() error {
	actions, _, err := p.client.GetActions(actionsIndexName, p.pageSize, p.cursorDate, p.cursorID, false)
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
