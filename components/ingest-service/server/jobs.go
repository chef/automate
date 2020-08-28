package server

import (
	"context"
	"fmt"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"

	es "github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/components/ingest-service/config"
	"github.com/chef/automate/components/ingest-service/serveropts"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/datalifecycle/purge"
)

var (
	MissingNodesWorkflowName = cereal.NewWorkflowName("missing_nodes")
	MissingNodesScheduleName = "periodic_missing_nodes"

	DeleteNodesWorkflowName = cereal.NewWorkflowName("delete_nodes")
	DeleteNodesScheduleName = "periodic_delete_nodes"

	MissingNodesForDeletionWorkflowName = cereal.NewWorkflowName("missing_nodes_for_deletion")
	MissingNodesForDeletionScheduleName = "periodic_missing_nodes_for_deletion"
)

func InitializeJobManager(c *cereal.Manager, client backend.Client, esSidecarClient es.EsSidecarServiceClient,
	nodeMgrClient manager.NodeManagerServiceClient, nodesClient nodes.NodesServiceClient) error {
	err := patterns.RegisterSingleTaskWorkflowExecutor(c, DeleteNodesWorkflowName,
		false, &DeleteExpiredMarkedNodesTask{client}, cereal.TaskExecutorOpts{})
	if err != nil {
		return err
	}
	err = patterns.RegisterSingleTaskWorkflowExecutor(c, MissingNodesWorkflowName,
		false, &MarkNodesMissingTask{client, nodeMgrClient}, cereal.TaskExecutorOpts{})
	if err != nil {
		return err
	}
	err = patterns.RegisterSingleTaskWorkflowExecutor(c, MissingNodesForDeletionWorkflowName,
		false, &MarkMissingNodesForDeletionTask{client, nodesClient}, cereal.TaskExecutorOpts{})
	if err != nil {
		return err
	}
	err = purge.ConfigureManager(
		c, PurgeWorkflowName, purge.WithTaskEsSidecarClient(esSidecarClient),
	)
	if err != nil {
		return errors.Wrap(err, "failed to configure purge workflow")
	}

	return nil
}

func MigrateJobsSchedule(ctx context.Context, c *cereal.Manager, oldConfigFile string,
	opts serveropts.JobsConfig) error {
	jc := config.NewOldJobConfig(opts)
	err := jc.FromFile(oldConfigFile)
	if err != nil {
		log.WithError(err).Warn("failed to read old job config from disk, defaults will be used")
	}

	for idx, name := range config.JobList {
		scheduleName := fmt.Sprintf("periodic_%s", name)
		config, err := jc.ConfigForJob(idx)
		if err != nil {
			return err
		}

		d, err := time.ParseDuration(config.Every)
		if err != nil {
			return errors.Wrap(err, "could not parse job configuration")
		}
		r, err := rrule.NewRRule(rrule.ROption{
			Freq:     rrule.SECONDLY,
			Interval: int(d.Seconds()),
			Dtstart:  time.Now(),
		})
		if err != nil {
			return errors.Wrap(err, "could not create recurrence rule for job configuration")
		}

		err = c.CreateWorkflowSchedule(ctx, scheduleName, cereal.NewWorkflowName(name), config.Threshold, config.Running, r)
		if err == cereal.ErrWorkflowScheduleExists {
			log.Infof("Schedule for %s already exists, not migrating", scheduleName)
		} else if err != nil {
			return errors.Wrap(err, "could not create scheduled workflow")
		}
	}
	return nil
}

func workflowNameToInstanceName(workflowName cereal.WorkflowName) string {
	return fmt.Sprintf("periodic_%s", workflowName)
}

type DeleteExpiredMarkedNodesTask struct {
	Client backend.Client
}

func (t *DeleteExpiredMarkedNodesTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var threshold string
	err := task.GetParameters(&threshold)
	if err != nil {
		return nil, errors.Wrap(err, "could not get threshold parameter")
	}

	return nil, deleteExpiredMarkedNodes(ctx, threshold, t.Client)
}

type MarkNodesMissingTask struct {
	Client        backend.Client
	NodeMgrClient manager.NodeManagerServiceClient
}

func (t *MarkNodesMissingTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var threshold string
	err := task.GetParameters(&threshold)
	if err != nil {
		return nil, errors.Wrap(err, "could not get threshold parameter")
	}

	return nil, markNodesMissing(ctx, threshold, t.Client, t.NodeMgrClient)
}

type MarkMissingNodesForDeletionTask struct {
	Client      backend.Client
	NodesClient nodes.NodesServiceClient
}

func (t *MarkMissingNodesForDeletionTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var threshold string
	err := task.GetParameters(&threshold)
	if err != nil {
		return nil, errors.Wrap(err, "could not get threshold parameter")
	}

	return nil, markMissingNodesForDeletion(ctx, threshold, t.Client, t.NodesClient)
}

// markNodesMissing is a job that will call the backend to mark
// all nodes that haven't checked in passed the threshold
func markNodesMissing(ctx context.Context, threshold string, client backend.Client,
	nodeMgrClient manager.NodeManagerServiceClient) error {
	logctx := log.WithFields(log.Fields{
		"threshold": threshold,
		"job":       "MarkMissingNodesForDeletion",
	})

	logctx.Debug("Starting job")

	// mark the nodes missing
	updatedNodeIDs, err := client.MarkNodesMissing(ctx, threshold)
	if err != nil {
		logctx.WithError(err).Error("Job failed")
		return err
	}

	// Walk through each node and update the status in the nodemanager
	for _, nodeID := range updatedNodeIDs {
		_, err = nodeMgrClient.ChangeNodeState(ctx, &manager.NodeState{
			Id:    nodeID,
			State: manager.NodeState_MISSING,
		})
		if err != nil {
			logctx.WithError(err).Error("Job failed")
			return err
		}
	}

	f := log.Fields{"nodes_updated": len(updatedNodeIDs)}
	if len(updatedNodeIDs) > 0 {
		logctx.WithFields(f).Info("Job updated nodes")
	} else {
		logctx.WithFields(f).Debug("Job ran without updates")
	}
	return nil
}

// markMissingNodesForDeletion is a job that will call the backend to mark all missing nodes
// that haven't checked in passed the threshold ready for deletion
func markMissingNodesForDeletion(ctx context.Context, threshold string, client backend.Client,
	nodesClient nodes.NodesServiceClient) error {
	logctx := log.WithFields(log.Fields{
		"threshold": threshold,
		"job":       "MarkMissingNodesForDeletion",
	})

	logctx.Debug("Starting job")

	deletedNodeIDs, err := client.MarkMissingNodesForDeletion(ctx, threshold)
	if err != nil {
		log.WithError(err).Error("Job failed")
		return err
	}

	// Walk through each node and update the status in the nodemanager
	for _, nodeID := range deletedNodeIDs {
		_, err = nodesClient.Delete(ctx, &nodes.Id{Id: nodeID})
		if err != nil {
			logctx.WithError(err).Error("Job failed")
			return err
		}
	}

	f := log.Fields{"nodes_updated": len(deletedNodeIDs)}
	if len(deletedNodeIDs) > 0 {
		logctx.WithFields(f).Info("Job updated nodes")
	} else {
		logctx.WithFields(f).Debug("Job ran without updates")
	}
	return nil
}

// deleteExpiredMarkedNodes is a job that will call the backend to delete all expired
// nodes marked for deletion
func deleteExpiredMarkedNodes(ctx context.Context, threshold string, client backend.Client) error {
	logctx := log.WithFields(log.Fields{
		"job":       "DeleteExpiredMarkedNodes",
		"threshold": threshold,
	})

	logctx.Debug("Starting Job")
	updateCount, err := client.DeleteMarkedNodes(ctx, threshold)
	if err != nil {
		logctx.WithError(err).Error("Job Failed")
		return err
	}

	f := log.Fields{"nodes_deleted": updateCount}
	if updateCount > 0 {
		logctx.WithFields(f).Info("Job deleted nodes")
	} else {
		logctx.WithFields(f).Debug("Job ran without updates")
	}
	return nil
}

func JobSettingsToUpdateOpts(settings *ingest.JobSettings, oldSchedule *cereal.Schedule) ([]cereal.WorkflowScheduleUpdateOpt, bool, error) {
	shouldRunNow := false
	err := settings.Validate()
	if err != nil {
		return nil, shouldRunNow, err
	}

	ret := make([]cereal.WorkflowScheduleUpdateOpt, 0)
	var newRRule *rrule.RRule
	if e := settings.GetEvery(); len(e) > 0 {
		// Convert duration to an rrule
		d, err := time.ParseDuration(e)
		if err != nil {
			// Unlikely as validate already checked this
			return nil, shouldRunNow, err
		}
		oldRecurrence, err := oldSchedule.GetRRule()
		if err != nil {
			return nil, shouldRunNow, errors.Wrap(err, "could not parse existing recurrence rule")
		}

		newRRule, err = rrule.NewRRule(rrule.ROption{
			Freq:     rrule.SECONDLY,
			Interval: int(d.Seconds()),
			Dtstart:  oldRecurrence.OrigOptions.Dtstart,
		})
		if err != nil {
			return nil, shouldRunNow, errors.Wrap(err, "could not construct new recurrence rule")
		}
	} else if recurrence := settings.GetRecurrence(); recurrence != "" {
		newRRule, err = rrule.StrToRRule(recurrence)
		if err != nil {
			return nil, shouldRunNow, errors.Wrap(err, "invalid recurrence rule")
		}
	}
	if newRRule != nil {
		ret = append(ret, cereal.UpdateRecurrence(newRRule))
	}

	if t := settings.GetThreshold(); len(t) > 0 {
		var oldThreshold string
		err := oldSchedule.GetParameters(&oldThreshold)
		if err != nil {
			oldThreshold = ""
		}

		if settings.GetRunning() && oldThreshold != t {
			shouldRunNow = true
		}
		ret = append(ret, cereal.UpdateParameters(t))
	}

	if !oldSchedule.Enabled && settings.GetRunning() {
		shouldRunNow = true
	}

	ret = append(ret, cereal.UpdateEnabled(settings.GetRunning()))

	return ret, shouldRunNow, nil
}
