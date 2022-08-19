package migrations

import (
	"context"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/processor"
	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"time"
)

var (
	MigrationWorkflowName         = cereal.NewWorkflowName("migration-workflow")
	UpgradeTaskName               = cereal.NewTaskName("upgrade-task")
	ControlIndexMigrationTaskName = cereal.NewTaskName("control-index-task")
)

func InitCerealManager(cerealManager *cereal.Manager, workerCount int, client *ingestic.ESClient, upgradesDB *pgdb.UpgradesDB) error {
	logrus.Info("Successfully starting control-workflow")
	err := cerealManager.RegisterWorkflowExecutor(MigrationWorkflowName, &MigrationWorkflow{})
	if err != nil {
		logrus.Info("Found error migration-workflow")
		logrus.Info(err)
		return err
	}

	logrus.Info("Successfully registered migration-workflow")
	return cerealManager.RegisterTaskExecutor(UpgradeTaskName, &UpgradeTask{
		ESClient:   client,
		UpgradesDB: upgradesDB,
	}, cereal.TaskExecutorOpts{Workers: workerCount})

}

type MigrationWorkflow struct {
}

type MigrationWorkflowParameters struct {
	DayLatestFlag    bool
	ControlIndexFlag bool
	CompRunInfoFlag  bool
}

type MigrationWorkflowPayload struct {
	DayLatestFlag    bool
	ControlIndexFlag bool
	CompRunInfoFlag  bool
}

func (s *MigrationWorkflow) OnStart(w cereal.WorkflowInstance,
	ev cereal.StartEvent) cereal.Decision {

	logrus.Debug("In migration-workflow start method")

	workflowParams := MigrationWorkflowParameters{}
	err := w.GetParameters(&workflowParams)
	if err != nil {
		err = errors.Wrap(err, "failed to unmarshal migration-workflow parameters")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Debugf("In On Start Method %t", workflowParams.DayLatestFlag)

	workflowPayload := MigrationWorkflowPayload{
		DayLatestFlag:    workflowParams.DayLatestFlag,
		ControlIndexFlag: workflowParams.ControlIndexFlag,
		CompRunInfoFlag:  workflowParams.CompRunInfoFlag,
	}

	err = w.EnqueueTask(UpgradeTaskName, UpgradeParameters{
		DayLatestFlag:   workflowParams.DayLatestFlag,
		ControlFlag:     workflowParams.ControlIndexFlag,
		CompRunInfoFlag: workflowParams.CompRunInfoFlag,
	})
	if err != nil {
		err = errors.Wrap(err, "failed to enqueue the migration-task")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}
	return w.Continue(&workflowPayload)
}

func (s *MigrationWorkflow) OnTaskComplete(w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {

	var payload MigrationWorkflowPayload
	if err := w.GetPayload(&payload); err != nil {
		err = errors.Wrap(err, "failed to unmarshal migration-workflow payload")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Debugf("Entered MigrationWorkflow > OnTaskComplete with payload %+v", payload)

	//get the results returned by task run
	var dayLatestParameters UpgradeParameters
	err := ev.Result.Get(&dayLatestParameters)
	if err != nil {
		err = errors.Wrap(err, "failed to get the task run result in OnTaskComplete")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	return w.Complete()
}

func (s *MigrationWorkflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	logrus.Debugf("ReportWorkflow got OnCancel")
	return w.Complete()
}

type UpgradeTask struct {
	ESClient   *ingestic.ESClient
	UpgradesDB *pgdb.UpgradesDB
}

type UpgradeParameters struct {
	DayLatestFlag   bool
	ControlFlag     bool
	CompRunInfoFlag bool
}

func (t *UpgradeTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var job UpgradeParameters
	if err := task.GetParameters(&job); err != nil {
		err = errors.Wrap(err, "could not unmarshal GenerateReportParameters")
		logrus.WithError(err).Error()
		return nil, err
	}

	logrus.Debug("Inside the upgrades flag flow")
	if job.ControlFlag {
		if err := performActionForUpgrade(ctx, t.ESClient); err != nil {
			logrus.WithError(err).Error("Unable to upgrade control index flag for latest record ")
		}

		err := t.UpgradesDB.UpdateCompRunInfoFlagToFalse()
		if err != nil {
			logrus.Warnf("Unable to set upgrades flag in database")
		}
	}

	return &job, nil
}

type ControlIndexUpgradeTask struct {
	ESClient   *ingestic.ESClient
	UpgradesDB *pgdb.UpgradesDB
}

func performActionForUpgrade(ctx context.Context, esClient *ingestic.ESClient) error {
	mapping := mappings.ComplianceRepDate
	time90DaysAgo := time.Now().Add(-24 * time.Hour * 90)
	reportsMap, latestReportsMap, err := esClient.GetReportsDailyLatestTrue(ctx, time90DaysAgo)
	if err != nil {
		logrus.Errorf("Unable to Get Report IDs where daily latest true with err %v", err)
		return err
	}

	if len(reportsMap) > 0 {
		for report, endTime := range reportsMap {

			parsedEndTime, _ := time.Parse(time.RFC3339, endTime)
			index := mapping.IndexTimeseriesFmt(parsedEndTime)

			inspecReport, err := esClient.GetDocByReportUUId(context.Background(), report, index)
			if err != nil {
				logrus.Errorf("Unable to fetch report for the reportuuid %s", report)
			}

			if _, found := latestReportsMap[report]; found {
				//Updating the comp run info Flag
				err := esClient.InsertComplianceRunInfo(ctx, inspecReport, inspecReport.EndTime)
				if err != nil {
					logrus.Errorf("Unable to perform action compliance run info for node %s with error %v", inspecReport.NodeID, err)
				}

				//Updating Day Latest Flag
				err = esClient.SetNodesDayLatestFalseForUpgrade(ctx, inspecReport.NodeID, inspecReport.EndTime)
				if err != nil {
					logrus.Errorf("Unable to perform action for day latest flag for node %s with error %v", inspecReport.NodeID, err)
				}

			}
			//Updating controls Index structure
			controls, err := processor.MapStructsESInSpecReportToControls(inspecReport)
			if err != nil {
				logrus.Errorf("Unable to parse the structure from reportuuid to controls with reportuuid:%s", report)
				continue
			}

			logrus.Debugf("Parsed results got results")
			err = esClient.UploadDataToControlIndex(ctx, report, controls, parsedEndTime)
			if err != nil {
				logrus.Errorf("Unable to add data to index with reportuuid:%s", report)
			}

		}
	}

	logrus.Info("Successfully completed the upgrade all the indexes")

	return nil

}
