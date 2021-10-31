package worker

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	"github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/report-manager-service/objstore"
	"github.com/chef/automate/components/report-manager-service/storage"
	"github.com/chef/automate/components/report-manager-service/utils"
	"github.com/chef/automate/lib/cereal"
	"github.com/minio/minio-go/v7"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

var (
	ReportWorkflowName = cereal.NewWorkflowName("report-workflow")
	ReportTaskName     = cereal.NewTaskName("report-task")
)

const (
	RunningStatus string = "running"
	FailedStatus  string = "failed"
	SuccessStatus string = "success"
)

func InitCerealManager(cerealManager *cereal.Manager, workerCount int, db *storage.DB, objStoreClient *minio.Client,
	objBucket string) error {
	err := cerealManager.RegisterWorkflowExecutor(ReportWorkflowName, &ReportWorkflow{
		DB: db,
	})
	if err != nil {
		return err
	}

	return cerealManager.RegisterTaskExecutor(ReportTaskName, &GenerateReportTask{
		ObjStoreClient: objstore.ReportManagerObjStore{
			ObjStoreClient: objStoreClient,
		},
		ObjBucketName: objBucket,
	}, cereal.TaskExecutorOpts{Workers: workerCount})
}

type ReportWorkflow struct {
	DB *storage.DB
}

type ReportWorkflowParameters struct {
	JobID            string
	RequestorID      string
	Retries          int
	RequestToProcess *report_manager.CustomReportRequest
}

type ReportWorkflowPayload struct {
	JobID       string
	Status      string
	RetriesLeft int
	StartTime   *time.Time
}

func (s *ReportWorkflow) OnStart(w cereal.WorkflowInstance,
	ev cereal.StartEvent) cereal.Decision {

	startTime := time.Now()
	workflowPayload := ReportWorkflowPayload{
		StartTime: &startTime,
	}

	workflowParams := ReportWorkflowParameters{}
	err := w.GetParameters(&workflowParams)
	if err != nil {
		err = errors.Wrap(err, "failed to unmarshal report-workflow parameters")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Infof("In On Start Method %s", workflowParams.JobID)

	workflowPayload.JobID = workflowParams.JobID
	workflowPayload.RetriesLeft = workflowParams.Retries
	workflowPayload.Status = RunningStatus

	err = w.EnqueueTask(ReportTaskName, GenerateReportParameters{
		JobID:            workflowParams.JobID,
		RequestToProcess: workflowParams.RequestToProcess,
	})
	if err != nil {
		err = errors.Wrap(err, "failed to enqueue the report-task")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Infof("Inserting task %s into DB", workflowParams.JobID)
	err = s.DB.InsertTask(workflowParams.JobID, workflowParams.RequestorID, RunningStatus, startTime, startTime)
	if err != nil {
		err = errors.Wrap(err, "failed to insert the record in postgres")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	return w.Continue(&workflowPayload)
}

func (s *ReportWorkflow) OnTaskComplete(w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {

	var payload ReportWorkflowPayload

	if err := w.GetPayload(&payload); err != nil {
		err = errors.Wrap(err, "failed to unmarshal report-workflow payload")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Infof("Entered ReportWorkflow > OnTaskComplete with payload %+v", payload)

	if err := ev.Result.Err(); err != nil {
		//received error, if the retries are available enqueue the task
		if payload.RetriesLeft > 0 {
			logrus.Debugf("retring report-task %s", payload.JobID)

			workflowParams := ReportWorkflowParameters{}
			err := w.GetParameters(&workflowParams)
			if err != nil {
				err = errors.Wrap(err, "failed to unmarshal report-workflow parameters")
				logrus.WithError(err).Error()
				return w.Fail(err)
			}

			err = w.EnqueueTask(ReportTaskName, GenerateReportParameters{
				JobID:            payload.JobID,
				RequestToProcess: workflowParams.RequestToProcess,
			})
			if err != nil {
				err = errors.Wrap(err, "failed to enqueue the report-task")
				logrus.WithError(err).Error()
				return w.Fail(err)
			}

			payload.RetriesLeft--
			return w.Continue(&payload)
		}
		//if there are no retries left, update the failed status to db
		dbErr := s.DB.UpdateTask(payload.JobID, FailedStatus, err.Error(), time.Now())
		if dbErr != nil {
			dbErr = errors.Wrap(dbErr, "failed to update the record in postgres")
			logrus.WithError(dbErr).Error()
			return w.Fail(dbErr)
		}

		err = errors.Wrap(err, "failed to run report-task")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}
	//update the finished status to db
	err := s.DB.UpdateTask(payload.JobID, SuccessStatus, "", time.Now())
	if err != nil {
		err = errors.Wrap(err, "failed to update the record in postgres")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Infof("successfully completed the task %s", payload.JobID)
	return w.Complete()
}

func (s *ReportWorkflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	logrus.Debugf("ReportWorkflow got OnCancel")
	return w.Complete()
}

type GenerateReportTask struct {
	ObjStoreClient objstore.ObjectStore
	ObjBucketName  string
}

type GenerateReportParameters struct {
	JobID            string
	StartTime        *time.Time
	EndTime          *time.Time
	RequestToProcess *report_manager.CustomReportRequest
}

func (t *GenerateReportTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var job GenerateReportParameters
	if err := task.GetParameters(&job); err != nil {
		err = errors.Wrap(err, "could not unmarshal GenerateReportParameters")
		logrus.WithError(err).Error()
		return nil, err
	}

	logrus.Infof("In TaskRun working on job %s", job.JobID)
	logrus.Infof("%v\n", job)

	startTime := time.Now().UTC().Round(time.Second)
	job.StartTime = &startTime

	for _, report := range job.RequestToProcess.Reports {
		controlsMap := make(map[string]map[string]*inspec.Control)
		profilesMap := make(map[string]*inspec.Profile)
		objectName := utils.GetObjName(report.GetReportId())
		bytes, err := t.ObjStoreClient.GetObject(ctx, t.ObjBucketName, objectName, minio.GetObjectOptions{})
		if err != nil {
			err = errors.Wrap(err, "could not get the file from object store")
			logrus.WithError(err).Error()
			return nil, err
		}
		cmpReport := compliance.Report{}
		err = json.Unmarshal(*bytes, &cmpReport)
		if err != nil {
			err = errors.Wrap(err, "error in unmarshalling the report content")
			logrus.WithError(err).Error()
			return nil, err
		}

		//populate controlsMap and ProfileMap
		for _, profile := range cmpReport.Profiles {
			profilesMap[profile.Sha256] = profile
			controlMap := make(map[string]*inspec.Control)
			for _, control := range profile.Controls {
				controlMap[control.Id] = control
			}
			controlsMap[profile.Sha256] = controlMap
		}

		//prepare json or csv reports
		fmt.Println(cmpReport.ReportUuid)
	}

	endTime := time.Now().UTC().Round(time.Second)
	job.EndTime = &endTime

	return &job, nil
}
