package worker

import (
	"bytes"
	"context"
	"encoding/json"
	"time"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	"github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	"github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/chef/automate/api/interservice/report_manager"
	"github.com/chef/automate/components/report-manager-service/objstore"
	"github.com/chef/automate/components/report-manager-service/storage"
	"github.com/chef/automate/components/report-manager-service/utils"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/stringutils"
	"github.com/golang/protobuf/ptypes"
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
		//if there are no retries left, update the failed status to db and set the object size as 0
		dbErr := s.DB.UpdateTask(payload.JobID, FailedStatus, err.Error(), time.Now(), 0)
		if dbErr != nil {
			dbErr = errors.Wrap(dbErr, "failed to update the record in postgres")
			logrus.WithError(dbErr).Error()
			return w.Fail(dbErr)
		}

		err = errors.Wrap(err, "failed to run report-task")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	//get the results returned by task run
	var jobResult GenerateReportParameters
	err := ev.Result.Get(&jobResult)
	if err != nil {
		err = errors.Wrap(err, "failed to get the task run result in OnTaskComplete")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	//update the finished status to db
	err = s.DB.UpdateTask(payload.JobID, SuccessStatus, "", time.Now(), jobResult.ReportSize)
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
	ReportSize       int64
}

func (t *GenerateReportTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	var job GenerateReportParameters
	if err := task.GetParameters(&job); err != nil {
		err = errors.Wrap(err, "could not unmarshal GenerateReportParameters")
		logrus.WithError(err).Error()
		return nil, err
	}

	logrus.Infof("In TaskRun working on job %s", job.JobID)

	startTime := time.Now().UTC().Round(time.Second)
	job.StartTime = &startTime

	finalData, err := t.prepareCustomReportData(ctx, job.RequestToProcess)
	if err != nil {
		return nil, err
	}

	//check the existence of bucket
	isBucketExist, err := t.ObjStoreClient.BucketExists(ctx, job.RequestToProcess.RequestorId)
	if err != nil {
		err = errors.Wrap(err, "error in checking the existence of bucket in objectstore")
		logrus.WithError(err).Error()
		return nil, err
	}
	if !isBucketExist {
		err := t.ObjStoreClient.MakeBucket(ctx, job.RequestToProcess.RequestorId, minio.MakeBucketOptions{})
		if err != nil {
			err = errors.Wrap(err, "error in creating a bucket in objectstore")
			logrus.WithError(err).Error()
			return nil, err
		}
	}

	var reportSize int64
	if job.RequestToProcess.ReportType == "json" {
		reportSize, err = t.jsonExporter(ctx, finalData, job.JobID, job.RequestToProcess.RequestorId)
		if err != nil {
			return nil, err
		}
	} else if job.RequestToProcess.ReportType == "csv" {
		/*reportSize, err = t.csvExporter(ctx, finalData, job.JobID, job.RequestToProcess.RequestorId)
		if err != nil {
			return nil, err
		}*/
	}

	endTime := time.Now().UTC().Round(time.Second)
	job.EndTime = &endTime
	job.ReportSize = reportSize

	return &job, nil
}

func (t *GenerateReportTask) prepareCustomReportData(ctx context.Context, reqToProcess *report_manager.CustomReportRequest) ([]*reporting.Report, error) {
	finalData := []*reporting.Report{}
	for _, report := range reqToProcess.Reports {
		controlsMap := make(map[string]map[string]*inspec.Control)
		profilesMap := make(map[string]*inspec.Profile)
		objectName := utils.GetObjName(report.GetReportId())
		objReader, err := t.ObjStoreClient.GetObject(ctx, t.ObjBucketName, objectName, minio.GetObjectOptions{})
		if err != nil {
			err = errors.Wrap(err, "could not get the file from object store")
			logrus.WithError(err).Error()
			return nil, err
		}
		cmpReport := compliance.Report{}
		d := json.NewDecoder(objReader)
		err = d.Decode(&cmpReport)
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

		reportData := &reporting.Report{
			Id:            cmpReport.ReportUuid,
			NodeId:        cmpReport.NodeUuid,
			NodeName:      cmpReport.NodeName,
			Status:        cmpReport.Status,
			Environment:   cmpReport.Environment,
			Version:       cmpReport.Version,
			JobId:         cmpReport.JobUuid,
			Ipaddress:     cmpReport.Ipaddress,
			Fqdn:          cmpReport.Fqdn,
			Roles:         cmpReport.Roles,
			ChefTags:      cmpReport.ChefTags,
			StatusMessage: cmpReport.StatusMessage,
		}
		if cmpReport.Platform != nil && (cmpReport.Platform.Name != "" || cmpReport.Platform.Release != "") {
			reportData.Platform = &reporting.Platform{
				Name:    cmpReport.Platform.Name,
				Release: cmpReport.Platform.Release,
				Full:    stringutils.GetFullPlatformName(cmpReport.Platform.Name, cmpReport.Platform.Release),
			}
		}
		if cmpReport.Statistics != nil {
			reportData.Statistics = &reporting.Statistics{
				Duration: cmpReport.Statistics.Duration,
			}
		}
		if cmpReport.EndTime != "" {
			endTime, err := time.Parse(time.RFC3339, cmpReport.EndTime)
			if err != nil {
				err = errors.Wrap(err, "error in converting the end time content")
				logrus.WithError(err).Error()
				return nil, err
			}
			endTimeProto, err := ptypes.TimestampProto(endTime)
			if err != nil {
				err = errors.Wrap(err, "error in converting the end time content to proto format")
				logrus.WithError(err).Error()
				return nil, err
			}
			reportData.EndTime = endTimeProto
		}
		for _, profile := range report.Profiles {
			compProfile, ok := profilesMap[profile.ProfileId]
			if !ok {
				continue
			}
			//TODO:: Fix Profile status
			profileData := &reporting.Profile{
				Name:           compProfile.Name,
				Title:          compProfile.Title,
				Copyright:      compProfile.Copyright,
				CopyrightEmail: compProfile.CopyrightEmail,
				Summary:        compProfile.Summary,
				Version:        compProfile.Version,
				Full:           stringutils.GetFullProfileName(compProfile.Title, compProfile.Version),
				Sha256:         compProfile.Sha256,
				Status:         compProfile.Status,
				SkipMessage:    compProfile.SkipMessage,
				StatusMessage:  compProfile.StatusMessage,
			}
			for _, depend := range compProfile.Depends {
				profileData.Depends = append(profileData.Depends, &reporting.Dependency{
					Name:        depend.Name,
					Url:         depend.Url,
					Path:        depend.Path,
					Git:         depend.Git,
					Branch:      depend.Branch,
					Tag:         depend.Tag,
					Commit:      depend.Commit,
					Version:     depend.Version,
					Supermarket: depend.Supermarket,
					Compliance:  depend.Compliance,
					Status:      depend.Status,
					SkipMessage: depend.SkipMessage,
				})
			}
			controlMap := controlsMap[profile.ProfileId]
			for _, control := range profile.Controls {
				compControl, ok := controlMap[control]
				if !ok {
					continue
				}
				//TODO: Add Tags and waived_str at control level
				controlData := &reporting.Control{
					Id:     compControl.Id,
					Code:   compControl.Code,
					Desc:   compControl.Desc,
					Impact: compControl.Impact,
					Title:  compControl.Title,
				}
				if compControl.SourceLocation != nil && (compControl.SourceLocation.Ref != "" ||
					compControl.SourceLocation.Line != 0) {
					controlData.SourceLocation = &reporting.SourceLocation{
						Ref:  compControl.SourceLocation.Ref,
						Line: compControl.SourceLocation.Line,
					}
				}
				if compControl.WaiverData != nil && (compControl.WaiverData.ExpirationDate != "" ||
					compControl.WaiverData.Justification != "" || compControl.WaiverData.Run ||
					compControl.WaiverData.SkippedDueToWaiver || compControl.WaiverData.Message != "") {
					controlData.WaiverData = &reporting.OrigWaiverData{
						ExpirationDate:     compControl.WaiverData.ExpirationDate,
						Justification:      compControl.WaiverData.Justification,
						Run:                compControl.WaiverData.Run,
						SkippedDueToWaiver: compControl.WaiverData.SkippedDueToWaiver,
						Message:            compControl.WaiverData.Message,
					}
				}
				if compControl.RemovedResultsCounts != nil && (compControl.RemovedResultsCounts.Failed != 0 ||
					compControl.RemovedResultsCounts.Skipped != 0 || compControl.RemovedResultsCounts.Passed != 0) {
					controlData.RemovedResultsCounts = &reporting.RemovedResultsCounts{
						Failed:  compControl.RemovedResultsCounts.Failed,
						Skipped: compControl.RemovedResultsCounts.Skipped,
						Passed:  compControl.RemovedResultsCounts.Passed,
					}
				}
				for _, result := range compControl.Results {
					controlData.Results = append(controlData.Results, &reporting.Result{
						Status:      result.Status,
						CodeDesc:    result.CodeDesc,
						RunTime:     result.RunTime,
						Message:     result.Message,
						SkipMessage: result.SkipMessage,
					})
				}
				profileData.Controls = append(profileData.Controls, controlData)
			}
			reportData.Profiles = append(reportData.Profiles, profileData)
		}
		finalData = append(finalData, reportData)
	}
	return finalData, nil
}

func (t *GenerateReportTask) jsonExporter(ctx context.Context, finalData []*reporting.Report, jobID,
	requestorID string) (int64, error) {
	buf := new(bytes.Buffer)
	err := json.NewEncoder(buf).Encode(finalData)
	if err != nil {
		err = errors.Wrap(err, "error in encoding the final data")
		logrus.WithError(err).Error()
		return 0, err
	}

	objectName := utils.GetObjName(jobID)
	info, err := t.ObjStoreClient.PutObject(ctx, requestorID, objectName, buf, -1, minio.PutObjectOptions{})
	if err != nil {
		err = errors.Wrap(err, "error in storing the data object to objectstore")
		logrus.WithError(err).Error()
		return 0, err
	}
	return info.Size, nil
}
