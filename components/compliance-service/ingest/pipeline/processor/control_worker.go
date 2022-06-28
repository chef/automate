package processor

import (
	"context"
	"fmt"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/lib/cereal"
	elastic "github.com/olivere/elastic/v7"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"time"
)

var (
	ReportWorkflowName = cereal.NewWorkflowName("control-workflow")
	ReportTaskName     = cereal.NewTaskName("control-task")
)

const (
	RunningStatus string = "running"
	FailedStatus  string = "failed"
	SuccessStatus string = "success"
)

func InitCerealManager(cerealManager *cereal.Manager, workerCount int, client *ingestic.ESClient) error {
	logrus.Info("Successfully starting control-workflow")
	err := cerealManager.RegisterWorkflowExecutor(ReportWorkflowName, &ControlWorkflow{})
	if err != nil {
		logrus.Info("Found error control-workflow")
		logrus.Info(err)
		return err
	}

	logrus.Info("Successfully registered control-workflow")
	return cerealManager.RegisterTaskExecutor(ReportTaskName, &GenerateControlTask{
		ESClient: client,
	}, cereal.TaskExecutorOpts{Workers: workerCount})

}

type ControlWorkflow struct {
}

type ControlWorkflowParameters struct {
	ReportUuid string
	Retries    int
	EndTime    time.Time
}

type ControlWorkflowPayload struct {
	ReportUuid  string
	RetriesLeft int
	Status      string
	EndTime     time.Time
}

func (s *ControlWorkflow) OnStart(w cereal.WorkflowInstance,
	ev cereal.StartEvent) cereal.Decision {

	logrus.Debug("In control-workflow start method")

	workflowPayload := ControlWorkflowPayload{}

	workflowParams := ControlWorkflowParameters{}
	err := w.GetParameters(&workflowParams)
	if err != nil {
		err = errors.Wrap(err, "failed to unmarshal control-workflow parameters")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Debugf("In On Start Method %s", workflowParams.ReportUuid)

	workflowPayload.ReportUuid = workflowParams.ReportUuid
	workflowPayload.RetriesLeft = workflowParams.Retries
	workflowPayload.EndTime = workflowParams.EndTime
	workflowPayload.Status = RunningStatus

	err = w.EnqueueTask(ReportTaskName, GenerateControlParameters{
		ReportUuid: workflowParams.ReportUuid,
		EndTime:    workflowParams.EndTime,
	})
	if err != nil {
		err = errors.Wrap(err, "failed to enqueue the control-task")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}
	return w.Continue(&workflowPayload)
}

func (s *ControlWorkflow) OnTaskComplete(w cereal.WorkflowInstance,
	ev cereal.TaskCompleteEvent) cereal.Decision {

	var payload ControlWorkflowPayload

	if err := w.GetPayload(&payload); err != nil {
		err = errors.Wrap(err, "failed to unmarshal control-workflow payload")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	logrus.Debugf("Entered ControlWorkflow > OnTaskComplete with payload %+v", payload)

	if err := ev.Result.Err(); err != nil {
		//received error, if the retries are available enqueue the task
		if payload.RetriesLeft > 0 {
			logrus.Debugf("retring control-task %s", payload.ReportUuid)
			payload.RetriesLeft--
			workflowParams := ControlWorkflowParameters{}
			err := w.GetParameters(&workflowParams)
			if err != nil {
				err = errors.Wrap(err, "failed to unmarshal control-workflow parameters")
				logrus.WithError(err).Error()
				return w.Fail(err)
			}

			err = w.EnqueueTask(ReportTaskName, GenerateControlParameters{
				ReportUuid: payload.ReportUuid,
			})
			if err != nil {
				err = errors.Wrap(err, "failed to enqueue the control-task")
				logrus.WithError(err).Error()
				return w.Fail(err)
			}
			return w.Continue(&payload)
		}
		err = errors.Wrap(err, "failed to run control-task")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}

	//get the results returned by task run
	var controlResult GenerateControlParameters
	err := ev.Result.Get(&controlResult)
	if err != nil {
		err = errors.Wrap(err, "failed to get the task run result in OnTaskComplete")
		logrus.WithError(err).Error()
		return w.Fail(err)
	}
	logrus.Infof("successfully completed the task %s", payload.ReportUuid)
	return w.Complete()
}

func (s *ControlWorkflow) OnCancel(w cereal.WorkflowInstance, ev cereal.CancelEvent) cereal.Decision {
	logrus.Debugf("ReportWorkflow got OnCancel")
	return w.Complete()
}

type GenerateControlTask struct {
	ESClient *ingestic.ESClient
	Esr      relaxting.ES2Backend
}

type GenerateControlParameters struct {
	ReportUuid string
	EndTime    time.Time
}

func (t *GenerateControlTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {

	var job GenerateControlParameters
	if err := task.GetParameters(&job); err != nil {
		err = errors.Wrap(err, "could not unmarshal GenerateReportParameters")
		logrus.WithError(err).Error()
		return nil, err
	}

	logrus.Infof("In TaskRun working on job %s", job.ReportUuid)

	//startTime := time.Now().UTC().Round(time.Second)

	//time taking to commit to records to ES
	time.Sleep(60 * time.Second)
	mapping := mappings.ComplianceRepDate
	index := mapping.IndexTimeseriesFmt(job.EndTime)

	controls, err := ParseReportCtrlStruct(ctx, t.ESClient, job.ReportUuid, index)
	if err != nil {
		logrus.Errorf("Unable to parse the structure from reportuuid to controls with reportuuid:%s", job.ReportUuid)
		return nil, err
	}

	err = UploadDataToControlIndex(ctx, job.ReportUuid, controls, t.ESClient, job.EndTime)

	return &job, nil
}

func UploadDataToControlIndex(ctx context.Context, reportuuid string, controls []Control, client *ingestic.ESClient, endTime time.Time) error {
	mapping := mappings.ComplianceControlRepData
	index := mapping.IndexTimeseriesFmt(endTime)

	bulkRequest := client.Client.Bulk()
	for _, control := range controls {
		docId := GetDocIdByControlIdAndProfileID(control.ControlID, control.Profile.ProfileID)
		found, err := CheckIfControlIdExistsForToday(docId, index, client)
		if err != nil {
			logrus.Errorf("Unable to fetch document for control id %s|%s", control.ControlID, control.Profile.ProfileID)
		}
		if found {
			bulkRequest = bulkRequest.Add(elastic.NewBulkUpdateRequest().Index(index).Id(docId).Script(createScriptForAddingNode(control.Nodes[0])).Type("_doc"))
		} else {
			bulkRequest = bulkRequest.Add(elastic.NewBulkIndexRequest().Index(index).Id(docId).Doc(control).Type("_doc"))
		}

	}

	approxBytes := bulkRequest.EstimatedSizeInBytes()
	bulkResponse, err := bulkRequest.Refresh("false").Do(ctx)
	if err != nil {
		logrus.Errorf("Unable to send the request in bulk for reportuuid :%s with error :%v", reportuuid, err)
		return err
	}
	if bulkResponse == nil {
		logrus.Errorf("Unable to fetch the response of bulk request reportuuid id:%s with error :%v", reportuuid, err)
		return err
	}

	logrus.Debugf("Bulk insert %d summaries, ~size %dB, took %dms", len(controls), approxBytes, bulkResponse.Took)
	return nil

}

func GetDocIdByControlIdAndProfileID(controlID string, profileID string) string {
	return fmt.Sprintf("%s|%s", controlID, profileID)
}

func createScriptForAddingNode(node Node) *elastic.Script {
	params := make(map[string]interface{})
	params["node"] = node

	return elastic.NewScript("if (!(ctx._source.nodes instanceof Collection)) {ctx._source.nodes = [ctx._source.nodes];} ctx._source.nodes.add(params.node)").Params(params)

}

func CheckIfControlIdExistsForToday(docId string, indexToday string, esClient *ingestic.ESClient) (bool, error) {
	logrus.Debugf("Checking the control document exists for today with doc Id :%s", docId)
	fsc := elastic.NewFetchSourceContext(false)
	boolQuery := elastic.NewBoolQuery()
	idsQuery := elastic.NewIdsQuery()
	idsQuery.Ids(docId)
	boolQuery = boolQuery.Must(idsQuery)
	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(boolQuery).
		Size(1)
	searchResult, err := esClient.Client.Search().
		SearchSource(searchSource).
		Index(indexToday).
		Do(context.Background())
	if err != nil {
		switch {
		case elastic.IsTimeout(err):
			logrus.Errorf("Timeout retrieving document: %v", err)
			return false, err
		default:
			logrus.Errorf("Received error: %v", err)
			return false, err
		}
	}

	if searchResult.TotalHits() > 0 {
		// Iterate through results
		for _, hit := range searchResult.Hits.Hits {
			// hit.Index contains the id of the index
			if len(hit.Id) > 0 {
				logrus.Debugf("Found the document with for control with doc Id %s", docId)
				return true, nil
			}

		}
	}
	return false, nil

}
