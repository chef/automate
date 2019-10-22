package service

import (
	"bytes"
	"compress/gzip"
	"context"
	"encoding/json"
	"net/http"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"

	secrets "github.com/chef/automate/api/external/secrets"
	cfgmgmtResponse "github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/components/data-feed-service/dao"
	"github.com/chef/automate/lib/cereal"
	grpccereal "github.com/chef/automate/lib/cereal/grpc"

	//"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/grpc/secureconn"
)

type datafeedNotification struct {
	username string
	password string
	url      string
	data     []datafeedMessage
}

type datafeedMessage struct {
	Automatic map[string]interface{} `json:"automatic"`
	LastRun   *cfgmgmtResponse.Run   `json:"last_run"`
	Report    *reporting.Report      `json:"report"`
}

type DataClient struct {
	client http.Client
}

func NewDataClient() DataClient {
	return DataClient{
		client: http.Client{},
	}
}

type NotificationSender interface {
	sendNotification(notification datafeedNotification) error
}

func Start(dataFeedConfig *config.DataFeedConfig, connFactory *secureconn.Factory, db *dao.DB) error {
	log.Info("Starting data-feed-service")
	conn, err := connFactory.Dial("cereal-service", dataFeedConfig.CerealConfig.Target)
	if err != nil {
		return err
	}

	backend := grpccereal.NewGrpcBackendFromConn("data-feed-service", conn)
	manager, err := cereal.NewManager(backend)
	if err != nil {
		return err
	}

	dataFeedPollTask, err := NewDataFeedPollTask(dataFeedConfig, connFactory, db, manager)
	if err != nil {
		return err
	}

	dataFeedClientTask, err := NewDataFeedClientTask(dataFeedConfig, connFactory)
	if err != nil {
		return err
	}

	dataFeedComplianceTask, err := NewDataFeedComplianceTask(dataFeedConfig, connFactory)
	if err != nil {
		return err
	}
	dataFeedNotifierTask, err := NewDataFeedNotifierTask(dataFeedConfig, connFactory, db)
	if err != nil {
		return err
	}

	err = manager.RegisterWorkflowExecutor(dataFeedWorkflowName, &DataFeedWorkflowExecutor{workflowName: dataFeedWorkflowName})
	if err != nil {
		return err
	}
	err = manager.RegisterTaskExecutor(dataFeedPollTaskName, dataFeedPollTask, cereal.TaskExecutorOpts{
		Workers: 1,
	})
	if err != nil {
		return err
	}
	err = manager.RegisterTaskExecutor(dataFeedClientTaskName, dataFeedClientTask, cereal.TaskExecutorOpts{
		Workers: 1,
	})
	if err != nil {
		return err
	}
	err = manager.RegisterTaskExecutor(dataFeedComplianceTaskName, dataFeedComplianceTask, cereal.TaskExecutorOpts{
		Workers: 1,
	})
	err = manager.RegisterTaskExecutor(dataFeedNotifierTaskName, dataFeedNotifierTask, cereal.TaskExecutorOpts{
		Workers: 1,
	})
	if err != nil {
		return err
	}

	r, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: int(dataFeedConfig.ServiceConfig.FeedInterval.Seconds()),
		Dtstart:  time.Now().Round(dataFeedConfig.ServiceConfig.FeedInterval),
	})
	if err != nil {
		return err
	}

	var zeroTime time.Time
	dataFeedTaskParams := DataFeedPollTaskParams{
		FeedInterval:    dataFeedConfig.ServiceConfig.FeedInterval,
		AssetPageSize:   dataFeedConfig.ServiceConfig.AssetPageSize,
		ReportsPageSize: dataFeedConfig.ServiceConfig.ReportsPageSize,
		NextFeedStart:   zeroTime,
		NextFeedEnd:     zeroTime,
	}
	dataFeedClientTaskParams := DataFeedClientTaskParams{NodeIDs: []string{}}
	dataFeedComplianceTaskParams := DataFeedComplianceTaskParams{}
	log.Infof("dataFeedComplianceTaskParams %v", dataFeedComplianceTaskParams)
	dataFeedWorkflowParams := DataFeedWorkflowParams{PollTaskParams: dataFeedTaskParams, ClientTaskParams: dataFeedClientTaskParams, ComplianceTaskParams: dataFeedComplianceTaskParams}

	err = manager.CreateWorkflowSchedule(context.Background(),
		dataFeedScheduleName, dataFeedWorkflowName,
		dataFeedWorkflowParams, true, r)
	if err == cereal.ErrWorkflowScheduleExists {
		// TODO(ssd) 2019-09-09: Right now this resets Dtstart on the recurrence, do we want that?
		err = manager.UpdateWorkflowScheduleByName(context.Background(), dataFeedScheduleName, dataFeedWorkflowName,
			cereal.UpdateParameters(dataFeedWorkflowParams),
			cereal.UpdateEnabled(true),
			cereal.UpdateRecurrence(r))
		if err != nil {
			return err
		}
	} else if err != nil {
		return err
	}

	return manager.Start(context.Background())
}

func handleSendErr(notification datafeedNotification, startTime time.Time, endTime time.Time, err error) {
	log.Errorf("Failed to send notification to %v. Start: %v, End: %v. %v", notification.url, startTime, endTime, err)
	// TODO report this failure to the UI with the time window and notification details
}

func GetCredentials(ctx context.Context, client secrets.SecretsServiceClient, secretID string) (string, string, error) {
	secret, err := client.Read(ctx, &secrets.Id{Id: secretID})
	if err != nil {
		return "", "", err
	}

	username := ""
	password := ""
	data := secret.GetData()
	for kv := range data {
		if data[kv].Key == "username" {
			username = data[kv].Value
		} else if data[kv].Key == "password" {
			password = data[kv].Value
		}
	}

	return username, password, nil
}

func send(sender NotificationSender, notification datafeedNotification) error {
	return sender.sendNotification(notification)
}

func (client DataClient) sendNotification(notification datafeedNotification) error {

	messageBytes, err := json.Marshal(notification.data)
	if err != nil {
		log.Errorf("Error creating json bytes %v", err)
		return err
	}
	log.Debugf("sendNotification bytes length %v", len(messageBytes))

	var contentBuffer bytes.Buffer
	zip := gzip.NewWriter(&contentBuffer)
	_, err = zip.Write(messageBytes)
	if err != nil {
		return err
	}
	err = zip.Close()
	if err != nil {
		return err
	}

	request, err := http.NewRequest("POST", notification.url, &contentBuffer)
	if err != nil {
		log.Error("Error creating request")
		return err
	}
	request.SetBasicAuth(notification.username, notification.password)
	request.Header.Add("Content-Type", "application/json")
	request.Header.Add("Content-Encoding", "gzip")
	request.Header.Add("Accept", "application/json")
	log.Debugf("request %v", request)

	response, err := client.client.Do(request)
	if err != nil {
		log.Errorf("Error sending test message %v", err)
		return err
	} else {
		log.Infof("Asset data posted to %v, Status %v", notification.url, response.Status)
	}
	if response.StatusCode != http.StatusCreated {
		return errors.New(response.Status)
	}
	err = response.Body.Close()
	if err != nil {
		log.Warnf("Error clsoing response body %v", err)
	}
	return nil
}
