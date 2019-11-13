package service

import (
	"bytes"
	"compress/gzip"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"strings"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	cfgmgmtRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	rrule "github.com/teambition/rrule-go"

	secrets "github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/components/data-feed-service/dao"
	"github.com/chef/automate/lib/cereal"
	grpccereal "github.com/chef/automate/lib/cereal/grpc"

	"github.com/chef/automate/lib/grpc/secureconn"
)

type datafeedNotification struct {
	username string
	password string
	url      string
	data     []interface{}
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

	dataFeedListReportsTask, err := NewDataFeedListReportsTask(dataFeedConfig, connFactory)
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
	err = manager.RegisterTaskExecutor(dataFeedListReportsTaskName, dataFeedListReportsTask, cereal.TaskExecutorOpts{
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
	if err != nil {
		return err
	}
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

	dataFeedWorkflowParams := DataFeedWorkflowParams{PollTaskParams: dataFeedTaskParams,
		NodeBatchSize:    dataFeedConfig.ServiceConfig.NodeBatchSize,
		UpdatedNodesOnly: dataFeedConfig.ServiceConfig.UpdatedNodesOnly,
	}

	err = manager.CreateWorkflowSchedule(context.Background(),
		dataFeedScheduleName, dataFeedWorkflowName,
		dataFeedWorkflowParams, true, r)
	if err == cereal.ErrWorkflowScheduleExists {
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

func getNodeData(ctx context.Context, client cfgmgmt.CfgMgmtClient, filters []string) (map[string]interface{}, error) {

	nodeData := make(map[string]interface{}, 0)
	nodeFilters := &cfgmgmtRequest.Nodes{Filter: filters}
	nodes, err := client.GetNodes(ctx, nodeFilters)
	if err != nil {
		log.Errorf("Error getting cfgmgmt/nodes %v", err)
		return nodeData, err
	}

	if len(nodes.Values) == 0 {
		log.Debug("no client run data exists for this node")
		nodeData["node_data"] = DataFeedMessage{}
		return nodeData, nil
	}
	nodeStruct := nodes.Values[0].GetStructValue()
	id := nodeStruct.Fields["id"].GetStringValue()
	lastRunId := nodeStruct.Fields["latest_run_id"].GetStringValue()
	nodeAttributes, err := client.GetAttributes(ctx, &cfgmgmtRequest.Node{NodeId: id})
	if err != nil {
		log.Errorf("Error getting attributes %v", err)
		return nodeData, err
	}
	var automaticJson map[string]interface{}
	err = json.Unmarshal([]byte(nodeAttributes.Automatic), &automaticJson)
	if err != nil {
		log.Errorf("Could not parse automatic attributes from json: %v", err)
		return nodeData, err
	}
	attributesJson := buildDynamicJson(automaticJson)
	nodeData["attributes"] = attributesJson

	lastRun, err := client.GetNodeRun(ctx, &cfgmgmtRequest.NodeRun{NodeId: id, RunId: lastRunId})

	if err != nil {
		log.Errorf("Error getting node run %v", err)
		return nodeData, err
	} else {
		log.Debugf("Last run\n %v", lastRun)
	}
	nodeData["node_data"] = DataFeedMessage{LastRun: lastRun}
	return nodeData, nil
}

func buildDynamicJson(automaticJson map[string]interface{}) map[string]interface{} {
	// check what format he json is e.g. ubuntu/ windows
	// remove specific sections
	// insert normalised
	var dynamicJson map[string]interface{}
	if automaticJson["lsb"] != nil {
		dynamicJson = buildUbuntuJson(automaticJson)
	} else if automaticJson["os"] == "windows" {
		dynamicJson = buildWindowsJson(automaticJson)
	}

	return dynamicJson
}

func buildUbuntuJson(ubuntuJson map[string]interface{}) map[string]interface{} {
	// check what format he json is e.g. ubuntu/ windows
	// remove specific sections
	// insert normalised

	lsb := ubuntuJson["lsb"].(map[string]interface{})
	ubuntuJson["description"] = lsb["description"]

	dmi := ubuntuJson["dmi"].(map[string]interface{})
	system := dmi["system"].(map[string]interface{})
	ubuntuJson["serial_number"] = system["serial_number"]

	//delete(ubuntuJson, "lsb")
	//delete(ubuntuJson, "system")

	return ubuntuJson
}

func buildWindowsJson(windowsJson map[string]interface{}) map[string]interface{} {
	// check what format he json is e.g. ubuntu/ windows
	// remove specific sections
	// insert normalised

	kernel := windowsJson["kernel"].(map[string]interface{})
	windowsJson["description"] = kernel["name"]
	osInfo := kernel["os_info"].(map[string]interface{})
	windowsJson["serial_number"] = osInfo["serial_number"]
	servicePackMajorVersion := fmt.Sprintf("%g", osInfo["service_pack_major_version"].(float64))
	servicePackMinorVersion := fmt.Sprintf("%g", osInfo["service_pack_minor_version"].(float64))
	servicePack := strings.Join([]string{servicePackMajorVersion, servicePackMinorVersion}, ".")
	windowsJson["os_service_pack"] = servicePack

	//delete(windowsJson, "kernel")
	// TODO we could get the chassis from the kernel attributes

	return windowsJson
}
