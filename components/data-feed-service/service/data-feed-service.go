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

const version = "1"

type datafeedNotification struct {
	username string
	password string
	url      string
	data     bytes.Buffer
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

	cfgMgmtConn, err := connFactory.Dial("config-mgmt-service", dataFeedConfig.CfgmgmtConfig.Target)
	if err != nil {
		return errors.Wrap(err, "could not connect to config-mgmt-service")
	}

	complianceConn, err := connFactory.Dial("compliance-service", dataFeedConfig.ComplianceConfig.Target)
	if err != nil {
		return errors.Wrap(err, "could not connect to compliance-service")
	}

	secretsConn, err := connFactory.Dial("secrets-service", dataFeedConfig.SecretsConfig.Target)
	if err != nil {
		return errors.Wrap(err, "could not connect to secrets-service")
	}

	dataFeedPollTask, err := NewDataFeedPollTask(dataFeedConfig, cfgMgmtConn, complianceConn, db, manager)
	if err != nil {
		return errors.Wrap(err, "could not create data feed poll task")
	}

	dataFeedAggregateTask := NewDataFeedAggregateTask(dataFeedConfig, cfgMgmtConn, complianceConn, secretsConn, db)

	err = manager.RegisterWorkflowExecutor(dataFeedWorkflowName, &DataFeedWorkflowExecutor{workflowName: dataFeedWorkflowName, dataFeedConfig: dataFeedConfig, manager: manager})
	if err != nil {
		return err
	}
	err = manager.RegisterTaskExecutor(dataFeedPollTaskName, dataFeedPollTask, cereal.TaskExecutorOpts{
		Workers: 1,
	})
	if err != nil {
		return err
	}
	err = manager.RegisterTaskExecutor(dataFeedAggregateTaskName, dataFeedAggregateTask, cereal.TaskExecutorOpts{
		Workers: 1,
	})
	if err != nil {
		return err
	}

	r, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.SECONDLY,
		Interval: int(dataFeedConfig.ServiceConfig.FeedInterval.Seconds()),
		Dtstart:  time.Now().Round(dataFeedConfig.ServiceConfig.FeedInterval).Add(30 * time.Second),
	})
	if err != nil {
		return err
	}

	dataFeedWorkflowParams := DataFeedWorkflowParams{}

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

	log.Debugf("sendNotification bytes length %v", notification.data.Len())
	var contentBuffer bytes.Buffer
	zip := gzip.NewWriter(&contentBuffer)
	_, err := zip.Write(notification.data.Bytes())
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
	request.Header.Add("Chef-Data-Feed-Message-Version", version)

	response, err := client.client.Do(request)
	if err != nil {
		log.Errorf("Error sending message %v", err)
		return err
	} else {
		log.Infof("Asset data posted to %v, Status %v", notification.url, response.Status)
	}
	if response.StatusCode != http.StatusAccepted {
		return errors.New(response.Status)
	}
	err = response.Body.Close()
	if err != nil {
		log.Warnf("Error closing response body %v", err)
	}
	return nil
}

func getNodeData(ctx context.Context, client cfgmgmt.CfgMgmtClient, filters []string) (map[string]interface{}, error) {

	nodeData := make(map[string]interface{})
	nodeId, lastRunId, err := getNodeFields(ctx, client, filters)
	if err != nil {
		return nodeData, err
	}

	attributesJson, err := getNodeAttributes(ctx, client, nodeId)
	if err != nil {
		return nodeData, err
	}

	nodeData["attributes"] = attributesJson

	lastRun, err := client.GetNodeRun(ctx, &cfgmgmtRequest.NodeRun{NodeId: nodeId, RunId: lastRunId})

	if err != nil {
		log.Errorf("Error getting node run %v", err)
		return nodeData, err
	}
	automaticAttrs := attributesJson["automatic"].(map[string]interface{})
	ipaddress, macAddress, hostname := getHostAttributes(automaticAttrs)
	nodeDataContent := make(map[string]interface{})
	nodeData["client_run"] = lastRun
	nodeDataContent["macaddress"] = macAddress
	nodeDataContent["hostname"] = hostname
	nodeDataContent["ipaddress"] = ipaddress
	addDataContent(nodeDataContent, automaticAttrs)
	nodeData["node"] = nodeDataContent
	return nodeData, nil
}

func addDataContent(nodeDataContent map[string]interface{}, attributes map[string]interface{}) {
	if strings.ToLower(attributes["os"].(string)) == "windows" {
		kernel := attributes["kernel"].(map[string]interface{})
		osInfo := kernel["os_info"].(map[string]interface{})
		nodeDataContent["serial_number"] = osInfo["serial_number"]
		servicePackMajorVersion := fmt.Sprintf("%g", osInfo["service_pack_major_version"].(float64))
		servicePackMinorVersion := fmt.Sprintf("%g", osInfo["service_pack_minor_version"].(float64))
		servicePack := strings.Join([]string{servicePackMajorVersion, servicePackMinorVersion}, ".")
		nodeDataContent["os_service_pack"] = servicePack
	} else {
		// assume linux
		dmi := attributes["dmi"].(map[string]interface{})
		system := dmi["system"].(map[string]interface{})
		nodeDataContent["serial_number"] = system["serial_number"]
	}
}

func getNodeFields(ctx context.Context, client cfgmgmt.CfgMgmtClient, filters []string) (string, string, error) {

	nodeFilters := &cfgmgmtRequest.Nodes{Filter: filters}
	nodes, err := client.GetNodes(ctx, nodeFilters)
	if err != nil {
		log.Errorf("Error getting cfgmgmt/nodes %v", err)
		return "", "", err
	}

	if len(nodes.Values) == 0 {
		log.Debug("no node data exists for this node")
		return "", "", nil
	}
	node := nodes.Values[0].GetStructValue()
	id := node.Fields["id"].GetStringValue()
	lastRunId := node.Fields["latest_run_id"].GetStringValue()

	return id, lastRunId, nil

}

func getNodeAttributes(ctx context.Context, client cfgmgmt.CfgMgmtClient, nodeId string) (map[string]interface{}, error) {

	attributesJson := make(map[string]interface{})

	nodeAttributes, err := client.GetAttributes(ctx, &cfgmgmtRequest.Node{NodeId: nodeId})
	if err != nil {
		log.Errorf("Error getting attributes %v", err)
		return attributesJson, err
	}

	attributesJson["automatic"] = getAttributesAsJson(nodeAttributes.Automatic, "automatic")
	attributesJson["default"] = getAttributesAsJson(nodeAttributes.Default, "default")
	attributesJson["normal"] = getAttributesAsJson(nodeAttributes.Normal, "normal")
	attributesJson["override"] = getAttributesAsJson(nodeAttributes.Override, "override")
	attributesJson["all_value_count"] = nodeAttributes.AllValueCount
	attributesJson["automatic_value_count"] = nodeAttributes.AutomaticValueCount
	attributesJson["default_value_count"] = nodeAttributes.DefaultValueCount
	attributesJson["normal_value_count"] = nodeAttributes.NormalValueCount
	attributesJson["override_value_count"] = nodeAttributes.OverrideValueCount
	attributesJson["node_id"] = nodeAttributes.NodeId
	attributesJson["name"] = nodeAttributes.Name
	attributesJson["run_list"] = nodeAttributes.RunList
	attributesJson["chef_environment"] = nodeAttributes.ChefEnvironment

	return attributesJson, nil
}

func getAttributesAsJson(attributes string, attributeType string) map[string]interface{} {
	attributesJson := make(map[string]interface{})
	err := json.Unmarshal([]byte(attributes), &attributesJson)
	if err != nil {
		log.Errorf("Could not parse %v attributes from json: %v", attributeType, err)
	}
	return attributesJson
}

func getNodeHostFields(ctx context.Context, client cfgmgmt.CfgMgmtClient, filters []string) (string, string, string, error) {
	nodeId, _, err := getNodeFields(ctx, client, filters)
	if err != nil {
		return "", "", "", err
	}
	attributesJson, err := getNodeAttributes(ctx, client, nodeId)
	if err != nil {
		return "", "", "", err
	}
	ipaddress, macAddress, hostname := getHostAttributes(attributesJson["automatic"].(map[string]interface{}))
	return ipaddress, macAddress, hostname, nil
}

func getHostAttributes(attributesJson map[string]interface{}) (string, string, string) {
	return attributesJson["ipaddress"].(string), attributesJson["macaddress"].(string), attributesJson["hostname"].(string)
}
