package service

import (
	"bytes"
	"compress/gzip"
	"context"
	"encoding/json"
	"fmt"
	"math/rand"
	"net/http"
	"strconv"
	"strings"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"

	cfgmgmtRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	rrule "github.com/teambition/rrule-go"

	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/chef/automate/components/data-feed-service/dao"
	"github.com/chef/automate/lib/cereal"
	grpccereal "github.com/chef/automate/lib/cereal/grpc"

	"github.com/chef/automate/lib/grpc/secureconn"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3/s3manager"
)

const version = "1"

type datafeedNotification struct {
	credentials      Credentials
	url              string
	data             bytes.Buffer
	contentType      string
	services         string
	integrationTypes string
}

type DataClient struct {
	client              http.Client
	acceptedStatusCodes []int32
}

func NewDataClient(statusCodes []int32) DataClient {
	return DataClient{
		client:              http.Client{},
		acceptedStatusCodes: statusCodes,
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

func send(sender NotificationSender, notification datafeedNotification) error {
	return sender.sendNotification(notification)
}

func AddCustomHeader(credentials Credentials, header http.Header) {
	if credentials.GetAuthType() == HEADER_AUTH {
		headerString := credentials.GetValues().HeaderJSONString
		var headerMap map[string]string
		err := json.Unmarshal([]byte(headerString), &headerMap)
		if err != nil {
			log.Warnf("Error parsing headers %v", err)
		}
		for key, value := range headerMap {
			header.Set(key, value)
		}
	} else {
		authHeader := credentials.GetValues().AuthorizationHeader
		tokenValue := authHeader[strings.LastIndex(authHeader, " ")+1:]
		if tokenValue != "" {
			header.Add("Authorization", authHeader)
		}
	}
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

	if notification.services == S3 || notification.services == Minio {
		cred := notification.credentials.GetValues().AwsCreds
		sess := ConnectAWS(cred, notification.url, notification.services)
		FileUploadInAws(sess, cred, notification.data.Bytes(), "Prod")

	} else {
		request, err := http.NewRequest("POST", notification.url, &contentBuffer)
		if err != nil {
			log.Error("Error creating request")
			return err
		}

		request.Header.Add("Content-Type", notification.contentType)
		request.Header.Add("Content-Encoding", "gzip")
		request.Header.Add("Accept", notification.contentType)
		request.Header.Add("Chef-Data-Feed-Message-Version", version)
		AddCustomHeader(notification.credentials, request.Header)

		//notification.addCustomHeader(request.Header)

		response, err := client.client.Do(request)
		if err != nil {
			log.Errorf("Error sending message %v", err)
			return err
		} else {
			log.Infof("Asset data posted to %v, Status %v", notification.url, response.Status)
		}
		if !config.IsAcceptedStatusCode(int32(response.StatusCode), client.acceptedStatusCodes) {
			return errors.New(response.Status)
		}
		err = response.Body.Close()
		if err != nil {
			log.Warnf("Error closing response body %v", err)
		}
	}
	return nil
}

func addDataContent(nodeDataContent map[string]interface{}, attributes map[string]interface{}) {
	os, _ := attributes["os"].(string)
	if strings.ToLower(os) == "windows" {
		dmi, _ := attributes["dmi"].(map[string]interface{})
		system, _ := dmi["system"].(map[string]interface{})
		serialNumber := system["serial_number"]
		if serialNumber == nil {
			serialNumber = ""
		}
		nodeDataContent["serial_number"] = serialNumber

		kernel, _ := attributes["kernel"].(map[string]interface{})
		osInfo, _ := kernel["os_info"].(map[string]interface{})
		nodeDataContent["os_service_pack"] = ""

		majorVersion, ok := osInfo["service_pack_major_version"].(float64)
		if !ok {
			return
		}
		minorVersion, ok := osInfo["service_pack_minor_version"].(float64)
		if !ok {
			return
		}
		servicePackMajorVersion := fmt.Sprintf("%g", majorVersion)
		servicePackMinorVersion := fmt.Sprintf("%g", minorVersion)
		servicePack := strings.Join([]string{servicePackMajorVersion, servicePackMinorVersion}, ".")
		nodeDataContent["os_service_pack"] = servicePack
	} else {
		// assume linux
		dmi, _ := attributes["dmi"].(map[string]interface{})
		system, _ := dmi["system"].(map[string]interface{})
		serialNumber := system["serial_number"]
		if serialNumber == nil {
			serialNumber = ""
		}
		nodeDataContent["serial_number"] = serialNumber
	}
}

func getNodeFields(ctx context.Context, client cfgmgmt.CfgMgmtServiceClient, filters []string) (string, string, error) {

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

func getNodeAttributes(ctx context.Context, client cfgmgmt.CfgMgmtServiceClient, nodeId string) (map[string]interface{}, error) {

	attributesJson := make(map[string]interface{})

	nodeAttributes, err := client.GetAttributes(ctx, &cfgmgmtRequest.Node{NodeId: nodeId})
	if err != nil {
		log.Warnf("Error getting attributes %v", err)
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

func getNodeHostFields(ctx context.Context, client cfgmgmt.CfgMgmtServiceClient, filters []string) (string, string, string, error) {
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

	ipAddress, _ := attributesJson["ipaddress"].(string)
	macAddress, _ := attributesJson["macaddress"].(string)
	hostname, _ := attributesJson["hostname"].(string)

	return ipAddress, macAddress, hostname
}
func ConnectAWS(cred AwsCredentials, url string, service string) *session.Session {
	var config *aws.Config
	if service == "Minio" {
		config = &aws.Config{
			Endpoint:         aws.String(url),
			Region:           aws.String("us-east-2"),
			DisableSSL:       aws.Bool(true),
			S3ForcePathStyle: aws.Bool(true),
			Credentials:      credentials.NewStaticCredentials(string(cred.GetValues().AwsCreds.accesskey), string(cred.GetValues().AwsCreds.secretAccessKey), ""),
		}
	} else {
		config = &aws.Config{
			Region:      aws.String(cred.GetValues().AwsCreds.region),
			Credentials: credentials.NewStaticCredentials(string(cred.GetValues().AwsCreds.accesskey), string(cred.GetValues().AwsCreds.secretAccessKey), ""),
		}
	}
	sess, err := session.NewSession(config)
	if err != nil {
		log.Errorf("Error while getting  NewSession: %v", err)
	}
	return sess
}

func FileUploadInAws(sess *session.Session, cred AwsCredentials, data []byte, FolderName string) (*s3manager.UploadOutput, error) {
	t := time.Now().UTC()
	year := t.Year()
	month := int(t.Month())
	day := t.Day()
	hr := t.Hour()
	min := t.Minute()
	sec := t.Second()
	rand := rand.New(rand.NewSource(time.Now().UnixNano()))

	filePathAndName := "automate/" + FolderName + "/" +
		strconv.Itoa(year) + "/" +
		strconv.Itoa(month) + "/" +
		strconv.Itoa(day) + "/" +
		strconv.Itoa(hr) + "_" + strconv.Itoa(min) + "_" + strconv.Itoa(sec) + "_" + strconv.Itoa(rand.Intn(1000)) + ".json"

	uploader := s3manager.NewUploader(sess)

	res, err := uploader.Upload(&s3manager.UploadInput{
		Bucket: aws.String(cred.bucket),     // Bucket to be used
		Key:    aws.String(filePathAndName), // Name of the file to be saved
		Body:   bytes.NewReader(data),       // File
	})
	if err != nil {
		// Do your error handling here
		return nil, err
	}
	return res, nil
}
