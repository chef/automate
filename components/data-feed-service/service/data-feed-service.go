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

	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"

	secrets "github.com/chef/automate/api/external/secrets"
	cfgmgmtRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cfgmgmtResponse "github.com/chef/automate/api/interservice/cfgmgmt/response"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/data-feed-service/config"
	notifications "github.com/chef/automate/components/notifications-client/api"
	"github.com/chef/automate/lib/cereal"
	grpccereal "github.com/chef/automate/lib/cereal/grpc"
	"github.com/chef/automate/lib/cereal/patterns"
	"github.com/chef/automate/lib/grpc/secureconn"
)

var (
	dataFeedTaskName     = cereal.NewTaskName("data-feed-poll")
	dataFeedWorkflowName = cereal.NewWorkflowName("data-feed-workflow")
	dataFeedScheduleName = "periodic-data-feed-workflow"
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

func Start(dataFeedConfig *config.DataFeedConfig, connFactory *secureconn.Factory) error {
	conn, err := connFactory.Dial("cereal-service", dataFeedConfig.CerealConfig.Target)
	if err != nil {
		return err
	}

	backend := grpccereal.NewGrpcBackendFromConn("data-feed-service", conn)
	manager, err := cereal.NewManager(backend)
	if err != nil {
		return err
	}

	dataFeedPollTask, err := NewDataFeedPollTask(dataFeedConfig, connFactory)
	if err != nil {
		return err
	}

	err = manager.RegisterWorkflowExecutor(dataFeedWorkflowName, patterns.NewSingleTaskWorkflowExecutor(dataFeedTaskName, false))
	if err != nil {
		return err
	}
	err = manager.RegisterTaskExecutor(dataFeedTaskName, dataFeedPollTask, cereal.TaskExecutorOpts{
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

	dataFeedTaskParams := DataFeedPollTaskParams{
		FeedInterval:    dataFeedConfig.ServiceConfig.FeedInterval,
		AssetPageSize:   dataFeedConfig.ServiceConfig.AssetPageSize,
		ReportsPageSize: dataFeedConfig.ServiceConfig.ReportsPageSize,
	}

	err = manager.CreateWorkflowSchedule(context.Background(),
		dataFeedScheduleName, dataFeedWorkflowName,
		dataFeedTaskParams, true, r)
	if err == cereal.ErrWorkflowScheduleExists {
		// TODO(ssd) 2019-09-09: Right now this resets Dtstart on the recurrence, do we want that?
		err = manager.UpdateWorkflowScheduleByName(context.Background(), dataFeedScheduleName, dataFeedWorkflowName,
			cereal.UpdateParameters(dataFeedTaskParams),
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

type DataFeedPollTask struct {
	notifications notifications.NotificationsClient
	cfgMgmt       cfgmgmt.CfgMgmtClient
	secrets       secrets.SecretsServiceClient
	reporting     reporting.ReportingServiceClient
}

type DataFeedPollTaskParams struct {
	AssetPageSize   int32
	ReportsPageSize int32
	FeedInterval    time.Duration
}

func NewDataFeedPollTask(dataFeedConfig *config.DataFeedConfig, connFactory *secureconn.Factory) (*DataFeedPollTask, error) {
	notifConn, err := connFactory.Dial("notifications-service", dataFeedConfig.NotificationsConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to notifications-service")
	}

	cfgMgmtConn, err := connFactory.Dial("config-mgmt-service", dataFeedConfig.CfgmgmtConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to config-mgmt-service")
	}

	secretsConn, err := connFactory.Dial("secrets-service", dataFeedConfig.SecretsConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to secrets-service")
	}

	complianceConn, err := connFactory.Dial("compliance-service", dataFeedConfig.ComplianceConfig.Target)
	if err != nil {
		return nil, errors.Wrap(err, "could not connect to compliance-service")
	}

	return &DataFeedPollTask{
		notifications: notifications.NewNotificationsClient(notifConn),
		secrets:       secrets.NewSecretsServiceClient(secretsConn),
		reporting:     reporting.NewReportingServiceClient(complianceConn),
		cfgMgmt:       cfgmgmt.NewCfgMgmtClient(cfgMgmtConn),
	}, nil
}

func (d *DataFeedPollTask) Run(ctx context.Context, task cereal.Task) (interface{}, error) {
	params := DataFeedPollTaskParams{}
	err := task.GetParameters(&params)
	if err != nil {
		return nil, errors.Wrap(err, "failed to parse task parameters")
	}

	now := time.Now()
	feedStartTime, feedEndTime := getFeedTimes(params.FeedInterval, now)
	assetRules := d.getAssetRules(ctx)
	if len(assetRules) == 0 {
		return nil, nil
	}
	// build messages for any nodes which have had a CCR in last window, include last compliance report
	datafeedMessages := d.buildDatafeed(ctx, params.AssetPageSize, feedStartTime, feedEndTime)
	// build messages for any reports in last window include last CCR and OHAI
	d.buildReportFeed(ctx, params.ReportsPageSize, feedStartTime, feedEndTime, datafeedMessages)
	// get the valus from this ipaddress:datafeedMessage map
	data := make([]datafeedMessage, 0, len(datafeedMessages))
	for _, value := range datafeedMessages {
		data = append(data, value)
	}

	if len(datafeedMessages) == 0 {
		return nil, nil
	}
	for rule := range assetRules {
		log.Debugf("Rule id %v", assetRules[rule].Id)
		log.Debugf("Rule Name %v", assetRules[rule].Name)
		log.Debugf("Rule Event %v", assetRules[rule].Event)
		log.Debugf("Rule Action %v", assetRules[rule].Action)
		switch action := assetRules[rule].Action.(type) {
		case *notifications.Rule_ServiceNowAlert:
			log.Debugf("Service now alert URL  %v", action.ServiceNowAlert.Url)
			log.Debugf("Service now alert secret  %v", action.ServiceNowAlert.SecretId)
			username, password, err := getCredentials(ctx, d.secrets, action.ServiceNowAlert.SecretId)
			if err != nil {
				log.Errorf("Error retrieving credentials, cannot send asset notification: %v", err)
				// TODO error handling - need some form of report in automate that indicates when data was sent and if it was successful
			} else {
				// build and send notification for this rule
				notification := datafeedNotification{username: username, password: password, url: action.ServiceNowAlert.Url, data: data}
				client := NewDataClient()
				err = send(client, notification)
				if err != nil {
					handleSendErr(notification, feedStartTime, feedEndTime, err)
				}
			}
		}
	}
	return nil, nil
}

func (d *DataFeedPollTask) getAssetRules(ctx context.Context) []notifications.Rule {
	var retry = 0
	assetRules := make([]notifications.Rule, 0)
	for {
		listResponse, err := d.notifications.ListRules(ctx, &notifications.Empty{})
		if err != nil {
			// this is currently inadequate, retry period must be calculated so it does not overlap with the interval
			// bring the service down if we can't get a response in 50 seconds since the smallest interval we allow is 1 minute
			if retry == 5 {
				log.Fatalf("Error calling list rules on notifications-service attempt %v: %v", retry, err)
			}
			log.Errorf("Error calling list rules on notifications-service attempt %v: %v", retry, err)
			retry++
			wait, _ := time.ParseDuration("10s")
			time.Sleep(wait)
			continue
		}
		responseCode := listResponse.Code
		log.Debugf("ListRules responseCode %v", responseCode)
		messages := listResponse.Messages
		// get the rules here - service now rules only
		log.Debugf("ListRules messages %v", messages)

		rules := listResponse.Rules
		for rule := range rules {
			event := rules[rule].Event
			if event == notifications.Rule_Assets {
				assetRules = append(assetRules, *rules[rule])
			}
		}
		break
	}
	return assetRules
}

func handleSendErr(notification datafeedNotification, startTime time.Time, endTime time.Time, err error) {
	log.Errorf("Failed to send notification to %v. Start: %v, End: %v. %v", notification.url, startTime, endTime, err)
	// TODO report this failure to the UI with the time window and notification details
}

func getFeedTimes(feedInterval time.Duration, now time.Time) (time.Time, time.Time) {
	var feedEndTime time.Time
	var feedStartTime time.Time

	feedEndTime = getFeedEndTime(feedInterval, now)
	feedStartTime = feedEndTime.Add(-feedInterval)

	log.Debugf("Feed interval start, end: %s, %s", feedStartTime.Format("15:04:05"), feedEndTime.Format("15:04:05"))
	return feedStartTime, feedEndTime
}

func getFeedEndTime(feedInterval time.Duration, now time.Time) time.Time {
	log.Debugf("Time Now: %s", now.Format("15:04:05"))
	/*
	 * We round the current time down based on the interval duration to get the end of the last interval.
	 *
	 * Round will round either up or down to the nearest value of the inteval duration.
	 * e.g 1:20pm rounds to 1pm, 1:40pm rounds to 2pm
	 *
	 * If we have rounded down that will be our feed end time. The end of a clock interval
	 * rather than current time e.g. 1pm
	 */
	feedEndTime := now.Round(feedInterval)
	log.Debugf("FeedInterval/Units: %s", feedInterval)
	/*
	 * If we have rounded up we subtract the interval to effectively round down
	 */
	if feedEndTime.After(now) {
		feedEndTime = feedEndTime.Add(-feedInterval)
		log.Debugf("feedEndTime after: %s", feedEndTime.Format("15:04:05"))
	}
	log.Debugf("feedEndTime: %s", feedEndTime.Format("15:04:05"))
	return feedEndTime
}

func getCredentials(ctx context.Context, client secrets.SecretsServiceClient, secretID string) (string, string, error) {
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
	return nil
}

func (d *DataFeedPollTask) buildDatafeed(ctx context.Context, pageSize int32, feedStartTime time.Time, feedEndTime time.Time) map[string]datafeedMessage {
	log.Debug("Inventory nodes start")
	feedStartString, err := ptypes.TimestampProto(feedStartTime)
	if err != nil {
		return nil
	}
	feedEndString, err := ptypes.TimestampProto(feedEndTime)
	if err != nil {
		return nil
	}

	nodesRequest := &cfgmgmtRequest.InventoryNodes{
		PageSize: pageSize,
		Start:    feedStartString,
		End:      feedEndString,
		Sorting: &cfgmgmtRequest.Sorting{
			Order: cfgmgmtRequest.Order_desc,
		},
	}

	inventoryNodes, err := d.cfgMgmt.GetInventoryNodes(ctx, nodesRequest)
	if err != nil {
		return nil
	}

	nodeIDs := make([]string, 0)
	log.Debugf("No of inventory nodes %v", len(inventoryNodes.Nodes))
	for len(inventoryNodes.Nodes) > 0 {
		for _, node := range inventoryNodes.Nodes {
			log.Debugf("Inventory node #%v", node)
			nodeIDs = append(nodeIDs, node.Id)
		}
		lastNode := inventoryNodes.Nodes[len(inventoryNodes.Nodes)-1]
		nodesRequest.CursorId = lastNode.Id
		nodesRequest.CursorDate = lastNode.Checkin

		inventoryNodes, err = d.cfgMgmt.GetInventoryNodes(ctx, nodesRequest)
		log.Debugf("inventory nodes %v, cursor %v", len(inventoryNodes.Nodes), lastNode.Id)
		if err != nil {
			return nil
		}
	}
	log.Info("Building data feed...")

	nodeMessages := make(map[string]datafeedMessage)

	for _, nodeID := range nodeIDs {
		filters := []string{"id:" + nodeID}

		message := getNodeData(ctx, d.cfgMgmt, filters)

		ipaddress := message.Automatic["ipaddress"].(string)

		filter := &reporting.ListFilter{Type: "ipaddress", Values: []string{ipaddress}}
		reportFilters := []*reporting.ListFilter{filter}

		ipaddressQuery := &reporting.Query{
			Page:    0,
			PerPage: 1,
			Filters: reportFilters,
			Sort:    "latest_report.end_time",
			Order:   reporting.Query_DESC,
		}
		reports, err := d.reporting.ListReports(ctx, ipaddressQuery)
		if err != nil {
			log.Errorf("Error getting report by ip %v", err)
			//TODO
		}
		// might not be a report!

		//var fullReport *reporting.
		fullReport := new(reporting.Report)
		if len(reports.Reports) != 0 {
			reportID := &reporting.Query{Id: reports.Reports[0].Id}
			fullReport, err = d.reporting.ReadReport(ctx, reportID)
			// TODO err handling
			if err != nil {
				log.Errorf("Error getting report by ip %v", err)
				//TODO
			}
		}

		message.Report = fullReport
		nodeMessages[ipaddress] = message

	}
	log.Debugf("%v node attribute messages retrieved in interval", len(nodeMessages))
	return nodeMessages
}

func getNodeData(ctx context.Context, client cfgmgmt.CfgMgmtClient, filters []string) datafeedMessage {

	nodeFilters := &cfgmgmtRequest.Nodes{Filter: filters}
	nodes, err := client.GetNodes(ctx, nodeFilters)
	if err != nil {
		log.Errorf("Error getting cfgmgmt/nodes %v", err)
		//TODO
	}

	nodeStruct := nodes.Values[0].GetStructValue()
	id := nodeStruct.Fields["id"].GetStringValue()
	lastRunId := nodeStruct.Fields["latest_run_id"].GetStringValue()
	nodeAttributes, err := client.GetAttributes(ctx, &cfgmgmtRequest.Node{NodeId: id})
	if err != nil {
		log.Errorf("Error getting attributes %v", err)
		//TODO
	}
	var automaticJson map[string]interface{}
	err = json.Unmarshal([]byte(nodeAttributes.Automatic), &automaticJson)
	if err != nil {
		log.Errorf("Could not parse automatic attributes from json: %v", err)
		// continue to the next node
		// TODO
	}
	attributesJson := buildDynamicJson(automaticJson)

	lastRun, error := client.GetNodeRun(ctx, &cfgmgmtRequest.NodeRun{NodeId: id, RunId: lastRunId})

	if error != nil {
		log.Errorf("Error getting node run %v", error)
	} else {
		log.Debugf("Last run\n %v", lastRun)
	}

	return datafeedMessage{Automatic: attributesJson, LastRun: lastRun}
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

func (d *DataFeedPollTask) buildReportFeed(ctx context.Context, pageSize int32, feedStartTime time.Time, feedEndTime time.Time, datafeedMessages map[string]datafeedMessage) {
	feedStartString := strings.SplitAfter(feedStartTime.Format(time.RFC3339), "Z")[0]
	feedEndString := strings.SplitAfter(feedEndTime.Format(time.RFC3339), "Z")[0]
	log.Infof("Building report feed... %v - %v", feedStartString, feedEndString)

	startFilter := &reporting.ListFilter{Type: "start_time", Values: []string{feedStartString}}
	endFilter := &reporting.ListFilter{Type: "end_time", Values: []string{feedEndString}}

	filters := []*reporting.ListFilter{startFilter, endFilter}

	// page is not something we can configure we should start with
	// page at 1 and work out how many calls to make based on page
	// size divide the total by page size and add 1 and we loop
	// over that
	page := int32(1)
	query := &reporting.Query{
		Page:    page,
		PerPage: pageSize,
		Filters: filters,
		Sort:    "latest_report.end_time",
		Order:   reporting.Query_DESC,
	}
	log.Debugf("report query %v", query)

	reports, err := d.reporting.ListReports(ctx, query)
	if err != nil {
		log.Errorf("Error getting reporting/ListReports %v", err)
	}

	pages := (reports.Total / pageSize)
	if (reports.Total % pageSize) != 0 {
		pages++
	}
	log.Debugf("Total reports: %v, reports per page: %v, total pages %v: ",
		reports.Total, pageSize, pages)

	// get reports from the pages
	for page <= pages {
		log.Debugf("report query %v", query)
		for report := range reports.Reports {
			ipaddress := reports.Reports[report].Ipaddress
			log.Debugf("report has ipaddress %v", ipaddress)
			if _, ok := datafeedMessages[ipaddress]; ok {
				log.Debugf("node data already exists for %v", ipaddress)
			} else {
				// the node hasn't had a client run in last window, but has a report so we can add the report
				id := &reporting.Query{Id: reports.Reports[report].Id}

				fullReport, err := d.reporting.ReadReport(ctx, id)
				if err != nil {
					log.Debugf("Error getting report by if %v", err)
					//TODO
				}
				filters := []string{"ipaddress:" + ipaddress}
				// node the latest node data associated with this report
				message := getNodeData(ctx, d.cfgMgmt, filters)
				message.Report = fullReport
				datafeedMessages[ipaddress] = message
			}

		}
		page++
		query = &reporting.Query{
			Page:    page,
			PerPage: pageSize,
			Filters: filters}
		reports, err = d.reporting.ListReports(ctx, query)
		if err != nil {
			log.Errorf("Error getting reporting/ListReports %v", err)
		}
	}
}
