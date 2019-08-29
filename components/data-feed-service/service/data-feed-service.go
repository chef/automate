package service

import (
	"bytes"
	"compress/gzip"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"strings"
	"time"

	secrets "github.com/chef/automate/api/external/secrets"
	cfgmgmtRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"
	cfgmgmtResponse "github.com/chef/automate/api/interservice/cfgmgmt/response"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/chef/automate/components/data-feed-service/config"
	notifications "github.com/chef/automate/components/notifications-client/api"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/golang/protobuf/ptypes"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
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

type serviceClients struct {
	notifications notifications.NotificationsClient
	cfgMgmt       cfgmgmt.CfgMgmtClient
	secrets       secrets.SecretsServiceClient
	reporting     reporting.ReportingServiceClient
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

func getConnection(connectionFactory *secureconn.Factory, service string, target string) *grpc.ClientConn {
	connection, err := connectionFactory.Dial(service, target, grpc.WithBlock())
	if err != nil {
		log.Fatalf("Error getting connection to %v: %v", service, err)
	}
	return connection
}

func initServiceClients(dataFeedConfig *config.DataFeedConfig) serviceClients {
	clients := serviceClients{}
	connectionFactory := secureconn.NewFactory(*dataFeedConfig.ServiceCerts)
	connection := getConnection(connectionFactory, "notifications-service", dataFeedConfig.NotificationsConfig.Target)
	clients.notifications = notifications.NewNotificationsClient(connection)
	log.Debugf("NotificationsClient created")

	connection = getConnection(connectionFactory, "config-mgmt-service", dataFeedConfig.CfgmgmtConfig.Target)
	clients.cfgMgmt = cfgmgmt.NewCfgMgmtClient(connection)
	log.Debugf("CfgMgmtClient created")

	connection = getConnection(connectionFactory, "secrets-service", dataFeedConfig.SecretsConfig.Target)
	clients.secrets = secrets.NewSecretsServiceClient(connection)
	log.Debugf("Secrets created")

	connection = getConnection(connectionFactory, "compliance-service", dataFeedConfig.ComplianceConfig.Target)
	clients.reporting = reporting.NewReportingServiceClient(connection)
	log.Debugf("Reporting service client created")
	return clients
}

func Start(dataFeedConfig *config.DataFeedConfig) {
	log.Debugf("data-feed-service start")

	serviceClients := initServiceClients(dataFeedConfig)
	for {
		now := time.Now()
		feedStartTime, feedEndTime := getFeedTimes(dataFeedConfig, now)
		assetRules := getAssetRules(serviceClients)
		if len(assetRules) == 0 {
			waitForInterval(dataFeedConfig.ServiceConfig.FeedInterval, feedEndTime, now)
			continue
		}
		// build messages for any nodes which have had a CCR in last window, include last compliance report
		datafeedMessages := buildDatafeed(serviceClients, dataFeedConfig, feedStartTime, feedEndTime)
		// build messages for any reports in last window include last CCR and OHAI
		buildReportFeed(serviceClients, dataFeedConfig, feedStartTime, feedEndTime, datafeedMessages)
		// get the valus from this ipaddress:datafeedMessage map
		data := make([]datafeedMessage, 0)
		for _, value := range datafeedMessages {
			data = append(data, value)
		}

		if len(datafeedMessages) == 0 {
			waitForInterval(dataFeedConfig.ServiceConfig.FeedInterval, feedEndTime, now)
			continue
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
				username, password, err := getCredentials(serviceClients, dataFeedConfig, action.ServiceNowAlert.SecretId)
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

		waitForInterval(dataFeedConfig.ServiceConfig.FeedInterval, feedEndTime, now)
	}
}

func getAssetRules(serviceClients serviceClients) []notifications.Rule {
	var retry = 0
	assetRules := make([]notifications.Rule, 0)
	for {
		listResponse, err := serviceClients.notifications.ListRules(context.Background(), &notifications.Empty{})
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

func waitForInterval(feedInterval time.Duration, feedEndTime time.Time, now time.Time) {

	nextFeedEndTime := feedEndTime.Add(feedInterval)
	/*
	 * Calculate the sleep duration as the first sleep may be in the middle of an interval.
	 * E.g. for first wait now may be 1:55pm, waiting an hour to get data from a rounded
	 * down window of 12pm-1pm will introduce a 55min lag by waking at 2:55pm
	 *
	 * Calculating the sleepDuration will reduce the value
	 * e.g nextFeedEndTime 2pm - 1.55pm = 5min sleep
	 * waking at 2pm rather than 2:55pm to get 1pm-2pm data witout lag.
	 */
	sleepDuration := nextFeedEndTime.Sub(now)
	log.Debugf("Sleeping for %v", sleepDuration)
	time.Sleep(sleepDuration)
}

func getFeedTimes(dataFeedConfig *config.DataFeedConfig, now time.Time) (time.Time, time.Time) {
	var feedEndTime time.Time
	var feedStartTime time.Time

	feedInterval := dataFeedConfig.ServiceConfig.FeedInterval

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

func getCredentials(serviceClients serviceClients, dataFeedConfig *config.DataFeedConfig, secretId string) (string, string, error) {

	secret, err := serviceClients.secrets.Read(context.Background(), &secrets.Id{Id: secretId})

	username := ""
	password := ""
	if err == nil {
		data := secret.GetData()

		for kv := range data {
			if data[kv].Key == "username" {
				username = data[kv].Value
			} else if data[kv].Key == "password" {
				password = data[kv].Value
			}
		}
	}

	return username, password, err
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

func buildDatafeed(serviceClients serviceClients, dataFeedConfig *config.DataFeedConfig, feedStartTime time.Time, feedEndTime time.Time) map[string]datafeedMessage {

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
		PageSize: dataFeedConfig.ServiceConfig.AssetPageSize,
		Start:    feedStartString,
		End:      feedEndString,
		Sorting: &cfgmgmtRequest.Sorting{
			Order: cfgmgmtRequest.Order_desc,
		},
	}

	inventoryNodes, err := serviceClients.cfgMgmt.GetInventoryNodes(context.Background(), nodesRequest)
	if err != nil {
		return nil
	}

	nodeIds := make([]string, 0)
	log.Debugf("No of inventory nodes %v", len(inventoryNodes.Nodes))
	for len(inventoryNodes.Nodes) > 0 {
		for _, node := range inventoryNodes.Nodes {
			log.Debugf("Inventory node #%v", node)
			nodeIds = append(nodeIds, node.Id)
		}
		lastNode := inventoryNodes.Nodes[len(inventoryNodes.Nodes)-1]
		nodesRequest.CursorId = lastNode.Id
		nodesRequest.CursorDate = lastNode.Checkin

		inventoryNodes, err = serviceClients.cfgMgmt.GetInventoryNodes(context.Background(), nodesRequest)
		log.Debugf("inventory nodes %v, cursor %v", len(inventoryNodes.Nodes), lastNode.Id)
		if err != nil {
			return nil
		}
	}
	log.Info("Building data feed...")

	nodeMessages := make(map[string]datafeedMessage)

	for _, nodeId := range nodeIds {
		filters := []string{"id:" + nodeId}

		message := getNodeData(serviceClients, filters)

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
		reports, err := serviceClients.reporting.ListReports(context.Background(), ipaddressQuery)
		if err != nil {
			log.Errorf("Error getting report by ip %v", err)
			//TODO
		}
		// might not be a report!

		//var fullReport *reporting.
		fullReport := new(reporting.Report)
		if len(reports.Reports) != 0 {
			reportId := &reporting.Query{Id: reports.Reports[0].Id}
			fullReport, err = serviceClients.reporting.ReadReport(context.Background(), reportId)
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

func getNodeData(serviceClients serviceClients, filters []string) datafeedMessage {

	nodeFilters := &cfgmgmtRequest.Nodes{Filter: filters}
	nodes, err := serviceClients.cfgMgmt.GetNodes(context.Background(), nodeFilters)
	if err != nil {
		log.Errorf("Error getting cfgmgmt/nodes %v", err)
		//TODO
	}

	nodeStruct := nodes.Values[0].GetStructValue()
	id := nodeStruct.Fields["id"].GetStringValue()
	lastRunId := nodeStruct.Fields["latest_run_id"].GetStringValue()
	nodeAttributes, err := serviceClients.cfgMgmt.GetAttributes(context.Background(), &cfgmgmtRequest.Node{NodeId: id})
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

	lastRun, error := serviceClients.cfgMgmt.GetNodeRun(context.Background(), &cfgmgmtRequest.NodeRun{NodeId: id, RunId: lastRunId})

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

func buildReportFeed(serviceClients serviceClients, dataFeedConfig *config.DataFeedConfig, feedStartTime time.Time, feedEndTime time.Time, datafeedMessages map[string]datafeedMessage) {

	feedStartString := strings.SplitAfter(feedStartTime.Format(time.RFC3339), "Z")[0]
	feedEndString := strings.SplitAfter(feedEndTime.Format(time.RFC3339), "Z")[0]
	log.Infof("Building report feed... %v - %v", feedStartString, feedEndString)

	startFilter := &reporting.ListFilter{Type: "start_time", Values: []string{feedStartString}}
	endFilter := &reporting.ListFilter{Type: "end_time", Values: []string{feedEndString}}

	filters := []*reporting.ListFilter{startFilter, endFilter}
	// page is not something we can configure
	// we should start with page at 1 and work out how many calls to make based on page size
	// divide the total by page size and add 1 and we loop over that
	page := int32(1)

	query := &reporting.Query{
		Page:    page,
		PerPage: dataFeedConfig.ServiceConfig.ReportsPageSize,
		Filters: filters,
		Sort:    "latest_report.end_time",
		Order:   reporting.Query_DESC,
	}
	log.Debugf("report query %v", query)

	reports, err := serviceClients.reporting.ListReports(context.Background(), query)
	if err != nil {
		log.Errorf("Error getting reporting/ListReports %v", err)
	}

	pages := (reports.Total / dataFeedConfig.ServiceConfig.ReportsPageSize)
	if (reports.Total % dataFeedConfig.ServiceConfig.ReportsPageSize) != 0 {
		pages++
	}
	log.Debugf("Total reports: %v, reports per page: %v, total pages %v: ", reports.Total,
		dataFeedConfig.ServiceConfig.ReportsPageSize, pages)

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

				fullReport, err := serviceClients.reporting.ReadReport(context.Background(), id)
				if err != nil {
					log.Debugf("Error getting report by if %v", err)
					//TODO
				}
				filters := []string{"ipaddress:" + ipaddress}
				// node the latest node data associated with this report
				message := getNodeData(serviceClients, filters)
				message.Report = fullReport
				datafeedMessages[ipaddress] = message
			}

		}
		page++
		query = &reporting.Query{
			Page:    page,
			PerPage: dataFeedConfig.ServiceConfig.ReportsPageSize,
			Filters: filters}
		reports, err = serviceClients.reporting.ListReports(context.Background(), query)
		if err != nil {
			log.Errorf("Error getting reporting/ListReports %v", err)
		}
	}
}
