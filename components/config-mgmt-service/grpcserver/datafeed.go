package grpcserver

import (
	"context"
	"encoding/json"
	"fmt"
	"net"
	"strings"
	"time"

	pRequest "github.com/chef/automate/api/interservice/cfgmgmt/request"
	interserviceResp "github.com/chef/automate/api/interservice/cfgmgmt/response"
	"github.com/golang/protobuf/ptypes"
	log "github.com/sirupsen/logrus"
)

type Datafeed struct {
	FeedStart time.Time
	FeedEnd   time.Time
}

type Animal int

func (s *CfgMgmtServer) Getdata(ctx context.Context, req *pRequest.GetPaginationRequest) (*interserviceResp.GetPaginationResponse, error) {

	params := Datafeed{}
	feedStartString, err := ptypes.TimestampProto(params.FeedStart)
	if err != nil {
		return nil, err
	}
	feedEndString, err := ptypes.TimestampProto(params.FeedEnd)
	if err != nil {
		return nil, err
	}

	nodesRequest := &pRequest.InventoryNodes{
		PageSize: 300,
		Start:    feedStartString,
		End:      feedEndString,
		Sorting: &pRequest.Sorting{
			Order: pRequest.Order_DESC,
		},
	}

	inventoryNodes, err := s.GetInventoryNodes(ctx, nodesRequest)
	if err != nil {
		return nil, err
	}
	out := map[string]interface{}{}
	// data := datafeedServer.paginationData.Data
	out["offset"] = req.GetOffset()
	out["size"] = req.GetSize()
	out["inventoryNodes"] = inventoryNodes
	outputJSON, _ := json.Marshal(out)
	returnData := interserviceResp.GetPaginationResponse{
		Data: 	string(outputJSON),
	}
	return &returnData, nil
}

type NodeIDs struct {
	ClientID     string
	ComplianceID string
}

func (s *CfgMgmtServer) FetchCompliancedata(ctx context.Context, req *pRequest.GetPaginationRequest) (*interserviceResp.GetPaginationResponse, error) {

	params := Datafeed{}
	feedStartString, err := ptypes.TimestampProto(params.FeedStart)
	if err != nil {
		return nil, err
	}
	feedEndString, err := ptypes.TimestampProto(params.FeedEnd)
	if err != nil {
		return nil, err
	}

	nodesRequest := &pRequest.InventoryNodes{
		PageSize: req.Size,
		From:     req.Offset,
		Start:    feedStartString,
		End:      feedEndString,
		Sorting: &pRequest.Sorting{
			Order: pRequest.Order_DESC,
		},
	}

	inventoryNodes, err := s.GetInventoryNodes(ctx, nodesRequest)
	if err != nil {
		return nil, err
	}

	ipnet := make([]*net.IPNet, 1)
	nodeIDs := make(map[string]NodeIDs, 0)

	ipnet[0] = &net.IPNet{IP: []byte("0.0.0.0"), Mask: []byte("0")}

	fmt.Println("length ::::::::", len(inventoryNodes.Nodes), ipnet)

	if len(inventoryNodes.Nodes) > 0 {
		for _, node := range inventoryNodes.Nodes {
			fmt.Println("node ::::::::", node)
			if true {
				fmt.Println("inside if ::::::::")
				nodeIDs[node.Ipaddress] = NodeIDs{ClientID: node.Id}
			}
			fmt.Println("outside if ::::::::")
		}
		// lastNode := inventoryNodes.Nodes[len(inventoryNodes.Nodes)-1]
		// nodesRequest.CursorId = lastNode.Id
		// nodesRequest.CursorDate = lastNode.Checkin

		// inventoryNodes, err = s.GetInventoryNodes(ctx, nodesRequest)
		// log.Debugf("inventory nodes %v, cursor %v", len(inventoryNodes.Nodes), lastNode.Id)
		// if err != nil {
		// 	return nil, err
		// }
	}

	fmt.Println("::::::::::::", nodeIDs, "......", ipnet)

	nodeMessages := make(map[string]map[string]interface{})
	for resourceId, nodeID := range nodeIDs {
		if nodeID.ClientID == "" && nodeID.ComplianceID == "" {
			continue
		}
		// if resourceId is an IP address we get client data returned
		nodeData, err := getNodeClientData(ctx, resourceId, nodeID, false, s)
		if err != nil {
			log.Warnf("Error getting node data %v", err)
		}

		nodeMessages[resourceId] = nodeData
	}
	out := map[string]interface{}{}
	// // data := datafeedServer.paginationData.Data
	out["offset"] = req.Offset
	out["size"] = req.Size
	// out["Attribute"] = req.Attribute
	// out["inventoryNodes"] = inventoryNodes
	out["nodeMessages"] = nodeMessages
	// {Data: string(outputJSON)}
	outputJSON, _ := json.Marshal(out)
	// var zoo []Animal
	// if err := json.Unmarshal([]byte(outputJSON), &zoo); err != nil {
	// 	log.Fatal(err)
	// }
	returnData := interserviceResp.GetPaginationResponse{
		Data:  	string(outputJSON),
	}
	return &returnData, nil
}

func getNodeClientData(ctx context.Context, resourceId string, nodeID NodeIDs, updatedNodesOnly bool, s *CfgMgmtServer) (map[string]interface{}, error) {

	nodeData := make(map[string]interface{})
	var ipaddress string
	if isIPAddress(resourceId) {
		ipaddress = resourceId
	}

	var err error
	if nodeID.ClientID != "" || updatedNodesOnly == false {
		// get full node data
		log.Debugf("get full node data for NodeID  %v", nodeID)
		var filters []string
		if nodeID.ClientID != "" {
			filters = []string{"id:" + nodeID.ClientID}
		} else if isIPAddress(resourceId) {
			filters = []string{"ipaddress:" + ipaddress}
		}
		// get the attributes and last client run data of each node
		nodeData, err = safeGetNodeData(ctx, filters, s)

	} else if nodeID.ClientID == "" && updatedNodesOnly && isIPAddress(resourceId) {
		// get hosts data
		filters := []string{"ipaddress:" + ipaddress}
		_, macAddress, hostname, err := getNodeHostFields(ctx, s, filters)
		if err != nil {
			log.Warnf("Error getting node macaddress and hostname %v", err)
		}
		nodeDataContent := make(map[string]interface{})
		nodeDataContent["macaddress"] = macAddress
		nodeDataContent["hostname"] = hostname
		nodeDataContent["ipaddress"] = ipaddress
		nodeDataContent["automate_fqdn"] = "d.externalFqdn"
		nodeData["node"] = nodeDataContent

		// check dummy
	}
	if err != nil {
		return nodeData, err
	}
	return nodeData, nil
}

func safeGetNodeData(ctx context.Context, filters []string, s *CfgMgmtServer) (map[string]interface{}, error) {
	if len(filters) == 0 {
		return make(map[string]interface{}), nil
	}
	return getNodeData(ctx, filters, s)
}

func getNodeData(ctx context.Context, filters []string, s *CfgMgmtServer) (map[string]interface{}, error) {

	nodeData := make(map[string]interface{})
	client := s
	nodeId, lastRunId, err := getNodeFields(ctx, s, filters)
	if err != nil {
		return nodeData, err
	}

	attributesJson, err := getNodeAttributes(ctx, s, nodeId)
	if err != nil {
		return nodeData, err
	}

	nodeData["attributes"] = attributesJson

	lastRun, err := client.GetNodeRun(ctx, &pRequest.NodeRun{NodeId: nodeId, RunId: lastRunId})

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
	nodeDataContent["automate_fqdn"] = "d.externalFqdn"
	addDataContent(nodeDataContent, automaticAttrs)
	nodeData["node"] = nodeDataContent
	return nodeData, nil
}

func addDataContent(nodeDataContent map[string]interface{}, attributes map[string]interface{}) {
	os, _ := attributes["os"].(string)
	if strings.ToLower(os) == "windows" {
		kernel, ok := attributes["kernel"].(map[string]interface{})
		if !ok {
			nodeDataContent["serial_number"] = ""
			nodeDataContent["os_service_pack"] = ""
			return
		}

		osInfo, ok := kernel["os_info"].(map[string]interface{})
		if !ok {
			nodeDataContent["serial_number"] = ""
			nodeDataContent["os_service_pack"] = ""
			return
		}
		nodeDataContent["serial_number"] = osInfo["serial_number"]
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

func getNodeHostFields(ctx context.Context, client *CfgMgmtServer, filters []string) (string, string, string, error) {
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

func getNodeAttributes(ctx context.Context, client *CfgMgmtServer, nodeId string) (map[string]interface{}, error) {

	attributesJson := make(map[string]interface{})

	nodeAttributes, err := client.GetAttributes(ctx, &pRequest.Node{NodeId: nodeId})
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

func getNodeFields(ctx context.Context, s *CfgMgmtServer, filters []string) (string, string, error) {

	nodeFilters := &pRequest.Nodes{Filter: filters}
	nodes, err := s.GetNodes(ctx, nodeFilters)
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

func isIPAddress(resourceId string) bool {
	if net.ParseIP(resourceId) != nil {
		return true
	}
	return false
}

func includeNode(ipaddress string, cidrFilters []*net.IPNet) bool {
	// if d.disableCIDRFilter {
	// 	return true
	// }
	for _, ipNet := range cidrFilters {
		if ipNet.Contains(net.ParseIP(ipaddress)) {
			return true
		}
	}
	return false
}
