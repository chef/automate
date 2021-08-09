package service

import (
	"net/http"
	"net/http/httptest"

	"github.com/chef/automate/components/data-feed-service/config"

	cfgmgmtResponse "github.com/chef/automate/api/interservice/cfgmgmt/response"
	cfgmgmt "github.com/chef/automate/api/interservice/cfgmgmt/service"
	"github.com/golang/mock/gomock"

	"context"
	"errors"
	"testing"

	structpb "github.com/golang/protobuf/ptypes/struct"
)

const hostname = "test.chef.com"
const ipAttr = "172.18.2.110"
const macAttr = "0A:B1:4A:DB:01:C5"
const hostAttr = "datafeed.test.com"
const attrNodeId = "attr-node-id"
const attrNodeName = "attr-node-name"
const attrChefEnv = "attr-chef_env"
const mockErrMsg = "Mock error message"
const nodeRunId = "node-run-id"
const serialNumber = "serial-number"
const servicePackMajorVersion float64 = 2
const servicePackMinorVersion float64 = 1
const servicePack = "2.1"
const externalFqdn = "https://datafeed.automate.com"

var attrRunList = []string{"recipe1", "recipe2"}
var mockErr = errors.New(mockErrMsg)
var mockAttrString = "{\"foo\":\"bar\"}"
var mockAttrs = map[string]string{"foo": "bar"}
var automaticAttrs = "{\"dmi\":{\"system\":{\"serial_number\":\"serial-number\"}},\"hostname\":\"test.chef.com\",\"hostnamectl\":{\"operating_system\":\"ubuntu\"},\"ipaddress\":\"172.18.2.120\",\"macaddress\":\"00:1C:42:C1:2D:87\",\"os\":\"linux\",\"os_version\":\"4.13.0-45-generic\"}"
var automaticAttrsWin = "{\"os\":\"windows\",\"kernel\":{\"os_info\":{\"serial_number\":\"serial-number\",\"service_pack_major_version\":2,\"service_pack_minor_version\":1}},\"dmi\":{\"system\":{\"serial_number\":\"serial-number\"}},\"hostname\":\"test.chef.com\",\"ipaddress\":\"172.18.2.120\",\"macaddress\":\"00:1C:42:C1:2D:87\"}"
var mockConfig = &config.DataFeedConfig{}
var acceptedStatusCodes []int32 = []int32{200, 201, 202, 203, 204}
var mockCredentials = NewBasicAuthCredentials("user", "pass")

func TestAssetCreated(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusAccepted)
	}))
	defer ts.Close()

	client := ts.Client()
	dataClient := DataClient{client: *client, acceptedStatusCodes: acceptedStatusCodes}
	notification := datafeedNotification{credentials: mockCredentials, url: ts.URL}
	err := send(dataClient, notification)
	if err != nil {
		t.Errorf("error got: %s, want nil", err)
	}
}

func TestSendError(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusBadRequest)
	}))
	defer ts.Close()

	client := ts.Client()
	dataClient := DataClient{client: *client}
	notification := datafeedNotification{credentials: mockCredentials, url: ts.URL}
	err := send(dataClient, notification)
	if err == nil {
		t.Error("error got: nil, wanted an error")
	}
}

func TestGetHostAttributesNil(t *testing.T) {
	attributes := make(map[string]interface{})
	ip, mac, hostname := getHostAttributes(attributes)
	if ip != "" {
		t.Logf("expected empty ip address got: %s", ip)
		t.Fail()
	}
	if mac != "" {
		t.Logf("expected empty mac address got: %s", mac)
		t.Fail()
	}
	if hostname != "" {
		t.Logf("expected empty hostanme got: %s", hostname)
		t.Fail()
	}
}

func TestGetHostAttributesEmpty(t *testing.T) {
	attributes := make(map[string]interface{})
	attributes["ipaddress"] = ""
	attributes["macaddress"] = ""
	attributes["hostname"] = ""
	ip, mac, hostname := getHostAttributes(attributes)
	if ip != "" {
		t.Logf("expected empty ip address got: %s", ip)
		t.Fail()
	}
	if mac != "" {
		t.Logf("expected empty mac address got: %s", mac)
		t.Fail()
	}
	if hostname != "" {
		t.Logf("expected empty hostanme got: %s", hostname)
		t.Fail()
	}
}

func TestGetHostAttributes(t *testing.T) {
	attributes := make(map[string]interface{})
	attributes["ipaddress"] = ipAttr
	attributes["macaddress"] = macAttr
	attributes["hostname"] = hostAttr
	ip, mac, hostname := getHostAttributes(attributes)
	if ip != ipAttr {
		t.Logf("expected empty ip address got: %s", ip)
		t.Fail()
	}
	if mac != macAttr {
		t.Logf("expected empty mac address got: %s", mac)
		t.Fail()
	}
	if hostname != hostAttr {
		t.Logf("expected empty hostname got: %s", hostname)
		t.Fail()
	}
}

func TestGetNodeAttributesMissing(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetAttributes(
		context.Background(),
		gomock.Any(),
	).Return(&cfgmgmtResponse.NodeAttribute{}, nil)
	attributesJson, err := getNodeAttributes(context.Background(), mockCfgMgmtClient, "")
	if attributesJson == nil {
		t.Log("expected empty attributesJson map got: nil")
		t.Fail()
	}
	verifyAttributesEmpty(attributesJson, t)
	if err != nil {
		t.Logf("expected nil error got: %v", err)
		t.Fail()
	}
}

func TestGetNodeAttributes(t *testing.T) {
	nodeAttributes := getAttributeResponse(automaticAttrs)
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetAttributes(
		context.Background(),
		gomock.Any(),
	).Return(nodeAttributes, nil)
	attributesJson, err := getNodeAttributes(context.Background(), mockCfgMgmtClient, "")
	if attributesJson == nil {
		t.Log("expected empty attributesJson map got: nil")
		t.Fail()
	}
	verifyAttributes(attributesJson, t)
	if err != nil {
		t.Logf("expected nil error got: %v", err)
		t.Fail()
	}
}

func TestGetNodeAttributesError(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetAttributes(
		context.Background(),
		gomock.Any(),
	).Return(&cfgmgmtResponse.NodeAttribute{}, mockErr)
	attributesJson, err := getNodeAttributes(context.Background(), mockCfgMgmtClient, "")
	if attributesJson == nil {
		t.Log("expected empty attributesJson map got: nil")
		t.Fail()
	}
	if len(attributesJson) != 0 {
		t.Logf("expected empty map, got %v", attributesJson)
		t.Fail()
	}
	if err != mockErr {
		t.Logf("expected error %v got: %v", mockErr, err)
		t.Fail()
	}
}

func TestGetNodeFieldsEmpty(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetNodes(
		context.Background(),
		gomock.Any(),
	).Return(&structpb.ListValue{}, nil)
	id, lastRunId, err := getNodeFields(context.Background(), mockCfgMgmtClient, []string{})
	if id != "" {
		t.Log("expected empty id, got: ''")
		t.Fail()
	}
	if lastRunId != "" {
		t.Log("expected empty lastRunId, got: ''")
		t.Fail()
	}

	if err != nil {
		t.Logf("expected error nil got: %v", err)
		t.Fail()
	}
}

func TestGetNodeFields(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()

	nodeId := &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: attrNodeId}}
	runId := &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: nodeRunId}}
	fields := make(map[string]*structpb.Value)
	fields["id"] = nodeId
	fields["latest_run_id"] = runId
	node := &structpb.Struct{Fields: fields}
	values := []*structpb.Value{&structpb.Value{Kind: &structpb.Value_StructValue{StructValue: node}}}
	nodeResponse := &structpb.ListValue{Values: values}
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetNodes(
		context.Background(),
		gomock.Any(),
	).Return(nodeResponse, nil)
	id, lastRunId, err := getNodeFields(context.Background(), mockCfgMgmtClient, []string{})
	if id != attrNodeId {
		t.Logf("expected id %v, got: %v", attrNodeId, id)
		t.Fail()
	}
	if lastRunId != nodeRunId {
		t.Logf("expected lastRunId %v, got: %v", nodeRunId, lastRunId)
		t.Fail()
	}

	if err != nil {
		t.Logf("expected error nil got: %v", err)
		t.Fail()
	}
}

func TestGetNodeFieldsError(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetNodes(
		context.Background(),
		gomock.Any(),
	).Return(&structpb.ListValue{}, mockErr)
	id, lastRunId, err := getNodeFields(context.Background(), mockCfgMgmtClient, []string{})
	if id != "" {
		t.Log("expected empty id, got: ''")
		t.Fail()
	}
	if lastRunId != "" {
		t.Log("expected empty lastRunId, got: ''")
		t.Fail()
	}

	if err == nil {
		t.Logf("expected error %v got: %v", mockErr, err)
		t.Fail()
	}
}

func TestGetNodeHostFieldsEmpty(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetNodes(
		context.Background(),
		gomock.Any(),
	).Return(&structpb.ListValue{}, nil)
	mockCfgMgmtClient.EXPECT().GetAttributes(
		context.Background(),
		gomock.Any(),
	).Return(&cfgmgmtResponse.NodeAttribute{}, nil)
	ipAddress, macAddress, hostname, err := getNodeHostFields(context.Background(), mockCfgMgmtClient, []string{})
	if ipAddress != "" {
		t.Logf("expected empty ipAddress, got: %v", ipAddress)
		t.Fail()
	}
	if macAddress != "" {
		t.Logf("expected empty macAddress, got: %v", macAddress)
		t.Fail()
	}
	if hostname != "" {
		t.Logf("expected empty hostname, got: %v", hostname)
		t.Fail()
	}

	if err != nil {
		t.Logf("expected error nil got: %v", err)
		t.Fail()
	}
}

func TestGetNodeHostFieldsNodeError(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetNodes(
		context.Background(),
		gomock.Any(),
	).Return(&structpb.ListValue{}, mockErr)
	ipAddress, macAddress, hostname, err := getNodeHostFields(context.Background(), mockCfgMgmtClient, []string{})
	if ipAddress != "" {
		t.Logf("expected empty ipAddress, got: %v", ipAddress)
		t.Fail()
	}
	if macAddress != "" {
		t.Logf("expected empty macAddress, got: %v", macAddress)
		t.Fail()
	}
	if hostname != "" {
		t.Logf("expected empty hostname, got: %v", hostname)
		t.Fail()
	}

	if err == nil {
		t.Logf("expected error, got nil")
		t.Fail()
	}
}

func TestGetNodeHostFieldsAttrError(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetNodes(
		context.Background(),
		gomock.Any(),
	).Return(&structpb.ListValue{}, nil)
	mockCfgMgmtClient.EXPECT().GetAttributes(
		context.Background(),
		gomock.Any(),
	).Return(&cfgmgmtResponse.NodeAttribute{}, mockErr)
	ipAddress, macAddress, hostname, err := getNodeHostFields(context.Background(), mockCfgMgmtClient, []string{})
	if ipAddress != "" {
		t.Logf("expected empty ipAddress, got: %v", ipAddress)
		t.Fail()
	}
	if macAddress != "" {
		t.Logf("expected empty macAddress, got: %v", macAddress)
		t.Fail()
	}
	if hostname != "" {
		t.Logf("expected empty hostname, got: %v", hostname)
		t.Fail()
	}

	if err == nil {
		t.Log("expected error, got nil")
		t.Fail()
	}
}

func TestAddDataContentEmpty(t *testing.T) {
	attributes := make(map[string]interface{})
	nodeDataContent := make(map[string]interface{})

	addDataContent(nodeDataContent, attributes)
	if len(nodeDataContent) != 1 {
		t.Log("expected nodeDataContent to have length 1")
		t.Fail()
	}
	if nodeDataContent["serial_number"].(string) != "" {
		t.Logf("expected serial number %s, got %v", "", nodeDataContent["serial_number"])
		t.Fail()
	}
}

func TestAddDataContentWindows(t *testing.T) {
	attributes := make(map[string]interface{})
	attributes["os"] = "windows"
	kernel := make(map[string]interface{})
	dmi := make(map[string]interface{})

	system := make(map[string]interface{})
	system["serial_number"] = serialNumber
	dmi["system"] = system

	osInfo := make(map[string]interface{})
	osInfo["service_pack_major_version"] = servicePackMajorVersion
	osInfo["service_pack_minor_version"] = servicePackMinorVersion
	kernel["os_info"] = osInfo
	attributes["kernel"] = kernel
	attributes["dmi"] = dmi

	nodeDataContent := make(map[string]interface{})

	t.Log(attributes)
	addDataContent(nodeDataContent, attributes)
	if len(nodeDataContent) != 2 {
		t.Log("expected nodeDataContent to have length 2")
		t.Fail()
	}
	if nodeDataContent["serial_number"].(string) != serialNumber {
		t.Logf("expected serial number %s, got %v", serialNumber, nodeDataContent["serial_number"])
		t.Fail()
	}
	if nodeDataContent["os_service_pack"].(string) != servicePack {
		t.Logf("expected service pack %s, got %v", servicePack, nodeDataContent["os_service_pack"])
		t.Fail()
	}
}

func TestAddDataContentWindowsNoSPMajorVer(t *testing.T) {
	attributes := make(map[string]interface{})
	attributes["os"] = "windows"
	kernel := make(map[string]interface{})
	osInfo := make(map[string]interface{})
	osInfo["serial_number"] = serialNumber
	osInfo["service_pack_minor_version"] = servicePackMinorVersion
	kernel["os_info"] = osInfo
	attributes["kernel"] = kernel

	dmi := make(map[string]interface{})
	system := make(map[string]interface{})
	system["serial_number"] = serialNumber
	dmi["system"] = system
	attributes["dmi"] = dmi

	nodeDataContent := make(map[string]interface{})

	addDataContent(nodeDataContent, attributes)
	if len(nodeDataContent) != 2 {
		t.Log("expected nodeDataContent to have length 2")
		t.Fail()
	}
	if nodeDataContent["serial_number"].(string) != serialNumber {
		t.Logf("expected serial number %s, got %v", serialNumber, nodeDataContent["serial_number"])
		t.Fail()
	}
	if nodeDataContent["os_service_pack"].(string) != "" {
		t.Logf("expected service pack '', got %v", nodeDataContent["os_service_pack"])
		t.Fail()
	}
}

func TestAddDataContentWindowsNoSPMinorVer(t *testing.T) {
	attributes := make(map[string]interface{})
	attributes["os"] = "windows"
	kernel := make(map[string]interface{})
	osInfo := make(map[string]interface{})
	osInfo["service_pack_major_version"] = servicePackMajorVersion
	kernel["os_info"] = osInfo
	attributes["kernel"] = kernel

	dmi := make(map[string]interface{})
	system := make(map[string]interface{})
	system["serial_number"] = serialNumber
	dmi["system"] = system
	attributes["dmi"] = dmi

	nodeDataContent := make(map[string]interface{})

	addDataContent(nodeDataContent, attributes)
	if len(nodeDataContent) != 2 {
		t.Log("expected nodeDataContent to have length 2")
		t.Fail()
	}
	if nodeDataContent["serial_number"].(string) != serialNumber {
		t.Logf("expected serial number %s, got %v", serialNumber, nodeDataContent["serial_number"])
		t.Fail()
	}
	if nodeDataContent["os_service_pack"].(string) != "" {
		t.Logf("expected service pack '', got %v", nodeDataContent["os_service_pack"])
		t.Fail()
	}
}

func TestAddDataContentWindowsEmptyKernelAndEmptyDmi(t *testing.T) {
	attributes := make(map[string]interface{})
	attributes["os"] = "windows"
	attributes["kernel"] = make(map[string]interface{})
	attributes["dmi"] = make(map[string]interface{})
	nodeDataContent := make(map[string]interface{})

	addDataContent(nodeDataContent, attributes)
	if len(nodeDataContent) != 2 {
		t.Log("expected nodeDataContent to have length 2")
		t.Fail()
	}
	if nodeDataContent["serial_number"].(string) != "" {
		t.Logf("expected serial number %s, got %v", "", nodeDataContent["serial_number"])
		t.Fail()
	}
	if nodeDataContent["os_service_pack"].(string) != "" {
		t.Logf("expected service pack '', got %v", nodeDataContent["os_service_pack"])
		t.Fail()
	}
}

func TestAddDataContentWindowsEmptyOsinfo(t *testing.T) {
	attributes := make(map[string]interface{})
	attributes["os"] = "windows"
	kernel := make(map[string]interface{})
	osInfo := make(map[string]interface{})
	kernel["os_info"] = osInfo
	attributes["kernel"] = kernel
	nodeDataContent := make(map[string]interface{})

	addDataContent(nodeDataContent, attributes)
	if len(nodeDataContent) != 2 {
		t.Log("expected nodeDataContent to have length 2")
		t.Fail()
	}
	if nodeDataContent["serial_number"].(string) != "" {
		t.Logf("expected serial number %s, got %v", "", nodeDataContent["serial_number"])
		t.Fail()
	}
	if nodeDataContent["os_service_pack"].(string) != "" {
		t.Logf("expected service pack '', got %v", nodeDataContent["os_service_pack"])
		t.Fail()
	}
}

func TestAddDataContentWindowsEmptySystem(t *testing.T) {
	attributes := make(map[string]interface{})
	attributes["os"] = "windows"
	dmi := make(map[string]interface{})
	system := make(map[string]interface{})
	dmi["system"] = system
	attributes["dmi"] = dmi
	nodeDataContent := make(map[string]interface{})

	addDataContent(nodeDataContent, attributes)
	if len(nodeDataContent) != 2 {
		t.Log("expected nodeDataContent to have length 2")
		t.Fail()
	}
	if nodeDataContent["serial_number"].(string) != "" {
		t.Logf("expected serial number %s, got %v", "", nodeDataContent["serial_number"])
		t.Fail()
	}
	if nodeDataContent["os_service_pack"].(string) != "" {
		t.Logf("expected service pack '', got %v", nodeDataContent["os_service_pack"])
		t.Fail()
	}
}

func TestAddDataContentWindowsEmpty(t *testing.T) {

	attributes := make(map[string]interface{})
	attributes["os"] = "windows"

	nodeDataContent := make(map[string]interface{})

	addDataContent(nodeDataContent, attributes)
	if len(nodeDataContent) != 2 {
		t.Log("expected nodeDataContent to have length 2")
		t.Fail()
	}
	if nodeDataContent["serial_number"].(string) != "" {
		t.Logf("expected serial number %s, got %v", "", nodeDataContent["serial_number"])
		t.Fail()
	}
	if nodeDataContent["os_service_pack"].(string) != "" {
		t.Logf("expected service pack %s, got %v", "", nodeDataContent["os_service_pack"])
		t.Fail()
	}
}

func TestAddDataContentNotWindows(t *testing.T) {
	attributes := make(map[string]interface{})
	attributes["os"] = "not windows"
	dmi := make(map[string]interface{})
	system := make(map[string]interface{})
	system["serial_number"] = serialNumber
	dmi["system"] = system
	attributes["dmi"] = dmi

	nodeDataContent := make(map[string]interface{})

	addDataContent(nodeDataContent, attributes)
	if len(nodeDataContent) != 1 {
		t.Log("expected nodeDataContent to have length 1")
		t.Fail()
	}
	if nodeDataContent["serial_number"].(string) != serialNumber {
		t.Logf("expected serial number %s, got %v", serialNumber, nodeDataContent["serial_number"])
		t.Fail()
	}
}

func TestAddDataContentNotWindowsEmpty(t *testing.T) {
	attributes := make(map[string]interface{})
	attributes["os"] = "not windows"

	nodeDataContent := make(map[string]interface{})

	addDataContent(nodeDataContent, attributes)
	if len(nodeDataContent) != 1 {
		t.Log("expected nodeDataContent to have length 1")
		t.Fail()
	}
	if nodeDataContent["serial_number"].(string) != "" {
		t.Logf("expected serial number %s, got %v", "", nodeDataContent["serial_number"])
		t.Fail()
	}
}

func TestGetNodeDataEmpty(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetNodes(
		context.Background(),
		gomock.Any(),
	).Return(&structpb.ListValue{}, nil)
	mockCfgMgmtClient.EXPECT().GetAttributes(
		context.Background(),
		gomock.Any(),
	).Return(&cfgmgmtResponse.NodeAttribute{}, nil)
	mockCfgMgmtClient.EXPECT().GetNodeRun(
		context.Background(),
		gomock.Any(),
	).Return(&cfgmgmtResponse.Run{}, nil)
	aggTask := &DataFeedAggregateTask{cfgMgmt: mockCfgMgmtClient, externalFqdn: externalFqdn}
	nodeData, err := aggTask.getNodeData(context.Background(), []string{})
	if nodeData["attributes"] == nil {
		t.Log("expected attributes, got nil")
		t.Fail()
	}
	attributesJson := nodeData["attributes"].(map[string]interface{})
	verifyAttributesEmpty(attributesJson, t)
	if err != nil {
		t.Logf("expected nil error, got %v", err)
		t.Fail()
	}

	node := nodeData["node"].(map[string]interface{})
	if len(node) != 5 {
		t.Logf("excpected 4 entries in node got %v", node)
	}
	if node["ipaddress"].(string) != "" {
		t.Logf("expected empty ipaddress, got %v", node["ipaddress"])
		t.Fail()
	}
	if node["macaddress"].(string) != "" {
		t.Logf("expected empty macaddress, got %v", node["macaddress"])
		t.Fail()
	}
	if node["hostname"].(string) != "" {
		t.Logf("expected empty hostname, got %v", node["hostname"])
		t.Fail()
	}
	if node["serial_number"].(string) != "" {
		t.Logf("expected empty serial_number, got %v", node["serial_number"])
		t.Fail()
	}
	if node["automate_fqdn"].(string) != externalFqdn {
		t.Logf("expected fqdn %v, got %v", externalFqdn, node["automate_fqdn"])
		t.Fail()
	}
}

func TestGetNodeDataFieldsError(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetNodes(
		context.Background(),
		gomock.Any(),
	).Return(&structpb.ListValue{}, mockErr)
	aggTask := &DataFeedAggregateTask{cfgMgmt: mockCfgMgmtClient, externalFqdn: externalFqdn}
	nodeData, err := aggTask.getNodeData(context.Background(), []string{})
	if len(nodeData) != 0 {
		t.Logf("expected empty node data map, got %v", nodeData)
		t.Fail()
	}
	if err != mockErr {
		t.Logf("expected error %v, got %v", mockErr, err)
		t.Fail()
	}
}

func TestGetNodeDataAttrsError(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetNodes(
		context.Background(),
		gomock.Any(),
	).Return(&structpb.ListValue{}, nil)
	mockCfgMgmtClient.EXPECT().GetAttributes(
		context.Background(),
		gomock.Any(),
	).Return(&cfgmgmtResponse.NodeAttribute{}, mockErr)
	aggTask := &DataFeedAggregateTask{cfgMgmt: mockCfgMgmtClient, externalFqdn: externalFqdn}
	nodeData, err := aggTask.getNodeData(context.Background(), []string{})
	if len(nodeData) != 0 {
		t.Logf("expected empty node data map, got %v", nodeData)
		t.Fail()
	}
	if err != mockErr {
		t.Logf("expected error %v, got %v", mockErr, err)
		t.Fail()
	}
}

func TestGetNodeDataRunError(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetNodes(
		context.Background(),
		gomock.Any(),
	).Return(&structpb.ListValue{}, nil)
	mockCfgMgmtClient.EXPECT().GetAttributes(
		context.Background(),
		gomock.Any(),
	).Return(&cfgmgmtResponse.NodeAttribute{}, nil)
	mockCfgMgmtClient.EXPECT().GetNodeRun(
		context.Background(),
		gomock.Any(),
	).Return(&cfgmgmtResponse.Run{}, mockErr)
	aggTask := &DataFeedAggregateTask{cfgMgmt: mockCfgMgmtClient, externalFqdn: externalFqdn}
	nodeData, err := aggTask.getNodeData(context.Background(), []string{})
	attributesJson := nodeData["attributes"].(map[string]interface{})
	verifyAttributesEmpty(attributesJson, t)
	if err != mockErr {
		t.Logf("expected error %v, got %v", mockErr, err)
		t.Fail()
	}

}

func TestGetNodeDataNotWindows(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	nodeAttributes := getAttributeResponse(automaticAttrs)
	run := getNodeRun()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetNodes(
		context.Background(),
		gomock.Any(),
	).Return(&structpb.ListValue{}, nil)
	mockCfgMgmtClient.EXPECT().GetAttributes(
		context.Background(),
		gomock.Any(),
	).Return(nodeAttributes, nil)
	mockCfgMgmtClient.EXPECT().GetNodeRun(
		context.Background(),
		gomock.Any(),
	).Return(run, nil)
	aggTask := &DataFeedAggregateTask{cfgMgmt: mockCfgMgmtClient, externalFqdn: externalFqdn}
	filters := []string{"ipaddress:" + ipAttr}
	nodeData, err := aggTask.safeGetNodeData(context.Background(), filters)
	if nodeData["attributes"] == nil {
		t.Log("expected attributes, got nil")
		t.Fail()
	}
	attributesJson := nodeData["attributes"].(map[string]interface{})
	verifyAttributes(attributesJson, t)
	if err != nil {
		t.Logf("expected nil error, got %v", err)
		t.Fail()
	}

	node := nodeData["node"].(map[string]interface{})
	if len(node) != 5 {
		t.Logf("excpected 4 entries in node got %v", node)
	}
	if node["ipaddress"].(string) != "172.18.2.120" {
		t.Logf("expected 172.18.2.120 ipaddress, got %v", node["ipaddress"])
		t.Fail()
	}
	if node["macaddress"].(string) != "00:1C:42:C1:2D:87" {
		t.Logf("expected 00:1C:42:C1:2D:87 macaddress, got %v", node["macaddress"])
		t.Fail()
	}
	if node["hostname"].(string) != "test.chef.com" {
		t.Logf("expected test.chef.com hostname, got %v", node["hostname"])
		t.Fail()
	}
	if node["serial_number"].(string) != serialNumber {
		t.Logf("expected empty serial_number, got %v", node["serial_number"])
		t.Fail()
	}
	if node["automate_fqdn"].(string) != externalFqdn {
		t.Logf("expected fqdn %v, got %v", externalFqdn, node["automate_fqdn"])
		t.Fail()
	}

}

func TestGetNodeDataWindows(t *testing.T) {
	ctrl := gomock.NewController(t)
	defer ctrl.Finish()
	nodeAttributes := getAttributeResponse(automaticAttrsWin)
	run := getNodeRun()
	mockCfgMgmtClient := cfgmgmt.NewMockCfgMgmtServiceClient(ctrl)
	mockCfgMgmtClient.EXPECT().GetNodes(
		context.Background(),
		gomock.Any(),
	).Return(&structpb.ListValue{}, nil)
	mockCfgMgmtClient.EXPECT().GetAttributes(
		context.Background(),
		gomock.Any(),
	).Return(nodeAttributes, nil)
	mockCfgMgmtClient.EXPECT().GetNodeRun(
		context.Background(),
		gomock.Any(),
	).Return(run, nil)
	aggTask := &DataFeedAggregateTask{cfgMgmt: mockCfgMgmtClient, externalFqdn: externalFqdn}
	nodeData, err := aggTask.getNodeData(context.Background(), []string{})
	if nodeData["attributes"] == nil {
		t.Log("expected attributes, got nil")
		t.Fail()
	}
	attributesJson := nodeData["attributes"].(map[string]interface{})
	verifyAttributes(attributesJson, t)
	if err != nil {
		t.Logf("expected nil error, got %v", err)
		t.Fail()
	}

	node := nodeData["node"].(map[string]interface{})
	if len(node) != 6 {
		t.Logf("expected 6 entries in node got %v", node)
	}
	if node["ipaddress"].(string) != "172.18.2.120" {
		t.Logf("expected 172.18.2.120 ipaddress, got %v", node["ipaddress"])
		t.Fail()
	}
	if node["macaddress"].(string) != "00:1C:42:C1:2D:87" {
		t.Logf("expected 00:1C:42:C1:2D:87 macaddress, got %v", node["macaddress"])
		t.Fail()
	}
	if node["hostname"].(string) != "test.chef.com" {
		t.Logf("expected test.chef.com hostname, got %v", node["hostname"])
		t.Fail()
	}
	if node["serial_number"].(string) != serialNumber {
		t.Logf("expected empty serial_number, got %v", node["serial_number"])
		t.Fail()
	}
	if node["os_service_pack"].(string) != servicePack {
		t.Logf("expected service pack %v, got %v", servicePack, node["os_service_pack"])
		t.Fail()
	}
	if node["automate_fqdn"].(string) != externalFqdn {
		t.Logf("expected fqdn %v, got %v", externalFqdn, node["automate_fqdn"])
		t.Fail()
	}
}

func verifyAttributes(attributesJson map[string]interface{}, t *testing.T) {
	verifyStringValue(attributesJson, "name", attrNodeName, t)
	verifyStringValue(attributesJson, "node_id", attrNodeId, t)
	verifyStringValue(attributesJson, "chef_environment", attrChefEnv, t)
	automatic := attributesJson["automatic"].(map[string]interface{})
	verifyStringValue(automatic, "ipaddress", "172.18.2.120", t)
	verifyStringValue(automatic, "macaddress", "00:1C:42:C1:2D:87", t)
	verifyStringValue(automatic, "hostname", hostname, t)
	verifyMapValue(attributesJson, "normal", mockAttrs, t)
	verifyMapValue(attributesJson, "default", mockAttrs, t)
	verifyMapValue(attributesJson, "override", mockAttrs, t)
	verifyInt32Value(attributesJson, "normal_value_count", 1, t)
	verifyInt32Value(attributesJson, "default_value_count", 2, t)
	verifyInt32Value(attributesJson, "override_value_count", 3, t)
	verifyInt32Value(attributesJson, "automatic_value_count", 4, t)
	verifyInt32Value(attributesJson, "all_value_count", 10, t)
	runList, ok := attributesJson["run_list"].([]string)
	if !ok {
		t.Log("expected run_list to be an array")
		t.Fail()
	}
	if len(runList) != 2 {
		t.Logf("expected len(run_list) to be 2, got %v", len(runList))
		t.Fail()
	}
	if runList[0] != "recipe1" {
		t.Logf("expected run_list[0] to be recipe1, got %v", runList[0])
		t.Fail()
	}
	if runList[1] != "recipe2" {
		t.Logf("expected run_list[1] to be recipe2, got %v", runList[1])
		t.Fail()
	}
}

func verifyAttributesEmpty(attributesJson map[string]interface{}, t *testing.T) {
	verifyStringValue(attributesJson, "name", "", t)
	verifyStringValue(attributesJson, "node_id", "", t)
	verifyStringValue(attributesJson, "chef_environment", "", t)
	verifyInt32Value(attributesJson, "normal_value_count", 0, t)
	verifyInt32Value(attributesJson, "default_value_count", 0, t)
	verifyInt32Value(attributesJson, "override_value_count", 0, t)
	verifyInt32Value(attributesJson, "automatic_value_count", 0, t)
	verifyInt32Value(attributesJson, "all_value_count", 0, t)
	verifyEmptyMap(attributesJson, "automatic", t)
	verifyEmptyMap(attributesJson, "default", t)
	verifyEmptyMap(attributesJson, "normal", t)
	verifyEmptyMap(attributesJson, "override", t)
	runList, ok := attributesJson["run_list"].([]string)
	if !ok {
		t.Log("expected run_list to be an array")
		t.Fail()
	}
	if len(runList) != 0 {
		t.Logf("expected run_list to be an empty array, got %v", runList)
		t.Fail()
	}
}

func verifyMapValue(attrs map[string]interface{}, key string, expected map[string]string, t *testing.T) {
	m := attrs[key].(map[string]interface{})
	if len(m) != len(expected) {
		t.Logf("expected map length %d, got %d", len(expected), len(m))
		t.Fail()
	}
	for ek, ev := range expected {
		v, ok := m[ek]
		if !ok {
			t.Logf("key %s not found in %v", ek, m)
			t.Fail()
		}
		if v.(string) != ev {
			t.Logf("expected %s, got %s", ev, v)
			t.Fail()
		}
	}
}

func verifyStringValue(m map[string]interface{}, k string, v string, t *testing.T) {
	if m[k].(string) != v {
		t.Logf("expected %s to be %s, got: %v", k, v, m[k])
		t.Fail()
	} else {
		t.Logf("expected %s, got %s", v, m[k])
	}
}

func verifyInt32Value(m map[string]interface{}, k string, v int32, t *testing.T) {
	if m[k].(int32) != v {
		t.Logf("expected %s to be 0, got: %v", k, m[k])
		t.Fail()
	}
}

func verifyEmptyMap(m map[string]interface{}, k string, t *testing.T) {
	if len(m[k].(map[string]interface{})) != 0 {
		t.Logf("expected %s to be empty map, got: %v", k, m[k])
		t.Fail()
	}
}

func getAttributeResponse(automaticAttributes string) *cfgmgmtResponse.NodeAttribute {
	nodeAttributes := &cfgmgmtResponse.NodeAttribute{
		NodeId:              attrNodeId,
		Name:                attrNodeName,
		RunList:             attrRunList,
		ChefEnvironment:     attrChefEnv,
		Normal:              mockAttrString,
		Default:             mockAttrString,
		Override:            mockAttrString,
		NormalValueCount:    1,
		DefaultValueCount:   2,
		OverrideValueCount:  3,
		AllValueCount:       10,
		Automatic:           automaticAttributes,
		AutomaticValueCount: 4,
	}
	return nodeAttributes
}

func getNodeRun() *cfgmgmtResponse.Run {
	run := &cfgmgmtResponse.Run{
		Id:                    nodeRunId,
		NodeId:                attrNodeId,
		NodeName:              attrNodeName,
		Organization:          "Organization",
		StartTime:             nil,
		EndTime:               nil,
		Source:                "Source",
		Status:                "Status",
		TotalResourceCount:    0,
		UpdatedResourceCount:  0,
		ChefVersion:           "15",
		UptimeSeconds:         0,
		Environment:           "Environment",
		Fqdn:                  "Fqdn",
		SourceFqdn:            "SourceFqdn",
		Ipaddress:             "Ipaddress",
		Resources:             nil,
		RunList:               nil,
		Deprecations:          nil,
		Error:                 nil,
		Tags:                  nil,
		ResourceNames:         nil,
		Recipes:               nil,
		ChefTags:              nil,
		Cookbooks:             nil,
		Platform:              "",
		PlatformFamily:        "",
		PlatformVersion:       "",
		Roles:                 nil,
		PolicyName:            "",
		PolicyGroup:           "",
		PolicyRevision:        "",
		ExpandedRunList:       nil,
		Projects:              nil,
		VersionedCookbooks:    nil,
		Ip6Address:            "",
		Timezone:              "",
		Domain:                "",
		Hostname:              hostname,
		MemoryTotal:           "",
		Macaddress:            "",
		DmiSystemSerialNumber: serialNumber,
		DmiSystemManufacturer: "",
		VirtualizationRole:    "",
		VirtualizationSystem:  "",
		KernelVersion:         "",
		KernelRelease:         "",
		CloudProvider:         "",
	}
	return run
}
