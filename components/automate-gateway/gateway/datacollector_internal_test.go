//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2018, Chef Software Inc.
//

package gateway

import (
	"bytes"
	"context"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"

	ingestReq "github.com/chef/automate/api/external/ingest/request"
	mock_compliance_ingest "github.com/chef/automate/components/automate-gateway/gateway_mocks/mock_compliance_ingest"
	mock_ingest "github.com/chef/automate/components/automate-gateway/gateway_mocks/mock_ingest"
	mock_notifier "github.com/chef/automate/components/automate-gateway/gateway_mocks/mock_notifier"
	"github.com/golang/mock/gomock"
	gp "github.com/golang/protobuf/ptypes/empty"
	structpb "github.com/golang/protobuf/ptypes/struct"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

func init() {
	loadRawExamples()
}

// newMockGatewayServerWithAuth Is a Slick Wrapper that injects Auth for this particular test suite
func newMockGatewayServerWithAuth(t *testing.T, services ...interface{}) Server {
	mockAuthClient, mockAuthzClient := newAuthorizationMocks(t, "ingest:unified_events", "create")

	// Append Auth/AuthZ to Authorize the above resource
	services = append(services, mockAuthClient, mockAuthzClient)

	return newMockGatewayServer(t, services...)
}

func TestDataCollectorHandlerUnknownMsg(t *testing.T) {
	// Create a new gateway.Server instance with mocked Clients
	subject := newMockGatewayServerWithAuth(t)

	body := []byte(`{"message_type": "mystery"}`)

	// Generate a mock Request & Response
	w, r := newResponseAndRequestFromBytes(body)

	// Call the subject handler
	subject.dataCollectorHandler(w, r)

	// Assert the ResponseWriter
	response := w.Result()
	assert.Equal(t, "400 Bad Request", response.Status)
	assert.Equal(t, http.StatusBadRequest, response.StatusCode)
}

func TestDataCollectorHandlerChefActionMsgOk(t *testing.T) {
	for a, body := range rawactions {
		t.Run("action_type: "+a, func(t *testing.T) {
			// Mock the IngestClient
			mockIngest := mock_ingest.NewMockChefIngesterClient(gomock.NewController(t))
			// Assert that we will call the ProcessChefAction() func
			mockIngest.EXPECT().ProcessChefAction(gomock.Any(), gomock.Any())

			// Create a new gateway.Server instance with mocked Clients
			subject := newMockGatewayServerWithAuth(t, mockIngest)

			// Generate a mock Request & Response
			w, r := newResponseAndRequestFromBytes(body)

			// Call the subject handler
			subject.dataCollectorHandler(w, r)

			// Assert the ResponseWriter
			response := w.Result()
			assert.Equal(t, "200 OK", response.Status)
			assert.Equal(t, http.StatusOK, response.StatusCode)
		})
	}
}

func TestDataCollectorHandlerMsgErrorWithNonGRPCError(t *testing.T) {
	// Mock the IngestClient to test  the behavior when we've got a Non GRPC Error
	mockIngest := mock_ingest.NewMockChefIngesterClient(gomock.NewController(t))
	mockIngest.EXPECT().ProcessChefAction(gomock.Any(), gomock.Any()).DoAndReturn(
		func(_ context.Context, _ *ingestReq.Action) (*gp.Empty, error) {
			return &gp.Empty{}, errors.New("A Non GRPC Error")
		},
	)

	body := []byte(`{
	"message_type": "action",
	"entity_name": ""
	}`)

	var (
		w, r    = newResponseAndRequestFromBytes(body)
		subject = newMockGatewayServerWithAuth(t, mockIngest)
	)

	subject.dataCollectorHandler(w, r)
	response := w.Result()

	// Non GRPC Error will be defaulted to Internal Server Error (500)
	assert.Equal(t, "500 Internal Server Error", response.Status)
	assert.Equal(t, http.StatusInternalServerError, response.StatusCode)
}

func TestDataCollectorHandlerChefActionMsgError(t *testing.T) {
	// Mock the IngestClient to assert that we've got an error calling ProcessChefAction() func
	mockIngest := mock_ingest.NewMockChefIngesterClient(gomock.NewController(t))
	mockIngest.EXPECT().ProcessChefAction(gomock.Any(), gomock.Any()).DoAndReturn(
		func(_ context.Context, _ *ingestReq.Action) (*gp.Empty, error) {
			return &gp.Empty{}, status.Error(codes.Internal, "Something happened")
		},
	)

	// Create a new gateway.Server instance with mocked Clients
	subject := newMockGatewayServerWithAuth(t, mockIngest)

	body := []byte(`{
	"message_type": "action",
	"entity_name": "ename"
	}`)

	// Generate a mock Request & Response
	w, r := newResponseAndRequestFromBytes(body)

	// Call the subject handler
	subject.dataCollectorHandler(w, r)

	// Assert the ResponseWriter
	response := w.Result()
	assert.Equal(t, "500 Internal Server Error", response.Status)
	assert.Equal(t, http.StatusInternalServerError, response.StatusCode)
}

func TestDataCollectorHandlerChefRunStartMsg(t *testing.T) {
	// Create a new gateway.Server instance with mocked Clients
	//
	// When we receive a ChefRunStartMsg we do not talk to ingest at all
	// and we do not notify, we simply drop the message so no need to mock
	subject := newMockGatewayServerWithAuth(t)

	body := []byte(`{
	"message_type": "run_start",
	"node_name": "test_node"
	}`)

	// Generate a mock Request & Response
	w, r := newResponseAndRequestFromBytes(body)

	// Call the subject handler
	subject.dataCollectorHandler(w, r)

	// Assert the ResponseWriter
	response := w.Result()
	assert.Equal(t, "200 OK", response.Status)
	assert.Equal(t, http.StatusOK, response.StatusCode)
}

func TestDataCollectorHandlerChefRunConvergeMsgOk(t *testing.T) {
	for r, body := range rawruns {
		t.Run("JSON file"+r, func(t *testing.T) {
			ctrl := gomock.NewController(t)
			// Mock the IngestClient
			mockIngest := mock_ingest.NewMockChefIngesterClient(ctrl)
			// Assert that we will call the ProcessChefRun() func
			mockIngest.EXPECT().ProcessChefRun(gomock.Any(), gomock.Any())

			// Mock the Notifier
			mockNotifier := mock_notifier.NewMockNotifier(ctrl)
			// Assert that we will call notifier.Send() func
			mockNotifier.EXPECT().Send(gomock.Any(), gomock.Any())

			// Create a new gateway.Server instance with mocked Clients
			subject := newMockGatewayServerWithAuth(t, mockIngest, mockNotifier)

			// Generate a mock Request & Response
			w, r := newResponseAndRequestFromBytes(body)

			// Call the subject handler
			subject.dataCollectorHandler(w, r)

			// Assert the ResponseWriter
			response := w.Result()
			assert.Equal(t, "200 OK", response.Status)
			assert.Equal(t, http.StatusOK, response.StatusCode)
		})
	}
}

func TestDataCollectorHandlerChefRunMsgError(t *testing.T) {
	ctrl := gomock.NewController(t)
	// Mock the IngestClient
	mockIngest := mock_ingest.NewMockChefIngesterClient(ctrl)
	// Assert that we've got an error calling ProcessChefRun() func
	mockIngest.EXPECT().ProcessChefRun(gomock.Any(), gomock.Any()).DoAndReturn(
		func(_ context.Context, _ *ingestReq.Run) (*gp.Empty, error) {
			return &gp.Empty{}, status.Error(codes.Internal, "Something happened")
		},
	)

	// Mock the Notifier
	mockNotifier := mock_notifier.NewMockNotifier(ctrl)
	// Assert that we will call notifier.Send() func
	mockNotifier.EXPECT().Send(gomock.Any(), gomock.Any())

	// Create a new gateway.Server instance with mocked Clients
	subject := newMockGatewayServerWithAuth(t, mockIngest, mockNotifier)

	body := []byte(`{
	"message_type": "run_converge",
	"node_name": "guacamole"
	}`)

	// Generate a mock Request & Response
	w, r := newResponseAndRequestFromBytes(body)

	// Call the subject handler
	subject.dataCollectorHandler(w, r)

	// Assert the ResponseWriter
	response := w.Result()
	assert.Equal(t, "500 Internal Server Error", response.Status)
	assert.Equal(t, http.StatusInternalServerError, response.StatusCode)
}

func TestDataCollectorHandlerLivenessAgentMsg(t *testing.T) {
	// Mock the IngestClient
	mockIngest := mock_ingest.NewMockChefIngesterClient(gomock.NewController(t))
	// Assert that we will call the ProcessLivenessPing() func
	mockIngest.EXPECT().ProcessLivenessPing(gomock.Any(), gomock.Any())

	// Create a new gateway.Server instance with mocked Clients
	subject := newMockGatewayServerWithAuth(t, mockIngest)

	body, err := ioutil.ReadFile("../../ingest-service/examples/liveness_ping.json")
	if err != nil {
		panic(err)
	}

	// Generate a mock Request & Response
	w, r := newResponseAndRequestFromBytes(body)

	// Call the subject handler
	subject.dataCollectorHandler(w, r)

	// Assert the ResponseWriter
	response := w.Result()
	assert.Equal(t, "200 OK", response.Status)
	assert.Equal(t, http.StatusOK, response.StatusCode)
}

func TestDataCollectorHandlerComplianceReportMsg(t *testing.T) {
	for r, body := range rawreports {
		t.Run("compliance_report: "+r, func(t *testing.T) {
			ctrl := gomock.NewController(t)
			// Mock the ComplianceIngester
			mockComplianceIngester := mock_compliance_ingest.NewMockComplianceIngesterClient(ctrl)
			// Assert that we will call the ProcessComplianceReport() func
			mockComplianceIngester.EXPECT().ProcessComplianceReport(gomock.Any(), gomock.Any())

			// Create a new gateway.Server instance with mocked Clients
			subject := newMockGatewayServerWithAuth(t, mockComplianceIngester)

			// Generate a mock Request & Response
			w, r := newResponseAndRequestFromBytes(body)

			// Call the subject handler
			subject.dataCollectorHandler(w, r)

			// Assert the ResponseWriter
			response := w.Result()
			assert.Equal(t, "200 OK", response.Status)
			assert.Equal(t, http.StatusOK, response.StatusCode)
		})
	}
}

func TestDataCollectorHandlerWithMalformedJSON(t *testing.T) {
	// Create a new gateway.Server instance with mocked Clients
	subject := newMockGatewayServerWithAuth(t)

	malformedJSON := []byte(`
	"above": "missing-bracket ^",
	"entity_name": "ename",
	"message_type": "action"
	}`)

	// Generate a mock Request & Response
	w, r := newResponseAndRequestFromBytes(malformedJSON)

	// Call the subject handler
	subject.dataCollectorHandler(w, r)

	// Assert the ResponseWriter
	response := w.Result()
	assert.Equal(t, "400 Bad Request", response.Status)
	assert.Equal(t, http.StatusBadRequest, response.StatusCode)
}

func TestDataCollectorHandlerWithPartialMalformedJSON(t *testing.T) {
	// Since the JSON is partially malformed, the json parser actually reads the right
	// field to determine what type of message is the request, and then sends it to the
	// ingest service for ingestion
	//
	// TODO @afiune how do we protect or behave on those situations?
	malformedJSON := []byte(`{
	"entity_name": "ename"
	"message_type": "action"
	"above": "missing-coma ^",
	}`)

	// Mock the IngestClient
	mockIngest := mock_ingest.NewMockChefIngesterClient(gomock.NewController(t))
	// Assert that we will call the ProcessChefAction() func and return and error
	mockIngest.EXPECT().ProcessChefAction(gomock.Any(), gomock.Any()).DoAndReturn(
		func(_ context.Context, _ *ingestReq.Action) (*gp.Empty, error) {
			return &gp.Empty{}, status.Error(codes.InvalidArgument, "Malformed JSON Message")
		},
	)
	// Create a new gateway.Server instance with mocked Clients
	subject := newMockGatewayServerWithAuth(t, mockIngest)

	// Generate a mock Request & Response
	w, r := newResponseAndRequestFromBytes(malformedJSON)

	// Call the subject handler
	subject.dataCollectorHandler(w, r)

	// Assert the ResponseWriter
	response := w.Result()
	assert.Equal(t, "400 Bad Request", response.Status)
	assert.Equal(t, http.StatusBadRequest, response.StatusCode)
}

// newResponseAndRequestFromBytes genarates a new Response and Request to be use in testings,
// it receives a body (in bytes) that will be injected to the Request
func newResponseAndRequestFromBytes(body []byte) (*httptest.ResponseRecorder, *http.Request) {
	r, err := http.NewRequest("POST", mockURL, bytes.NewReader(body))
	if err != nil {
		panic(err)
	}
	return httptest.NewRecorder(), r
}

func TestDataCollectorGetStructIfExistsWhenFieldIsAStringWithScapedChars(t *testing.T) {
	// Sometimes we have string fields that are treated as Structs,
	// we can unescape them and convert them
	bytes := []byte(`{"data":"{\"id\":\"1\"}"}`)
	pbStruct := getStructIfExists("data", bytes)

	expectedDataField := &structpb.Struct{
		Fields: map[string]*structpb.Value{
			"id": {Kind: &structpb.Value_StringValue{"1"}},
		},
	}
	assert.Equal(t, expectedDataField, pbStruct)
}

func TestDataCollectorGetStructIfExistsWhenFieldIsAJSONObject(t *testing.T) {
	bytes := []byte(`{"data":{"id":"1"}}`)
	pbStruct := getStructIfExists("data", bytes)

	expectedDataField := &structpb.Struct{
		Fields: map[string]*structpb.Value{
			"id": {Kind: &structpb.Value_StringValue{"1"}},
		},
	}
	assert.Equal(t, expectedDataField, pbStruct)
}

func TestDataCollectorGetStructIfExistsWhenFieldIsSomethingElseReturnEmptyStruct(t *testing.T) {
	// empty, we can't convert a boolean, an array or number into a Struct
	// TODO @afiune do we want to log or do something on these cases?
	expectedDataField := &structpb.Struct{}

	// Boolean
	bytes := []byte(`{"data":true}`)
	pbStruct := getStructIfExists("data", bytes)
	assert.Equal(t, expectedDataField, pbStruct)

	// For an 'Array' you can't use 'getStructIfExists()' you must use 'getStructArray()'
	bytes = []byte(`{"data":[{"id":"1"},{"id":"1"}]}`)
	pbStruct = getStructIfExists("data", bytes)
	assert.Equal(t, expectedDataField, pbStruct)

	// Number
	bytes = []byte(`{"data":1}`)
	pbStruct = getStructIfExists("data", bytes)
	assert.Equal(t, expectedDataField, pbStruct)

	// Normal strings are also not allowed, return empty as well
	bytes = []byte(`{"data":"Hi"}`)
	pbStruct = getStructIfExists("data", bytes)
	assert.Equal(t, expectedDataField, pbStruct)
}

func TestDataCollectorGetStructArrayWhenTheArrayAreObjects(t *testing.T) {
	bytes := []byte(`{"data":[{"id":"1"},{"id":"2"}]}`)
	arrayStruct := getStructArray("data", bytes)

	expectedDataField := []*structpb.Struct{
		&structpb.Struct{
			Fields: map[string]*structpb.Value{
				"id": {Kind: &structpb.Value_StringValue{"1"}},
			},
		},
		&structpb.Struct{
			Fields: map[string]*structpb.Value{
				"id": {Kind: &structpb.Value_StringValue{"2"}},
			},
		},
	}
	assert.Len(t, arrayStruct, 2)
	assert.ElementsMatch(t, expectedDataField, arrayStruct)
}

func TestDataCollectorGetStructArrayWhenTheArrayAreStrings(t *testing.T) {
	bytes := []byte(`{"data":["{\"id\":\"1\"}","{\"id\":\"2\"}"]}`)
	arrayStruct := getStructArray("data", bytes)

	expectedDataField := []*structpb.Struct{
		&structpb.Struct{
			Fields: map[string]*structpb.Value{
				"id": {Kind: &structpb.Value_StringValue{"1"}},
			},
		},
		&structpb.Struct{
			Fields: map[string]*structpb.Value{
				"id": {Kind: &structpb.Value_StringValue{"2"}},
			},
		},
	}
	assert.Len(t, arrayStruct, 2)
	assert.ElementsMatch(t, expectedDataField, arrayStruct)
}
