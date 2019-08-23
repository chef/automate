package notifier

import (
	"context"
	"io/ioutil"
	"path"
	"strings"
	"testing"
	"time"

	"github.com/golang/protobuf/jsonpb"
	proto "github.com/golang/protobuf/proto"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/external/ingest/request"
	. "github.com/chef/automate/components/notifications-client/api"
	"github.com/chef/automate/components/notifications-client/builder"
)

const URL = "http://localhost"

type NotifyFunc = func(ctx context.Context, in *Event, opts ...grpc.CallOption) (*Response, error)

type simpleRPCMock struct {
	notifyFunc NotifyFunc
}

func (m *simpleRPCMock) Notify(ctx context.Context, in *Event,
	opts ...grpc.CallOption) (*Response, error) {

	return m.notifyFunc(ctx, in, opts...)
}

func (m *simpleRPCMock) AddRule(ctx context.Context, in *Rule, opts ...grpc.CallOption) (*RuleAddResponse, error) {
	return nil, nil
}

func (m *simpleRPCMock) DeleteRule(ctx context.Context, in *RuleIdentifier, opts ...grpc.CallOption) (*RuleDeleteResponse, error) {
	return nil, nil
}

func (m *simpleRPCMock) UpdateRule(ctx context.Context, in *Rule, opts ...grpc.CallOption) (*RuleUpdateResponse, error) {
	return nil, nil
}

func (m *simpleRPCMock) GetRule(ctx context.Context, in *RuleIdentifier, opts ...grpc.CallOption) (*RuleGetResponse, error) {
	return nil, nil
}

func (m *simpleRPCMock) ListRules(ctx context.Context, in *Empty, opts ...grpc.CallOption) (*RuleListResponse, error) {
	return nil, nil
}

func (m *simpleRPCMock) Version(ctx context.Context, in *VersionRequest, opts ...grpc.CallOption) (*VersionResponse, error) {
	return nil, nil
}

func (m *simpleRPCMock) ValidateWebhook(ctx context.Context, in *URLValidationRequest, opts ...grpc.CallOption) (*URLValidationResponse, error) {
	return nil, nil
}

// TestConcurrencyOne tests that setting concurrency to more than 1
// works correctly. We will send 2 events sent with 1 worker, meaning
// there will be one event in the backlog.
func TestConcurrencyOne(t *testing.T) {
	ctx, cancel := defaultContext()
	defer cancel()
	mock := mockClient(waitOnContextCancel(ctx))
	client := NewWithClient(mock, WithConcurrency(1))
	ev := mockEvent()

	client.Send(context.Background(), ev)
	client.Send(context.Background(), ev)

	waitForSteadyState()
	assert.Equal(t, 1, client.QueueSize())
}

// TestConcurrencyMany tests that setting concurrency to more than 1
// works correctly. We will have 2 workers, and send 2 events, meaning
// the backlog will be empty
func TestConcurrencyMany(t *testing.T) {
	ctx, cancel := defaultContext()
	defer cancel()
	mock := mockClient(waitOnContextCancel(ctx))
	client := NewWithClient(mock, WithConcurrency(2))
	ev := mockEvent()

	client.Send(context.Background(), ev)
	client.Send(context.Background(), ev)

	waitForSteadyState()
	assert.Equal(t, 0, client.QueueSize())
}

// TestBacklog tests that the backlog is working correctly. We will
// set the backlog to 1 and have 1 worker. 3 events will be sent that
// will cause the worker to block on the first one. The second event
// will be placed in the backlog, and the final event will be dropped
func TestBacklog(t *testing.T) {
	ctx, cancel := defaultContext()
	defer cancel()
	mock := mockClient(waitOnContextCancel(ctx))
	client := NewWithClient(mock, WithConcurrency(1), WithBacklog(1))
	ev := mockEvent()

	client.Send(context.Background(), ev)
	// Make sure the event gets picked up by the worker
	waitForSteadyState()

	client.Send(context.Background(), ev)
	// We expect this to be dropped
	client.Send(context.Background(), ev)

	waitForSteadyState()
	assert.Equal(t, 1, client.QueueSize())
}

func waitForSteadyState() {
	// We want to sleep on the test thread before doing our asserts so
	// that the checks happen after a steady state has been reached. The
	// other option is to use WaitGroups to try to check if the goroutines
	// you expected to start started. However it doesn't help making sure
	// goroutines you didn't expect to start didn't start.
	time.Sleep(10 * time.Millisecond)
}

func mockClient(doNotify NotifyFunc) *simpleRPCMock {
	return &simpleRPCMock{
		notifyFunc: doNotify,
	}
}

func waitOnContextCancel(ctx context.Context) NotifyFunc {
	return func(_ context.Context, in *Event, opts ...grpc.CallOption) (*Response, error) {
		<-ctx.Done()
		return nil, nil
	}
}

func mockEvent() *Event {
	var run request.Run
	// This test has 1 profile, with 2 controls. 1 of those controls
	// has 1 of 3 failing tests.

	content := `
	{
		"chef_server_fqdn": "chef-server.insights.co",
		"entity_uuid": "0271e125-97dd-498a-b026-8448ee60aafe",
		"node_name": "insights.chef.co",
		"id": "e2d05569-dfeb-4ff0-a5a2-863ce12cc13f",
		"message_version": "1.0.0",
		"message_type": "run_converge",
		"start_time": "2016-06-28T15:13:21Z",
		"end_time": "2016-06-28T15:13:22Z",
		"source": "chef_delivery",
		"status": "success",
		"total_resource_count": 10,
		"updated_resource_count": 4
	}
	`

	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	err := unmarshaler.Unmarshal(strings.NewReader(content), &run)

	if err != nil {
		panic(err)
	}

	ev, err := builder.ChefClientConverge(URL, &run)

	if err != nil {
		panic(err)
	}

	return ev
}

func defaultContext() (context.Context, context.CancelFunc) {
	return context.WithTimeout(context.Background(), 5*time.Second)
}

func from_json(name string, out proto.Message) error {
	p := path.Join("testdata", name)

	content, err := ioutil.ReadFile(p)
	if err != nil {
		panic(err)
	}

	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	err = unmarshaler.Unmarshal(strings.NewReader(string(content)), out)

	if err != nil {
		panic(err)
	}

	return nil
}
