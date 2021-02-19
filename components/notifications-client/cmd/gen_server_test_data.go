package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"
	"strings"

	"github.com/golang/protobuf/jsonpb"
	"github.com/golang/protobuf/proto"
	pbtype "github.com/golang/protobuf/ptypes/struct"
	grpc "google.golang.org/grpc"

	chef "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	. "github.com/chef/automate/components/notifications-client/api"
	"github.com/chef/automate/components/notifications-client/builder"
	"github.com/chef/automate/components/notifications-client/notifier"
)

type CaptureClient struct {
	path string
	done chan bool
}

func (c *CaptureClient) Notify(ctx context.Context, in *Event, opts ...grpc.CallOption) (*Response, error) {
	data, err := proto.Marshal(in)

	if err != nil {
		panic(err)
	}

	err = ioutil.WriteFile(c.path, data, 0644)

	if err != nil {
		panic(err)
	}

	c.done <- true

	return nil, nil
}

func (c *CaptureClient) AddRule(ctx context.Context, in *Rule, opts ...grpc.CallOption) (*RuleAddResponse, error) {
	return nil, nil
}

func (c *CaptureClient) DeleteRule(ctx context.Context, in *RuleIdentifier, opts ...grpc.CallOption) (*RuleDeleteResponse, error) {
	return nil, nil
}

func (c *CaptureClient) UpdateRule(ctx context.Context, in *Rule, opts ...grpc.CallOption) (*RuleUpdateResponse, error) {
	return nil, nil
}

func (c *CaptureClient) GetRule(ctx context.Context, in *RuleIdentifier, opts ...grpc.CallOption) (*RuleGetResponse, error) {
	return nil, nil
}

func (c *CaptureClient) ListRules(ctx context.Context, in *Empty, opts ...grpc.CallOption) (*RuleListResponse, error) {
	return nil, nil
}

func (c *CaptureClient) Version(ctx context.Context, in *VersionRequest, opts ...grpc.CallOption) (*VersionResponse, error) {
	return nil, nil
}

func (c *CaptureClient) ValidateWebhook(ctx context.Context, in *URLValidationRequest, opts ...grpc.CallOption) (*URLValidationResponse, error) {
	return nil, nil
}

func fatal(msg string) {
	fmt.Fprintln(os.Stderr, msg)
	os.Exit(1)
}

func main() {
	if len(os.Args) != 3 {
		fatal("Incorrect number of args")
	}

	capturer := new(CaptureClient)
	capturer.path = os.Args[2]
	capturer.done = make(chan bool)
	url := "https://localhost"
	client := notifier.NewWithClient(capturer, notifier.WithBacklog(1), notifier.WithConcurrency(1))

	unknown := pbtype.Struct{}
	inB, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		fatal(err.Error())
	}
	in := string(inB)
	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	err = unmarshaler.Unmarshal(strings.NewReader(in), &unknown)
	if err != nil {
		fatal(err.Error())
	}

	var ev *Event
	_, exists := unknown.GetFields()["profiles"]
	if exists {
		report := compliance.Report{}
		err = unmarshaler.Unmarshal(strings.NewReader(in), &report)
		if err != nil {
			fatal(err.Error())
		}
		ev, err = builder.Compliance(url, &report)
		if err != nil {
			fatal(err.Error())
		}
	} else {
		_, exists = unknown.GetFields()["message_type"]
		if exists {
			run := chef.Run{}
			err = unmarshaler.Unmarshal(strings.NewReader(in), &run)
			if err != nil {
				fatal(err.Error())
			}
			ev, err = builder.ChefClientConverge(url, &run)
			if err != nil {
				fatal(err.Error())
			}
		} else {
			fatal("Unknown type")
		}
	}
	client.Send(context.Background(), ev)
	<-capturer.done
}
