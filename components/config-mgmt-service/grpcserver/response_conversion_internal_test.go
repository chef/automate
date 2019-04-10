package grpcserver

import (
	"testing"

	"github.com/chef/automate/api/interservice/cfgmgmt/response"
	ingestBackend "github.com/chef/automate/components/ingest-service/backend"
	stpb "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"
)

func TestEmptyChefErrorToResponseError(t *testing.T) {
	emptyChefError := ingestBackend.ChefError{}
	expectedResponse := new(response.ChefError)
	gotResponse, err := toResponseError(emptyChefError)
	assert.Nil(t, err)
	assert.ObjectsAreEqual(expectedResponse, gotResponse)
	assert.Equal(t, expectedResponse, gotResponse)
}

func TestPartialChefErrorToResponseError(t *testing.T) {
	partialChefError := ingestBackend.ChefError{
		Class:     "class",
		Message:   "message",
		Backtrace: []string{"backtrace"},
	}

	expectedResponse := new(response.ChefError)
	expectedResponse.Class = "class"
	expectedResponse.Message = "message"
	expectedResponse.Backtrace = []string{"backtrace"}

	gotResponse, err := toResponseError(partialChefError)
	assert.Nil(t, err)
	assert.ObjectsAreEqual(expectedResponse, gotResponse)
	assert.Equal(t, expectedResponse, gotResponse)
}

func TestComplexChefErrorToResponseError(t *testing.T) {
	complexChefError := ingestBackend.ChefError{
		Class:     "class",
		Message:   "message",
		Backtrace: []string{"backtrace"},
		Description: struct {
			Title    string                   `json:"title"`
			Sections []map[string]interface{} `json:"sections"`
		}{
			Title: "title",
			Sections: []map[string]interface{}{
				map[string]interface{}{
					"Chef::Exceptions":        "Text text text",
					"Chef::Super::Stacktrace": "lalala",
				},
			},
		},
	}

	expectedResponse := &response.ChefError{
		Class:     "class",
		Message:   "message",
		Backtrace: []string{"backtrace"},
		Description: &response.Description{
			Title: "title",
			Sections: []*stpb.Struct{&stpb.Struct{
				Fields: map[string]*stpb.Value{
					"Chef::Exceptions":        {Kind: &stpb.Value_StringValue{"Text text text"}},
					"Chef::Super::Stacktrace": {Kind: &stpb.Value_StringValue{"lalala"}},
				},
			}},
		},
	}

	gotResponse, err := toResponseError(complexChefError)
	assert.Nil(t, err)
	assert.ObjectsAreEqual(expectedResponse, gotResponse)
	assert.Equal(t, expectedResponse, gotResponse)
}
