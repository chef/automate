package ingestic

import (
	"context"
	"log"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	// "github.com/olivere/elastic/v7"
	elastic "github.com/olivere/elastic/v7"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type Service interface {
	GetLog(app string, lines int) ([]string, error)
}

// func NewService(url string) (Service, error) {
// 	client, err := elastic.NewSimpleClient(elastic.SetURL(url))
// 	if err != nil {
// 		return nil, err
// 	}
// 	return &service{elasticClient: client}, nil
// }

func ElasticClient(url string) (*elastic.Client, error) {
	client, err := elastic.NewSimpleClient(elastic.SetURL(url))
	if err != nil {
		return nil, err
	}
	return client, nil
}

//

func TestESClient_UpdateDayLatestToFalse(t *testing.T) {
	handler := http.NotFound
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		handler(w, r)
	}))
	defer ts.Close()

	handler = func(w http.ResponseWriter, r *http.Request) {
		resp := `{}`
		w.Write([]byte(resp))
	}
	esClient, err := ElasticClient(ts.URL)
	require.NoError(t, err)
	require.NotEmpty(t, esClient)

	newEsClient := NewESClient(esClient)
	assert.NoError(t, err)

	type fields struct {
		client      *ESClient
		initialized bool
	}
	type args struct {
		ctx      context.Context
		nodeId   string
		reportId string
		index    string
		mapping  mappings.Mapping
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
		{
			name:   "Test 1",
			fields: fields{newEsClient, false},
			args: args{
				ctx:      context.Background(),
				nodeId:   "8c3b77ec-8eea-3af0-805c-e40a3a62087b",
				reportId: "5e1c2718-e405-4847-9119-cf79faa2c350",
				index:    "comp-7-s-2022.06.23",
				mapping:  mappings.ComplianceRepDate,
			},
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			backend := newEsClient
			if err := backend.UpdateDayLatestToFalse(tt.args.ctx, tt.args.nodeId, tt.args.reportId, tt.args.index, tt.args.mapping); (err != nil) != tt.wantErr {

				t.Errorf("ESClient.UpdateDayLatestToFalse() error = %v, wantErr %v", err, tt.wantErr)
			}
			log.Printf("Error: %+v", err)
		})
	}
}
