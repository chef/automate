package integration_test

import (
	"context"
	"testing"

	"github.com/chef/automate/api/interservice/compliance/stats"
	"github.com/sirupsen/logrus"
)


func TestNodeControl(t *testing.T) {
	statsServer := setupReadSummary(t)
	octoberTwentyFifthQuery := &stats.Query{
		Filters: []*stats.ListFilter{
			{Type: "end_time", Values: []string{"2018-10-25T23:59:59Z"}},
		},
	}
	logrus.Print(statsServer.ReadSummary(context.Background(), octoberTwentyFifthQuery))
}