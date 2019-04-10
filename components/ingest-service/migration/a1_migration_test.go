package migration_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/ingest-service/backend/elastic/mappings"
	subject "github.com/chef/automate/components/ingest-service/migration"
)

func TestRenameFromInsightsIndexName(t *testing.T) {
	cases := []struct {
		insightsIndexName string
		newIndexTagName   string
		expectedResult    string
	}{
		{
			insightsIndexName: "insights-2018.04.15",
			newIndexTagName:   mappings.ConvergeHistory.Index,
			expectedResult:    "converge-history-2018.04.15",
		},
		{
			insightsIndexName: "insights-2018.04.15",
			newIndexTagName:   mappings.Actions.Index,
			expectedResult:    "actions-2018.04.15",
		},
		{
			insightsIndexName: "insights-2018.04.15-1",
			newIndexTagName:   mappings.ConvergeHistory.Index,
			expectedResult:    "converge-history-2018.04.15",
		},
		{
			insightsIndexName: "insights-2018.04.15-1",
			newIndexTagName:   mappings.Actions.Index,
			expectedResult:    "actions-2018.04.15",
		},
		{
			insightsIndexName: "insights-2018.04.15-b",
			newIndexTagName:   mappings.ConvergeHistory.Index,
			expectedResult:    "converge-history-2018.04.15",
		},
		{
			insightsIndexName: "insights-2018.04.15-b",
			newIndexTagName:   mappings.Actions.Index,
			expectedResult:    "actions-2018.04.15",
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("with insightsIndexName = %s and newIndexTagName = %s it should = %s",
			test.insightsIndexName, test.newIndexTagName, test.expectedResult), func(t *testing.T) {

			result := subject.RenameFromInsightsIndexName(test.insightsIndexName, test.newIndexTagName)
			assert.Equal(t, test.expectedResult, result)
		})
	}

}
