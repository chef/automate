package relaxting

import (
	"encoding/json"
	"github.com/chef/automate/api/interservice/compliance/stats"
	"github.com/olivere/elastic/v7"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestGetControlSummaryResult(t *testing.T) {

	controlSummary := &stats.ControlsSummary{
		Passed:    2,
		Skipped:   14,
		Failures:  1,
		Waived:    0,
		Criticals: 1,
		Majors:    2,
		Minors:    4,
	}

	esr := ES2Backend{
		ESUrl:             "",
		Enterprise:        "",
		ChefDeliveryUser:  "",
		ChefDeliveryToken: "",
	}

	contentFailed := []byte(`{
            "doc_count": 1
        }`)

	contentPassed := []byte(`{
            "doc_count": 2
        }`)

	contentSkipped := []byte(`{
            "doc_count": 14
        }`)

	contentWaived := []byte(`{
            "doc_count": 0
        }`)

	contentImpact := []byte(`{
            "doc_count_error_upper_bound": 0,
            "sum_other_doc_count": 0,
            "buckets": [
                {
                    "key": 1.0,
                    "doc_count": 13,
                    "failed": {
                        "doc_count": 1
                    }
                },
                {
                    "key": 0.20000000298023224,
                    "doc_count": 1,
                    "failed": {
                        "doc_count": 2
                    }
                },
                {
                    "key": 0.30000001192092896,
                    "doc_count": 1,
                    "failed": {
                        "doc_count": 2
                    }
                },
                {
                    "key": 0.5,
                    "doc_count": 1,
                    "failed": {
                        "doc_count": 1
                    }
                },
                {
                    "key": 0.6000000238418579,
                    "doc_count": 1,
                    "failed": {
                        "doc_count": 1
                    }
                }
            ]
        }`)

	aggregations := make(map[string]json.RawMessage)
	aggregations["failed"] = contentFailed
	aggregations["passed"] = contentPassed
	aggregations["skipped"] = contentSkipped
	aggregations["waived"] = contentWaived
	aggregations["impact"] = contentImpact

	searchResult := &elastic.SearchResult{
		Aggregations: aggregations,
	}

	controlSummaryActual := esr.getStatsSummaryControlsResult(searchResult)

	assert.Equal(t, controlSummary.Passed, controlSummaryActual.Passed)
	assert.Equal(t, controlSummary.Failures, controlSummary.Failures)
	assert.Equal(t, controlSummary.Criticals, controlSummary.Criticals)
	assert.Equal(t, controlSummary.Majors, controlSummary.Majors)
	assert.Equal(t, controlSummary.Minors, controlSummary.Minors)
}
