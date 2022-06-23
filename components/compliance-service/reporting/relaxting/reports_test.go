package relaxting

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	reportingapi "github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/stretchr/testify/assert"
)

func TestConvertControlProcessesTags(t *testing.T) {
	profileControlsMap := make(map[string]*reportingapi.Control, 2)

	profileControlsMap["123"] = &reportingapi.Control{
		Id:     "123",
		Impact: 1,
		Title:  "Running worker process as non-privileged user",
	}
	inspecReportControl := ESInSpecReportControl{
		ID: "123",
		StringTags: []ESInSpecReportControlStringTags{
			{Key: "single string"},
			{Key: "test", Values: []string{"one", "two"}},
			{Key: "test 2", Values: []string{"one val"}},
		},
	}

	convertedControl := convertControl(profileControlsMap, inspecReportControl, map[string][]string{})

	stringTags := make(map[string]*reportingapi.TagValues, 0)
	stringTags["test"] = &reportingapi.TagValues{Values: []string{"one", "two"}}
	stringTags["single string"] = &reportingapi.TagValues{Values: []string{"null"}}
	stringTags["test 2"] = &reportingapi.TagValues{Values: []string{"one val"}}

	assert.Equal(t, &reportingapi.Control{
		Id:         "123",
		Impact:     1,
		Title:      "Running worker process as non-privileged user",
		StringTags: stringTags,
		Refs:       make([]*reportingapi.Ref, 0),
		Results:    make([]*reportingapi.Result, 0),
	}, convertedControl)
}

func setup() (map[string][]string, map[string]*reportingapi.Control) {
	filters := make(map[string][]string, 0)
	profileControlsMap := make(map[string]*reportingapi.Control, 3)

	profileControlsMap["123"] = &reportingapi.Control{
		Id:     "123",
		Impact: 1,
		Title:  "Running worker process as non-privileged user",
	}

	profileControlsMap["456"] = &reportingapi.Control{
		Id:     "456",
		Impact: 1,
		Title:  "Another Control",
	}

	profileControlsMap["789"] = &reportingapi.Control{
		Id:     "789",
		Impact: 1,
		Title:  "Another Control",
	}
	return filters, profileControlsMap
}

func TestConvertControlFiltersByTagNoMatch(t *testing.T) {
	filters, profileControlsMap := setup()
	filters["control_tag:nist"] = []string{"test-1"}

	inspecReportControl123 := ESInSpecReportControl{
		ID: "123",
		StringTags: []ESInSpecReportControlStringTags{
			{Key: "single string"},
			{Key: "test", Values: []string{"one", "two"}},
		},
	}

	convertedControl := convertControl(profileControlsMap, inspecReportControl123, filters)

	expected := &reportingapi.Control{}
	expected = nil

	assert.Equal(t, expected, convertedControl)
}

func TestConvertControlFiltersByTagValMatch(t *testing.T) {
	filters, profileControlsMap := setup()

	inspecReportControl456 := ESInSpecReportControl{
		ID: "456",
		StringTags: []ESInSpecReportControlStringTags{
			{Key: "nist", Values: []string{"test-1"}},
		},
	}

	convertedControl := convertControl(profileControlsMap, inspecReportControl456, filters)

	stringTags := make(map[string]*reportingapi.TagValues, 0)
	stringTags["nist"] = &reportingapi.TagValues{Values: []string{"test-1"}}

	assert.Equal(t, &reportingapi.Control{
		Id:         "456",
		Impact:     1,
		Title:      "Another Control",
		StringTags: stringTags,
		Refs:       make([]*reportingapi.Ref, 0),
		Results:    make([]*reportingapi.Result, 0),
	}, convertedControl)
}
func TestConvertControlFiltersByTagOnlyMatch(t *testing.T) {
	filters, profileControlsMap := setup()

	inspecReportControl789 := ESInSpecReportControl{
		ID: "789",
		StringTags: []ESInSpecReportControlStringTags{
			{Key: "cci", Values: []string{""}},
		},
		WaivedStr: "yes_run",
		WaiverData: &ESInSpecReportControlsWaiverData{
			ExpirationDate:     "2025-06-01",
			Run:                true,
			Justification:      "Some reason",
			SkippedDueToWaiver: false,
			Message:            "Some message",
		},
		RemovedResultsCounts: &ESInSpecReportControlRemovedResultsCounts{
			Failed:  11,
			Passed:  12,
			Skipped: 13,
		},
	}

	filters["control_tag:cci"] = []string{""}
	convertedControl := convertControl(profileControlsMap, inspecReportControl789, filters)

	stringTags := make(map[string]*reportingapi.TagValues, 0)
	stringTags["cci"] = &reportingapi.TagValues{Values: []string{""}}

	assert.Equal(t, &reportingapi.Control{
		Id:         "789",
		Impact:     1,
		Title:      "Another Control",
		StringTags: stringTags,
		Refs:       make([]*reportingapi.Ref, 0),
		Results:    make([]*reportingapi.Result, 0),
		WaivedStr:  "yes_run",
		WaiverData: &reportingapi.OrigWaiverData{
			ExpirationDate:     "2025-06-01",
			Run:                true,
			Justification:      "Some reason",
			SkippedDueToWaiver: false,
			Message:            "Some message",
		},
		RemovedResultsCounts: &reportingapi.RemovedResultsCounts{
			Failed:  11,
			Passed:  12,
			Skipped: 13,
		},
	}, convertedControl)
}

func TestDoesControlTagMatchFilter(t *testing.T) {
	// json tags set one
	tags1 := map[string]*reportingapi.TagValues{}
	tags1["test"] = &reportingapi.TagValues{
		Values: []string{"one", "two"},
	}
	// json tags set two
	tags2 := map[string]*reportingapi.TagValues{}
	tags2["shoe"] = &reportingapi.TagValues{
		Values: []string{"blue"},
	}
	tags2["sock"] = &reportingapi.TagValues{
		Values: []string{"yellow", "pink"},
	}
	// json tags set three
	tags3 := map[string]*reportingapi.TagValues{}
	tags3["key_only"] = &reportingapi.TagValues{
		Values: []string{"null"},
	}

	filterSock := map[string][]string{}
	filterSock["control_tag:sock"] = []string{"yellow"}

	filterShoe := map[string][]string{}
	filterShoe["control_tag:shoe"] = []string{"blue"}

	filterNull := map[string][]string{}
	filterNull["control_tag:key_only"] = []string{"null"}

	// test matching filter
	assert.Equal(t, true, doesControlTagMatchFilter(filterSock, tags2))
	assert.Equal(t, true, doesControlTagMatchFilter(filterShoe, tags2))

	// test no match filter
	assert.Equal(t, false, doesControlTagMatchFilter(filterSock, tags3))
	assert.Equal(t, false, doesControlTagMatchFilter(filterNull, tags2))

	// test null match
	assert.Equal(t, true, doesControlTagMatchFilter(filterNull, tags3))

	// test multiple control tag filters
	multFilters := filterSock
	multFilters["control_tag:shoe"] = []string{"blue"}
	assert.Equal(t, true, doesControlTagMatchFilter(multFilters, tags2))
}

func TestGetNodeInfoFromReportID_Success(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if strings.Contains(r.URL.Path, mappings.IndexNameProf) {
			w.Write([]byte(`{
				"took": 1,
				"timed_out": false,
				"_shards": {
				  "total": 5,
				  "successful": 5,
				  "skipped": 0,
				  "failed": 0
				},
				"hits": {
				  "total": 1,
				  "max_score": 1,
				  "hits": [
					{
					  "_index": "comp-3-profiles",
					  "_type": "_doc",
					  "_id": "f42d2f48c9acd48f52324d52ec575ca9028e405eb303f69cb34d79eb0e588b5c",
					  "_score": 1,
					  "_source": {
						"name": "ssh-baseline",
						"title": "DevSec SSH Baseline",
						"version": "2.2.0",
						"summary": "Test-suite for best-practice SSH hardening",
						"maintainer": "DevSec Hardening Framework Team",
						"license": "Apache 2 license",
						"copyright": "DevSec Hardening Framework Team",
						"copyright_email": "hello@dev-sec.io",
						"controls": [
						  {
							"id": "ssh-10",
							"code": "control 'ssh-10' do\n  impact 1.0\n  title 'Client: Check for secure ssh Message Authentication Codes'\n  desc 'Configure a list of Message Authentication Codes (MACs) to the best secure MACs (avoid older and weaker Message Authentication Codes)'\n  describe ssh_config do\n    its('MACs') { should eq(ssh_crypto.valid_macs) }\n  end\nend\n",
							"desc": "Configure a list of Message Authentication Codes (MACs) to the best secure MACs (avoid older and weaker Message Authentication Codes)",
							"impact": 1,
							"title": "Client: Check for secure ssh Message Authentication Codes",
							"source_location": {
							  "ref": "ssh-baseline-2.2.0/controls/ssh_spec.rb",
							  "line": 117
							},
							"refs": "[]",
							"tags": "{}"
						  }
						],
						"supports": [
						  {}
						],
						"attributes": [],
						"sha256": "f42d2f48c9acd48f52324d52ec575ca9028e405eb303f69cb34d79eb0e588b5c",
						"groups": [
						  {
							"id": "controls/ssh_spec.rb",
							"title": "SSH client config",
							"controls": [
							  "ssh-10"
							]
						  },
						  {
							"id": "controls/sshd_spec.rb",
							"title": "SSH server config",
							"controls": [
							  "sshd-10"
							]
						  }
						],
						"doc_version": "1",
						"@timestamp": "2021-10-01T08:04:27Z"
					  }
					}
				  ]
				}
			  }`))
		} else if strings.Contains(r.URL.Path, mappings.IndexNameRep) {
			assert.Contains(t, r.URL.Path, fmt.Sprintf("%s-2021.09.29", mappings.IndexNameRep))
			assert.Contains(t, r.URL.Path, fmt.Sprintf("%s-2021.09.30", mappings.IndexNameRep))
			w.Write([]byte(`{
				"took": 15,
				"hits": {
				  "total": 1,
				  "hits": [
					{
					  "_source": {
						"status_message": "",
						"environment": "DevSec Dev Delta",
						"roles": [
						  "base_windows",
						  "windows-hardening",
						  "best.role.ever"
						],
						"node_name": "chef-load-yellow-debs-khaki",
						"end_time": "2021-09-29T08:04:20Z",
						"node_uuid": "5d84476e-362a-3c65-8acd-dc8b38f3a272",
						"version": "2.2.20",
						"platform": {
						  "release": "18.04",
						  "name": "ubuntu",
						  "full": "ubuntu 18.04"
						},
						"status": "failed"
					  },
					  "inner_hits": {
						"profiles": {
						  "hits": {
							"total": 1,
							"max_score": 6.5892015,
							"hits": [
							  {
								"_index": "comp-7-r-2021.09.29",
								"_type": "_doc",
								"_id": "0d67b0ab-2709-49c7-81e4-efcc5700c5cf",
								"_nested": {
								  "field": "profiles",
								  "offset": 1
								},
								"_score": 6.5892015,
								"_source": {
								  "sha256": "f42d2f48c9acd48f52324d52ec575ca9028e405eb303f69cb34d79eb0e588b5c",
								  "depends": [],
								  "name": "ssh-baseline",
								  "skip_message": "",
								  "version": "2.2.0",
								  "status": "failed"
								},
								"inner_hits": {
								  "profiles.controls": {
									"hits": {
									  "total": 1,
									  "max_score": 4.797442,
									  "hits": [
										{
										  "_index": "comp-7-r-2021.09.29",
										  "_type": "_doc",
										  "_id": "0d67b0ab-2709-49c7-81e4-efcc5700c5cf",
										  "_nested": {
											"field": "profiles",
											"offset": 1,
											"_nested": {
											  "field": "controls",
											  "offset": 9
											}
										  },
										  "_score": 4.797442,
										  "_source": {
											"id": "ssh-10",
											"impact": 1,
											"title": "Client: Check for secure ssh Message Authentication Codes",
											"status": "failed",
											"results": [
											  {
												"status": "failed",
												"code_desc": "SSH Configuration MACs should eq \"hmac-sha2-512,hmac-sha2-256,hmac-ripemd160\"",
												"run_time": 0.000412,
												"message": "\nexpected: \"hmac-sha2-512,hmac-sha2-256,hmac-ripemd160\"\n     got: nil\n\n(compared using ==)\n"
											  }
											],
											"waiver_data": null,
											"waived_str": "no",
											"tags": "",
											"string_tags": [],
											"refs": [],
											"removed_results_counts": null
										  }
										}
									  ]
									}
								  }
								}
							  }
							]
						  }
						}
					  }
					}
				  ]
				}
			  }`))
		}
	}))
	defer ts.Close()

	esr := ES2Backend{
		ESUrl:             ts.URL,
		Enterprise:        "",
		ChefDeliveryUser:  "",
		ChefDeliveryToken: "",
	}
	filters := map[string][]string{
		"control":               []string{"ssh-10"},
		"end_time":              []string{"2021-09-30T23:59:59Z"},
		"environment":           []string{"DevSec Dev Delta"},
		"inspec_version":        []string{"2.2.20"},
		"node_id":               []string{"5d84476e-362a-3c65-8acd-dc8b38f3a272"},
		"platform_with_version": []string{"ubuntu 18.04"},
		"profile_id":            []string{"f42d2f48c9acd48f52324d52ec575ca9028e405eb303f69cb34d79eb0e588b5c"},
		"recipe":                []string{"tomcat"},
		"role":                  []string{"base_windows"},
		"start_time":            []string{"2021-09-29T00:00:00Z"},
	}

	nodeHeaderInfo, err := esr.GetNodeInfoFromReportID("0d67b0ab-2709-49c7-81e4-efcc5700c5cf", filters)
	assert.NoError(t, err)
	assert.Equal(t, "5d84476e-362a-3c65-8acd-dc8b38f3a272", nodeHeaderInfo.NodeId)
	assert.Equal(t, "chef-load-yellow-debs-khaki", nodeHeaderInfo.NodeName)
	assert.Equal(t, int64(1632902660), nodeHeaderInfo.EndTime.Seconds)
	assert.Equal(t, "failed", nodeHeaderInfo.Status)
	assert.Equal(t, "DevSec Dev Delta", nodeHeaderInfo.Environment)
	assert.Equal(t, "2.2.20", nodeHeaderInfo.Version)
	assert.Equal(t, "ubuntu", nodeHeaderInfo.Platform.Name)
	assert.Equal(t, "18.04", nodeHeaderInfo.Platform.Release)
	assert.Equal(t, "ubuntu 18.04", nodeHeaderInfo.Platform.Full)
	assert.Equal(t, 1, len(nodeHeaderInfo.Profiles))
	assert.Equal(t, "ssh-baseline", nodeHeaderInfo.Profiles[0].Name)
	assert.Equal(t, "failed", nodeHeaderInfo.Profiles[0].Status)
	assert.Equal(t, "", nodeHeaderInfo.Profiles[0].StatusMessage)
	assert.Equal(t, []string{"base_windows", "windows-hardening", "best.role.ever"}, nodeHeaderInfo.Roles)
	assert.Equal(t, "", nodeHeaderInfo.StatusMessage)
}

func TestGetNodeInfoFromReportID_Failed(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusBadRequest)
		w.Write([]byte(`{
			"error": {
				"status": 403
			}
		}`))
	}))
	defer ts.Close()

	esr := ES2Backend{
		ESUrl:             ts.URL,
		Enterprise:        "",
		ChefDeliveryUser:  "",
		ChefDeliveryToken: "",
	}
	filters := map[string][]string{
		"end_time":   []string{"2021-09-29T23:59:59Z"},
		"node_id":    []string{"5d84476e-362a-3c65-8acd-dc8b38f3a272"},
		"start_time": []string{"2021-09-29T00:00:00Z"},
	}
	_, err := esr.GetNodeInfoFromReportID("0d67b0ab-2709-49c7-81e4-efcc5700c5cf", filters)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "GetNodeInfoFromReportID unable to complete search")
}
