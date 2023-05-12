package hardwareresourcecount_test

import (
	"fmt"
	"testing"

	"github.com/bmizerany/assert"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/hardwareresourcecount"
	"github.com/chef/automate/lib/logger"
)

var (
	VALID_HARDWARE_RESOURCE_COUNT_REQUEST = &models.HardwareResourceRequest{
		AutomateNodeCount: 2,
		AutomateNodeIps: []string{
			"172.154.0.1",
			"172.154.0.2",
		},
		ChefInfraServerNodeCount: 2,
		ChefInfraServerNodeIps: []string{
			"172.154.0.3",
			"172.154.0.4",
		},
		PostgresqlNodeCount: 3,
		PostgresqlNodeIps: []string{
			"172.154.0.5",
			"172.154.0.6",
			"172.154.0.7",
		},
		OpenSearchNodeCount: 3,
		OpenSearchNodeIps: []string{
			"172.154.0.8",
			"172.154.0.9",
			"172.154.0.10",
		},
	}

	VALID_HARDWARE_RESOURCE_COUNT_RESPONSE = []models.HardwareResourceResponse{
		{
			IP:       "172.154.0.1",
			NodeType: "Automate",
			Checks: []models.Checks{
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is unique",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is of valid format",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Not shared with backend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Automate Type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.2",
			NodeType: "Automate",
			Checks: []models.Checks{
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is unique",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is of valid format",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Not shared with backend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Automate Type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.3",
			NodeType: "Chef-infra-server",
			Checks: []models.Checks{
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is unique",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is of valid format",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Not shared with backend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Chef-infra-server Type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.4",
			NodeType: "Chef-infra-server",
			Checks: []models.Checks{
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is unique",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is of valid format",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Not shared with backend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Chef-infra-server Type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.5",
			NodeType: "Postgresql",
			Checks: []models.Checks{
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is unique",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is of valid format",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Not shared with backend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Postgresql Type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.6",
			NodeType: "Postgresql",
			Checks: []models.Checks{
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is unique",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is of valid format",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Not shared with backend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Postgresql Type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.7",
			NodeType: "Postgresql",
			Checks: []models.Checks{
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is unique",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is of valid format",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Not shared with backend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Postgresql Type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.8",
			NodeType: "Opensearch",
			Checks: []models.Checks{
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is unique",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is of valid format",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Not shared with backend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Opensearch Type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.9",
			NodeType: "Opensearch",
			Checks: []models.Checks{
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is unique",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is of valid format",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Not shared with backend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Opensearch Type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.10",
			NodeType: "Opensearch",
			Checks: []models.Checks{
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is unique",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "IP address is of valid format",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Not shared with backend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "Opensearch Type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
	}
)

func TestHardwareResourceCountService(t *testing.T) {
	hrc := hardwareresourcecount.NewHardwareResourceCountService(logger.NewTestLogger())
	resourcecount := hrc.GetHardwareResourceCount(models.HardwareResourceRequest{})
	assert.Equal(t, []models.HardwareResourceResponse{}, resourcecount)
}

func TestUniqueIP(t *testing.T) {
	tests := []struct {
		TestName    string
		NodeType    string
		Set         int
		NodeCount   int
		ExpectedRes bool
	}{
		{"Unique Automate IP", "Automate", 2, 2, true},
		{"Not Unique Automate IP", "Automate", 3, 2, false},
		{"Unique Chef Server IP", "Chef-infra-server", 2, 2, true},
		{"Not Unique Chef Server IP", "Chef-infra-server", 2, 3, false},
	}

	for _, e := range tests {
		res := hardwareresourcecount.UniqueIP(e.NodeType, e.Set, e.NodeCount)
		assert.Equal(t, e.ExpectedRes, res.Passed)
	}
}

func TestValidFormat(t *testing.T) {
	tests := []struct {
		TestName    string
		IP          string
		ExpectedRes bool
	}{
		{"Valid IP", "172.154.0.1", true},
		{"Invalid Format IP", "172.154.03", false},
		{"Invalid Range IP", "1000.0.0.1", false},
		{"Non-Numeric IP", "a.b.c.d", false},
		{"Domain Name", "google.com", false},
	}

	for _, e := range tests {
		res := hardwareresourcecount.ValidFormat(e.IP)
		assert.Equal(t, e.ExpectedRes, res.Passed)
	}
}

func TestValidCount(t *testing.T) {
	tests := []struct {
		TestName     string
		MinNodeCount int
		ReqNodeCount int
		NodeType     string
		ExpectedRes  bool
	}{
		{"Valid Automate Nodes", 2, 2, "Automate", true},
		{"Invalid Automate Nodes", 2, 1, "Automate", false},
		{"More nodes than requirement", 2, 3, "Automate", true},
	}

	for _, e := range tests {
		res := hardwareresourcecount.ValidCount(e.MinNodeCount, e.ReqNodeCount, e.NodeType)
		assert.Equal(t, e.ExpectedRes, res.Passed)
	}
}

func TestSharedIP(t *testing.T) {
	tests := []struct {
		TestName    string
		NodeType    string
		IP          string
		Set         map[string]string
		ExpectedRes bool
	}{
		{"IP is not Shared", "Automate", "172.3.4.5", map[string]string{
			"172.3.4.6": "Postgresql",
			"172.3.4.7": "Opensearch"}, true},
		{"IP is Shared", "Automate", "172.3.4.5", map[string]string{
			"172.3.4.5": "Postgresql",
			"172.3.4.7": "Opensearch"}, false},
	}

	for _, e := range tests {
		res := hardwareresourcecount.SharedIP(e.NodeType, e.IP, e.Set)
		assert.Equal(t, e.ExpectedRes, res.Passed)
	}
}

func TestValidateHardwareResources(t *testing.T) {
	tests := []struct {
		TestName     string
		MinNodeCount int
		ReqNodeCount int
		NodeType     string
		IP           string
		Set          map[string]string
		SetBackend   map[string]string
		ExpectedRes  bool
	}{
		{"All Validations okay", 2, 2, "Automate", "174.2.3.4",
			map[string]string{
				"172.2.3.4": "Automate",
				"172.2.3.5": "Automate"},
			map[string]string{"172.3.4.6": "Postgresql"}, true},
		{"Unique IP Test Failed", 2, 2, "Automate", "174.2.3.4",
			map[string]string{
				"172.2.3.4": "Automate"},
			map[string]string{"172.3.4.6": "Postgresql"}, false},
	}

	for _, e := range tests {
		res := hardwareresourcecount.ValidateHardwareResources(e.MinNodeCount, e.ReqNodeCount, e.NodeType, e.IP, e.Set, e.SetBackend)
		fmt.Println(res)
		assert.Equal(t, e.ExpectedRes, res.Checks[0].Passed)
	}
}

func TestGetHardwareResourceCount(t *testing.T) {
	res := hardwareresourcecount.NewHardwareResourceCountService(logger.NewTestLogger()).GetHardwareResourceCount(*VALID_HARDWARE_RESOURCE_COUNT_REQUEST)
	assert.Equal(t, VALID_HARDWARE_RESOURCE_COUNT_RESPONSE, res)
}
