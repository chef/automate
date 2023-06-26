package hardwareresourcecount

import (
	"testing"

	"github.com/bmizerany/assert"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

var (
	VALID_HARDWARE_RESOURCE_COUNT_REQUEST = &models.Hardware{
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
			NodeType: "automate",
			Checks: []models.Checks{
				{
					Title:         "Instance count",
					Passed:        true,
					SuccessMsg:    "Number of IP Addresses are matched with the instance count",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
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
					SuccessMsg:    "automate type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.2",
			NodeType: "automate",
			Checks: []models.Checks{
				{
					Title:         "Instance count",
					Passed:        true,
					SuccessMsg:    "Number of IP Addresses are matched with the instance count",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
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
					SuccessMsg:    "automate type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.3",
			NodeType: "chef-infra-server",
			Checks: []models.Checks{
				{
					Title:         "Instance count",
					Passed:        true,
					SuccessMsg:    "Number of IP Addresses are matched with the instance count",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
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
					SuccessMsg:    "chef-infra-server type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.4",
			NodeType: "chef-infra-server",
			Checks: []models.Checks{
				{
					Title:         "Instance count",
					Passed:        true,
					SuccessMsg:    "Number of IP Addresses are matched with the instance count",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
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
					SuccessMsg:    "chef-infra-server type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.5",
			NodeType: "postgresql",
			Checks: []models.Checks{
				{
					Title:         "Instance count",
					Passed:        true,
					SuccessMsg:    "Number of IP Addresses are matched with the instance count",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
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
					SuccessMsg:    "Not shared with frontend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "postgresql type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.6",
			NodeType: "postgresql",
			Checks: []models.Checks{
				{
					Title:         "Instance count",
					Passed:        true,
					SuccessMsg:    "Number of IP Addresses are matched with the instance count",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
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
					SuccessMsg:    "Not shared with frontend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "postgresql type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.7",
			NodeType: "postgresql",
			Checks: []models.Checks{
				{
					Title:         "Instance count",
					Passed:        true,
					SuccessMsg:    "Number of IP Addresses are matched with the instance count",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
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
					SuccessMsg:    "Not shared with frontend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "postgresql type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.8",
			NodeType: "opensearch",
			Checks: []models.Checks{
				{
					Title:         "Instance count",
					Passed:        true,
					SuccessMsg:    "Number of IP Addresses are matched with the instance count",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
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
					SuccessMsg:    "Not shared with frontend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "opensearch type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.9",
			NodeType: "opensearch",
			Checks: []models.Checks{
				{
					Title:         "Instance count",
					Passed:        true,
					SuccessMsg:    "Number of IP Addresses are matched with the instance count",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
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
					SuccessMsg:    "Not shared with frontend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "opensearch type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
		{
			IP:       "172.154.0.10",
			NodeType: "opensearch",
			Checks: []models.Checks{
				{
					Title:         "Instance count",
					Passed:        true,
					SuccessMsg:    "Number of IP Addresses are matched with the instance count",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
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
					SuccessMsg:    "Not shared with frontend nodes",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         "IP address",
					Passed:        true,
					SuccessMsg:    "opensearch type has valid count as per Automate HA requirement",
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
		},
	}
)

func TestHardwareResourceCountService(t *testing.T) {
	hrc := NewHardwareResourceCountService(logger.NewTestLogger())
	resourcecount := hrc.GetHardwareResourceCount(models.Hardware{})
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
		res := uniqueIP(e.NodeType, e.Set, e.NodeCount)
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
		res := validFormat(e.IP)
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
		res := validCount(e.MinNodeCount, e.ReqNodeCount, e.NodeType)
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
		res := sharedIP(e.NodeType, e.IP, e.Set)
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
		res := validateHardwareResources(e.MinNodeCount, e.ReqNodeCount, e.NodeType, e.IP, e.Set, e.SetBackend)
		assert.Equal(t, e.ExpectedRes, res.Checks[0].Passed)
	}
}

func TestGetHardwareResourceCount(t *testing.T) {
	res := NewHardwareResourceCountService(logger.NewTestLogger()).GetHardwareResourceCount(*VALID_HARDWARE_RESOURCE_COUNT_REQUEST)
	assert.Equal(t, VALID_HARDWARE_RESOURCE_COUNT_RESPONSE, res)
}
