package hardwarecal

import (
	"errors"
	"fmt"
	"strconv"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/pmt"
	"github.com/chef/automate/lib/reporting"
	"github.com/jedib0t/go-pretty/v5/table"
)

type HardwareCalPrompt struct {
	Prompt pmt.Prompt  `toml:"-"`
	cw     *cli.Writer `toml:"-"`
}

func NewHardwareCalPrompt(p pmt.Prompt, cw *cli.Writer) *HardwareCalPrompt {
	return &HardwareCalPrompt{
		Prompt: p,
		cw:     cw,
	}
}

func getRangeValidationFunc(min, max int, appendErrMsg string) func(input string) error {
	validate := func(input string) error {
		val, err := strconv.Atoi(input)
		if err != nil {
			return errors.New("invalid int")
		}
		if val < min || val > max {
			return fmt.Errorf(appendErrMsg, min, max)
		}
		return nil
	}
	return validate
}

const INPUT_ERR_MSG = "Calculator only supports in range of %v <= N <= %v. Please run load testing by yourself to determine infrastructure need"

func (h *HardwareCalPrompt) Run() (err error) {
	fmt.Println("*** This Hardware calculator has not been completely tested ***")
	fmt.Println("*** Thus, we request you to do your own Benchmarking and then choose the hardware resource specifications ***")
	req := &HardwareCalReq{}
	req.NoOfNodes, err = h.Prompt.InputIntDefaultValidation("No. of nodes", 5000, getRangeValidationFunc(1000, 150000, INPUT_ERR_MSG))
	if err != nil {
		return
	}

	req.FreqCountComplianceScanPerDay, err = h.Prompt.InputIntDefaultValidation("Compliance Scans per day for each node", 1, getRangeValidationFunc(0, 24, INPUT_ERR_MSG))
	if err != nil {
		return
	}

	req.ComplianceReportSizeKB, err = h.Prompt.InputIntDefaultValidation("Compliance Scan avg. report size in KB", 1024, getRangeValidationFunc(100, 4*1024, INPUT_ERR_MSG))
	if err != nil {
		return
	}

	req.FreqCountClientRunPerDay, err = h.Prompt.InputIntDefaultValidation("Client Runs per day for each node", 24, getRangeValidationFunc(0, 48, INPUT_ERR_MSG))
	if err != nil {
		return
	}

	req.ClientRunReportSizeKB, err = h.Prompt.InputIntDefaultValidation("Client Run avg. report size in KB", 300, getRangeValidationFunc(10, 1024, INPUT_ERR_MSG))
	if err != nil {
		return
	}

	req.FreqCountEventFeedPerDay, err = h.Prompt.InputIntDefaultValidation("Event Feed per day for each node", 24, getRangeValidationFunc(0, 48, INPUT_ERR_MSG))
	if err != nil {
		return
	}

	req.EventFeedSizeKB, err = h.Prompt.InputIntDefaultValidation("Event Feed avg. report size in KB", 100, getRangeValidationFunc(1, 500, INPUT_ERR_MSG))
	if err != nil {
		return
	}

	req.DataRetentionDays, err = h.Prompt.InputIntDefaultValidation("Data Retention days", 30, getRangeValidationFunc(1, 90, INPUT_ERR_MSG))
	if err != nil {
		return
	}

	req.NoOfReplicasInOpenSearch, err = h.Prompt.InputIntDefaultValidation("No. of Replicas in OpenSearch", 1, getRangeValidationFunc(0, 3, INPUT_ERR_MSG))
	if err != nil {
		return
	}

	resp, err := req.NewCal()
	if err != nil {
		return errors.New(INPUT_ERR_MSG)
	}

	tbHeader := table.Row{"Instance", "Count", "vCPU", "RAM (GB)", "Storage Space (GB)", "AWS Machine Type"}

	rows := []table.Row{
		{"Chef Automate", resp.AutomateNode.InstanceCount, resp.AutomateNode.CpuCount, resp.AutomateNode.RamGB, resp.AutomateNode.StorageGB, resp.AutomateNode.Type},
		{"Chef Infra Server", resp.ChefServerNode.InstanceCount, resp.ChefServerNode.CpuCount, resp.ChefServerNode.RamGB, resp.ChefServerNode.StorageGB, resp.ChefServerNode.Type},
		{"PostgreSQL", resp.PostgresqlNode.InstanceCount, resp.PostgresqlNode.CpuCount, resp.PostgresqlNode.RamGB, resp.PostgresqlNode.StorageGB, resp.PostgresqlNode.Type},
		{"OpenSearch", resp.OpenSearchNode.InstanceCount, resp.OpenSearchNode.CpuCount, resp.OpenSearchNode.RamGB, resp.OpenSearchNode.StorageGB, resp.OpenSearchNode.Type},
	}

	tb := &reporting.Table{
		Title:     "Hardware Requirements",
		Rows:      rows,
		Header:    tbHeader,
		Footer:    []interface{}{},
		ColConfig: []table.ColumnConfig{},
	}

	tbMap := map[string]*reporting.Table{}
	tbMap["HardwareRequirement"] = tb

	report := reporting.NewReportingModule(h.cw, tbMap)
	report.GenerateTableOutputAndPrint(tb)

	return
}
