package verification

import (
	"fmt"
	"os"
	"time"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/reporting"
	"github.com/jedib0t/go-pretty/table"
)

type verifySetup interface {
	VerifyHAAWSProvision(string)
	VerifyHAAWSManagedProvision(string)
	VerifyHAAWSDeployment(string)
	VerifyHAAWSManagedDeployment(string)
	VerifyOnPremDeployment(string)
	VerifyOnPremAWSManagedDeployment(string)
	VerifyOnPremCustManagedDeployment(string)
	VerifyStandaloneDeployment(string)
	VerifyCertificates(string)
}

var reportChan = make(chan reporting.VerfictionReport)
var nodeInfoMap = make(map[string][]reporting.Info)

func VerifyHAAWSProvision(configFile string) error {
	// config, err := parseAWSAutomateConfig(configFile)
	// if err != nil {
	// 	return err
	// }

	// validateAWSProvisionConfig(config)
	// return nil
	report := validateCertificateExpiry("abc")

	wr := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
	tb := make(map[string]*reporting.Table)
	tb["AutomateStatusTable"] = &reporting.Table{
		Header:    table.Row{"No.", "Identifier", "Paramter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 5, WidthMax: 50}},
	}
	tb["AutomateSummaryTable"] = &reporting.Table{
		Header:    table.Row{"Paramter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 4, WidthMax: 50}},
	}
	rp := reporting.NewReportingModule(wr, time.Second, tb)
	go reporting.VerfictionReports(reportChan, rp, nodeInfoMap)
	go chanWriter(reportChan, "Automate", report)
	time.Sleep(time.Second * 3)
	return nil
}

func VerifyHAAWSManagedProvision(configFile string) error {
	config, err := parseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}
	validateAWSManagedProvisionConfig(config)
	return nil
}

func VerifyHAAWSDeployment(configFile string) error {
	config, err := parseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}
	validateAWSDeploymentConfig(config)
	return nil
}

func VerifyHAAWSManagedDeployment(configFile string) error {
	config, err := parseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}
	validateAWSManagedDeploymentConfig(config)
	return nil
}

func VerifyOnPremDeployment(configFile string) error {
	config, err := parseOnPremConfig(configFile)
	if err != nil {
		return err
	}
	validateOnPremConfig(config)
	return nil
}

func VerifyOnPremAWSManagedDeployment(configFile string) error {
	config, err := parseOnPremConfig(configFile)
	if err != nil {
		return err
	}
	validateOnPremAWSConfig(config)
	return nil
}

func VerifyOnPremCustManagedDeployment(configFile string) error {
	config, err := parseOnPremConfig(configFile)
	if err != nil {
		return err
	}
	validateOnPremCustomerConfig(config)
	return nil
}

func VerifyStandaloneDeployment(configFile string) error {
	config, err := parseStandaloneConfig(configFile)
	if err != nil {
		return err
	}
	validateStandaloneDeploymentConfig(config)
	return nil
}

func VerifyCertificates(certContents string) error {
	report := validateCertificateExpiry(certContents)

	wr := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
	tb := make(map[string]*reporting.Table)
	tb["AutomateStatusTable"] = &reporting.Table{
		Header:    table.Row{"No.", "Identifier", "Paramter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 5, WidthMax: 50}},
	}
	tb["AutomateSummaryTable"] = &reporting.Table{
		Header:    table.Row{"Paramter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 4, WidthMax: 50}},
	}
	rp := reporting.NewReportingModule(wr, time.Second, tb)
	go reporting.VerfictionReports(reportChan, rp, nodeInfoMap)
	go chanWriter(reportChan, "Automate", report)
	time.Sleep(time.Second * 3)
	fmt.Println(rp.GetTable("AutomateStatusTable"))

	return nil
}

func chanWriter(reportChan chan reporting.VerfictionReport, nodeType string, report reporting.Info) {
	msg := reporting.VerfictionReport{
		TableKey:     nodeType,
		Report:       report,
		TotalReports: 10,
	}
	reportChan <- msg
}
