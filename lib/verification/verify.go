package verification

import (
	"fmt"
	"os"
	"sync"
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

type failedReport struct {
	ipaddress   string
	parameter   string
	message     string
	errors      []string
	resolutions []string
}

type successReport struct {
	ipaddress string
	parameter string
	message   string
}

var reportChan = make(chan reporting.VerfictionReport)
var nodeInfoMap = make(map[string][]reporting.Info)
var doneChan = make(chan bool, 1)

func VerifyHAAWSProvision(configFile string) error {
	config, err := parseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}

	validateAWSProvisionConfig(config)
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
	var ipsMap = make(map[string]string)
	automateIps := config.ExistingInfra.Config.AutomatePrivateIps
	chefServerIps := config.ExistingInfra.Config.ChefServerPrivateIps
	opensearchIps := config.ExistingInfra.Config.OpensearchPrivateIps
	postgresIps := config.ExistingInfra.Config.PostgresqlPrivateIps
	// validateOnPremConfig(config)
	//fmt.Println(postgresIps);

	//mapChannel := make(chan bool)
	constructMap(automateIps, "Automate", &ipsMap)
	constructMap(chefServerIps, "ChefServer", &ipsMap)
	constructMap(postgresIps, "Postgres", &ipsMap)
	constructMap(opensearchIps, "OpenSearch", &ipsMap)

	dataMap := map[string][]string{
		"Automate":   automateIps,
		"ChefServer": chefServerIps,
		"OpenSearch": opensearchIps,
		"Postgres":   postgresIps,
	}

	for nodeType, nodeIps := range dataMap {
		constructMap(nodeIps, nodeType, &ipsMap)
	}

	// validateOnPremConfig(config)
	fmt.Println(ipsMap)
	startReportModule()

	var wg sync.WaitGroup

	for k, v := range ipsMap {
		wg.Add(3)
		ip := fmt.Sprintf("%v", k)
		nodeType := fmt.Sprintf("%v", v)
		go func() {
			defer wg.Done()
			validateNodeReachability(ip, nodeType, reportChan)
		}()
		go func() {

			defer wg.Done()
			runchecksForSystemResources(ip, nodeType)
		}()
		go func() {

			defer wg.Done()
			runchecksForSoftwareVersions(ip, nodeType)
		}()
	}
	wg.Wait()

	close(reportChan)
	if <-doneChan {
		fmt.Println("done!!")
	}

	return nil
}

func constructMap(nodeIps []string, nodeType string, m *map[string]string) {
	for _, val := range nodeIps {
		(*m)[val] = nodeType
	}
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

	startReportModule()

	producerChan := make(chan bool, 1)
	go runchecksForCertificates(producerChan)

	<-producerChan
	close(reportChan)
	if <-doneChan {
		fmt.Println("done!!")
	}

	//time.Sleep(time.Second * 3)
	return nil
}

func runchecksForCertificates(doneChan chan bool) {
	result := validateCertificateFormat("abc")
	if !result.Valid {
		chanWriter(reportChan, "Automate", result.Report)
	} else {
		result = validateCertificateExpiry("abc")
		if !result.Valid {
			chanWriter(reportChan, "Automate", result.Report)
		} else {
			report := successReport{
				ipaddress: "172.1.1.1",
				parameter: "Certificates",
				message:   "Cerificate validation successful",
			}
			chanWriter(reportChan, "Automate", getSuccessReport(report))
		}
	}
	doneChan <- true
}

func runchecksForSystemResources(ipaddress string, nodeType string) {

	var errors []string
	var resolutions []string

	result := validateFreeDisk(ipaddress)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.StatusMessage.ToResolve...)
	}
	result = validateCPU(ipaddress)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.StatusMessage.ToResolve...)
	}

	if errors != nil && resolutions != nil {

		report := failedReport{
			ipaddress:   ipaddress,
			parameter:   "System Resources",
			message:     "System Resource Check failed",
			errors:      errors,
			resolutions: resolutions,
		}
		chanWriter(reportChan, nodeType, getErrorReport(report))

	} else {
		report := successReport{
			ipaddress: ipaddress,
			parameter: "System Resources",
			message:   "All System Resources are as per the requirement",
		}
		chanWriter(reportChan, nodeType, getSuccessReport(report))
	}
}

func runchecksForSoftwareVersions(ipaddress string, nodeType string) {

	var errors []string
	var resolutions []string

	result := validateOSVersion(ipaddress)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.StatusMessage.ToResolve...)
	}
	result = validateLinuxCommands(ipaddress)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.StatusMessage.ToResolve...)
	}

	if errors != nil && resolutions != nil {

		report := failedReport{
			ipaddress:   ipaddress,
			parameter:   result.Report.Parameter,
			message:     result.Report.StatusMessage.MainMessage,
			errors:      errors,
			resolutions: resolutions,
		}
		chanWriter(reportChan, nodeType, getErrorReport(report))

	} else {
		report := successReport{
			ipaddress: ipaddress,
			parameter: result.Report.Parameter,
			message:   result.Report.StatusMessage.MainMessage,
		}
		chanWriter(reportChan, nodeType, getSuccessReport(report))
	}
}

func getSuccessReport(report successReport) reporting.Info {
	return reporting.Info{
		Hostip:    report.ipaddress,
		Parameter: report.parameter,
		Status:    "Success",
		StatusMessage: &reporting.StatusMessage{
			MainMessage: report.message,
		},
	}
}

func getErrorReport(report failedReport) reporting.Info {
	return reporting.Info{
		Hostip:    report.ipaddress,
		Parameter: report.parameter,
		Status:    "Failed",
		StatusMessage: &reporting.StatusMessage{
			MainMessage: report.message,
			SubMessage:  report.errors,
			ToResolve:   report.resolutions,
		},
	}
}
func startReportModule() {
	wr := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
	tb := createTables()
	rp := reporting.NewReportingModule(wr, time.Second, tb)
	go reporting.VerfictionReports(reportChan, rp, nodeInfoMap, doneChan)

}

func createTables() map[string]*reporting.Table {
	tb := make(map[string]*reporting.Table)
	tb["AutomateStatusTable"] = &reporting.Table{
		Header:    table.Row{"No.", "Identifier", "Paramter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 5, WidthMax: 50}},
	}
	tb["AutomateSummaryTable"] = &reporting.Table{
		Header:    table.Row{"Paramter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 4, WidthMax: 50}},
	}
	tb["ChefServerStatusTable"] = &reporting.Table{
		Header:    table.Row{"No.", "Identifier", "Paramter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 5, WidthMax: 50}},
	}
	tb["ChefServerSummaryTable"] = &reporting.Table{
		Header:    table.Row{"Paramter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 4, WidthMax: 50}},
	}
	tb["PostgresStatusTable"] = &reporting.Table{
		Header:    table.Row{"No.", "Identifier", "Paramter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 5, WidthMax: 50}},
	}
	tb["PostgresSummaryTable"] = &reporting.Table{
		Header:    table.Row{"Paramter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 4, WidthMax: 50}},
	}
	tb["OpenSearchStatusTable"] = &reporting.Table{
		Header:    table.Row{"No.", "Identifier", "Paramter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 5, WidthMax: 50}},
	}
	tb["OpenSearchSummaryTable"] = &reporting.Table{
		Header:    table.Row{"Paramter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 4, WidthMax: 50}},
	}
	return tb
}

func chanWriter(reportChan chan reporting.VerfictionReport, nodeType string, report reporting.Info) {
	msg := reporting.VerfictionReport{
		TableKey:     nodeType,
		Report:       report,
		TotalReports: 10,
	}
	reportChan <- msg
}
