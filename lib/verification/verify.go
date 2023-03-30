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

type report struct {
	ipaddress   string
	parameter   string
	message     string
	errors      []string
	resolutions []string
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
	go runchecksForCertificates("172.168.1.1", "Automate", producerChan)

	<-producerChan
	close(reportChan)
	if <-doneChan {
		fmt.Println("done!!")
	}

	//time.Sleep(time.Second * 3)
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

	dataMap := map[string][]string{
		"Automate":   automateIps,
		"ChefServer": chefServerIps,
		"OpenSearch": opensearchIps,
		"Postgres":   postgresIps,
	}

	for nodeType, nodeIps := range dataMap {
		constructMap(nodeIps, nodeType, &ipsMap)
	}

	startReportModule()

	//validateOnPremConfig(config)

	runTests(ipsMap)

	return nil
}

func constructMap(nodeIps []string, nodeType string, m *map[string]string) {
	for _, val := range nodeIps {
		(*m)[val] = nodeType
	}
}

func runTests(ipsMap map[string]string) {
	var wg sync.WaitGroup

	for k, v := range ipsMap {
		ip := fmt.Sprintf("%v", k)
		nodeType := fmt.Sprintf("%v", v)
		if nodeType == "OpenSearch" || nodeType == "Postgres" {
			wg.Add(4)
			go func() {
				defer wg.Done()
				producerChan := make(chan bool, 1)
				runchecksForCertificates(ip, nodeType, producerChan)
			}()
		} else {
			wg.Add(3)
		}
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
}

func runchecksForCertificates(ipaddress string, nodeType string, doneChan chan bool) {
	result := validateCertificateFormat(ipaddress, "abc")
	if !result.Valid {
		chanWriter(reportChan, nodeType, result.Report)
	} else {
		result = validateCertificateExpiry(ipaddress, "abc")
		if !result.Valid {
			chanWriter(reportChan, nodeType, result.Report)
		} else {
			report := report{
				ipaddress: ipaddress,
				parameter: "Certificates",
				message:   "Cerificate validation successful",
			}
			chanWriter(reportChan, nodeType, getReport(report, "Success"))
		}
	}
	doneChan <- true
}

func runchecksForSystemResources(ipaddress string, nodeType string) {

	var errors []string
	var resolutions []string
	failed := false

	result := validateFreeDisk(ipaddress, nodeType)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.StatusMessage.ToResolve...)
		failed = true
	} else if result.Valid && result.Report.StatusMessage.SubMessage != nil {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.StatusMessage.ToResolve...)
	}

	result = validateCPU(ipaddress, nodeType)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.StatusMessage.ToResolve...)
		failed = true
	} else if result.Valid && result.Report.StatusMessage.SubMessage != nil {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.StatusMessage.ToResolve...)
	}

	if failed {
		report := report{
			ipaddress:   ipaddress,
			parameter:   "System Resources",
			message:     "System Resource Check failed",
			errors:      errors,
			resolutions: resolutions,
		}
		chanWriter(reportChan, nodeType, getReport(report, "Failed"))

	} else {
		report := report{
			ipaddress:   ipaddress,
			parameter:   "System Resources",
			message:     "All System Resources are as per the requirement",
			errors:      errors,
			resolutions: resolutions,
		}
		chanWriter(reportChan, nodeType, getReport(report, "Success"))
	}
}

func runchecksForSoftwareVersions(ipaddress string, nodeType string) {

	var errors []string
	var resolutions []string
	failed := false

	result := validateOSVersion(ipaddress, nodeType)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.StatusMessage.ToResolve...)
		failed = true
	} else if result.Valid && result.Report.StatusMessage.SubMessage != nil {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.StatusMessage.ToResolve...)
	}

	result = validateLinuxCommands(ipaddress, nodeType)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.StatusMessage.ToResolve...)
		failed = true
	} else if result.Valid && result.Report.StatusMessage.SubMessage != nil {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.StatusMessage.ToResolve...)
	}

	if failed {
		report := report{
			ipaddress:   ipaddress,
			parameter:   result.Report.Parameter,
			message:     result.Report.StatusMessage.MainMessage,
			errors:      errors,
			resolutions: resolutions,
		}
		chanWriter(reportChan, nodeType, getReport(report, "Failed"))

	} else {
		report := report{
			ipaddress:   ipaddress,
			parameter:   result.Report.Parameter,
			message:     result.Report.StatusMessage.MainMessage,
			errors:      errors,
			resolutions: resolutions,
		}
		chanWriter(reportChan, nodeType, getReport(report, "Success"))
	}
}

func getReport(testReport report, status string) reporting.Info {
	return reporting.Info{
		Hostip:    testReport.ipaddress,
		Parameter: testReport.parameter,
		Status:    status,
		StatusMessage: &reporting.StatusMessage{
			MainMessage: testReport.message,
			SubMessage:  testReport.errors,
			ToResolve:   testReport.resolutions,
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
	tb["AutomateStatusTable"] = getStatusTable()
	tb["AutomateSummaryTable"] = getSummaryTable()
	tb["ChefServerStatusTable"] = getStatusTable()
	tb["ChefServerSummaryTable"] = getSummaryTable()
	tb["PostgresStatusTable"] = getStatusTable()
	tb["PostgresSummaryTable"] = getSummaryTable()
	tb["OpenSearchStatusTable"] = getStatusTable()
	tb["OpenSearchSummaryTable"] = getSummaryTable()
	return tb
}

func getStatusTable() *reporting.Table {
	return &reporting.Table{
		Header:    table.Row{"No.", "Identifier", "Paramter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{},
	}
}

func getSummaryTable() *reporting.Table {
	return &reporting.Table{
		Header:    table.Row{"Paramter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{},
	}
}

func chanWriter(reportChan chan reporting.VerfictionReport, nodeType string, report reporting.Info) {
	msg := reporting.VerfictionReport{
		TableKey:     nodeType,
		Report:       report,
		TotalReports: 10,
	}
	reportChan <- msg
}
