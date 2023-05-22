package verification

import (
	"fmt"
	"os"
	"sync"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/config_parser"
	"github.com/chef/automate/lib/config_verify"
	"github.com/chef/automate/lib/reporting"
	"github.com/jedib0t/go-pretty/v5/table"
)

type Verification interface {
	VerifyHAAWSProvision(configFile string) error
	VerifyHAAWSManagedProvision(configFile string) error
	VerifyHAAWSDeployment(configFile string) error
	VerifyHAAWSManagedDeployment(configFile string) error
	VerifyOnPremDeployment(configFile string) error
	VerifyOnPremAWSManagedDeployment(configFile string) error
	VerifyOnPremCustManagedDeployment(configFile string) error
	VerifyStandaloneDeployment(configFile string) error
	VerifyCertificates(configFile string) error
}

type VerificationModule struct{}

type report struct {
	ipaddress             string
	parameter             string
	message               string
	errors                []string
	resolutions           []string
	successfulChecksCount int
	failedChecksCount     int
}

type numberOfNodes struct {
	numberOfAutomateNodes    int
	numberOfChefServerNodes  int
	numberOfPostgresSQLNodes int
	numberOfOpenSearchNodes  int
}

var reportChan = make(chan reporting.VerificationReport)
var nodeInfoMap = make(map[string][]reporting.Info)
var doneChan = make(chan bool, 1)

func (v *VerificationModule) VerifyHAAWSProvision(configFile string) error {
	configParser := &config_parser.ConfigParserImpl{}
	config, err := configParser.ParseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}

	validateAWSProvisionConfig(config)
	return nil
}

func (v *VerificationModule) VerifyHAAWSManagedProvision(configFile string) error {
	configParser := &config_parser.ConfigParserImpl{}
	config, err := configParser.ParseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}
	validateAWSManagedProvisionConfig(config)
	return nil
}

func (v *VerificationModule) VerifyHAAWSDeployment(configFile string) error {
	configParser := &config_parser.ConfigParserImpl{}
	config, err := configParser.ParseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}
	configVerify := &config_verify.ConfigVerifyImpl{}
	if err := configVerify.ConfigValidateAWS(config); err != nil {
		return err
	}
	validateAWSDeploymentConfig(config)
	return nil
}

func (v *VerificationModule) VerifyHAAWSManagedDeployment(configFile string) error {
	configParser := &config_parser.ConfigParserImpl{}
	config, err := configParser.ParseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}
	configVerify := &config_verify.ConfigVerifyImpl{}
	if err := configVerify.ConfigValidateAWS(config); err != nil {
		return err
	}

	validateAWSManagedDeploymentConfig(config)
	return nil
}

func (v *VerificationModule) VerifyOnPremAWSManagedDeployment(configFile string) error {
	configParser := &config_parser.ConfigParserImpl{}
	config, err := configParser.ParseOnPremConfig(configFile)
	if err != nil {
		return err
	}
	validateOnPremAWSConfig(config)
	return nil
}

func (v *VerificationModule) VerifyOnPremCustManagedDeployment(configFile string) error {
	configParser := &config_parser.ConfigParserImpl{}
	config, err := configParser.ParseOnPremConfig(configFile)
	if err != nil {
		return err
	}
	validateOnPremCustomerConfig(config)
	return nil
}

func (v *VerificationModule) VerifyStandaloneDeployment(configFile string) error {
	configParser := &config_parser.ConfigParserImpl{}
	config, err := configParser.ParseStandaloneConfig(configFile)
	if err != nil {
		return err
	}
	configVerify := &config_verify.ConfigVerifyImpl{}
	if err := configVerify.ConfigValidateStandalone(config); err != nil {
		return err
	}
	return validateStandaloneDeploymentConfig(config)
}

func (v *VerificationModule) VerifyCertificates(certContents string) error {

	return nil
}

func (v *VerificationModule) VerifyOnPremDeployment(configFile string) error {

	configParser := &config_parser.ConfigParserImpl{}
	config, err := configParser.ParseOnPremConfig(configFile)
	if err != nil {
		return err
	}
	configVerify := &config_verify.ConfigVerifyImpl{}
	if err := configVerify.ConfigValidateOnPrem(config); err != nil {
		return err
	}
	var ipsMap = make(map[string]string)
	automateIps := config.ExistingInfra.Config.AutomatePrivateIps
	chefServerIps := config.ExistingInfra.Config.ChefServerPrivateIps
	opensearchIps := config.ExistingInfra.Config.OpensearchPrivateIps
	postgresIps := config.ExistingInfra.Config.PostgresqlPrivateIps

	dataMap := map[string][]string{
		"Automate":    automateIps,
		"ChefServer":  chefServerIps,
		"OpenSearch":  opensearchIps,
		"PostgresSQL": postgresIps,
	}

	for nodeType, nodeIps := range dataMap {
		constructMap(nodeIps, nodeType, &ipsMap)
	}

	numberOfNodes := &numberOfNodes{
		numberOfAutomateNodes:    len(automateIps),
		numberOfChefServerNodes:  len(chefServerIps),
		numberOfPostgresSQLNodes: len(postgresIps),
		numberOfOpenSearchNodes:  len(opensearchIps),
	}

	startReportModule(numberOfNodes)

	//validateOnPremConfig(config)

	runTests(ipsMap, numberOfNodes)

	return nil
}

func constructMap(nodeIps []string, nodeType string, m *map[string]string) {
	for _, val := range nodeIps {
		(*m)[val] = nodeType
	}
}

func runTests(ipsMap map[string]string, numberOfNodes *numberOfNodes) {
	var wg sync.WaitGroup

	for k, v := range ipsMap {
		wg.Add(3)
		ip := fmt.Sprintf("%v", k)
		nodeType := fmt.Sprintf("%v", v)
		if nodeType == "OpenSearch" || nodeType == "PostgresSQL" {
			wg.Add(1)
			go func() {
				defer wg.Done()
				producerChan := make(chan bool, 1)
				runChecksForCertificates(ip, nodeType, producerChan, numberOfNodes)
			}()
		}
		go func() {
			defer wg.Done()
			validateNodeReachability(ip, nodeType, reportChan, numberOfNodes)
		}()
		go func() {
			defer wg.Done()
			runChecksForSystemResources(ip, nodeType, numberOfNodes)
		}()
		go func() {
			defer wg.Done()
			runChecksForSoftwareVersions(ip, nodeType, numberOfNodes)
		}()
	}
	wg.Wait()
	close(reportChan)
	if <-doneChan {
		fmt.Println("done!!")
	}
}

func runChecksForCertificates(ipaddress string, nodeType string, doneChan chan bool, numberOfNodes *numberOfNodes) {
	var successCount, failedCount int
	result := validateCertificateFormat(ipaddress, "abc")
	if !result.Valid {
		failedCount++
		result.Report.SummaryInfo.FailedCount = failedCount
		chanWriter(reportChan, nodeType, result.Report, numberOfNodes)
	} else {
		result = validateCertificateExpiry(ipaddress, "abc")
		if !result.Valid {
			failedCount++
			result.Report.SummaryInfo.FailedCount = failedCount
			chanWriter(reportChan, nodeType, result.Report, numberOfNodes)
		} else {
			successCount++
			report := report{
				ipaddress:             ipaddress,
				parameter:             "Certificates",
				message:               "Certificate validation successful",
				successfulChecksCount: successCount,
			}
			chanWriter(reportChan, nodeType, getReport(report, "Success"), numberOfNodes)
		}
	}
	doneChan <- true
}

func runChecksForSystemResources(ipaddress string, nodeType string, numberOfNodes *numberOfNodes) {
	var successCount, failedCount int
	var errors []string
	var resolutions []string
	failed := false

	result := validateFreeDisk(ipaddress, nodeType)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		failed = true
		failedCount++
	} else if result.Valid && result.Report.StatusMessage.SubMessage != nil {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		successCount++
	} else if result.Valid {
		successCount++
	}

	result = validateCPU(ipaddress, nodeType)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		failed = true
		failedCount++
	} else if result.Valid && result.Report.StatusMessage.SubMessage != nil {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		successCount++
	} else if result.Valid {
		successCount++
	}

	if failed {
		report := report{
			ipaddress:             ipaddress,
			parameter:             "System Resources",
			message:               "System Resource Check failed",
			errors:                errors,
			resolutions:           resolutions,
			failedChecksCount:     failedCount,
			successfulChecksCount: successCount,
		}
		chanWriter(reportChan, nodeType, getReport(report, "Failed"), numberOfNodes)

	} else {
		report := report{
			ipaddress:             ipaddress,
			parameter:             "System Resources",
			message:               "All System Resources are as per the requirement",
			errors:                errors,
			resolutions:           resolutions,
			failedChecksCount:     failedCount,
			successfulChecksCount: successCount,
		}
		chanWriter(reportChan, nodeType, getReport(report, "Success"), numberOfNodes)
	}
}

func runChecksForSoftwareVersions(ipaddress string, nodeType string, numberOfNodes *numberOfNodes) {
	var successCount, failedCount int
	var errors []string
	var resolutions []string
	failed := false

	result := validateOSVersion(ipaddress, nodeType)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		failed = true
		failedCount++
	} else if result.Valid && result.Report.StatusMessage.SubMessage != nil {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		successCount++
	} else if result.Valid {
		successCount++
	}

	result = validateLinuxCommands(ipaddress, nodeType)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		failed = true
		failedCount++
	} else if result.Valid && result.Report.StatusMessage.SubMessage != nil {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		successCount++
	} else if result.Valid {
		successCount++
	}

	if failed {
		report := report{
			ipaddress:             ipaddress,
			parameter:             result.Report.Parameter,
			message:               result.Report.StatusMessage.MainMessage,
			errors:                errors,
			resolutions:           resolutions,
			failedChecksCount:     failedCount,
			successfulChecksCount: successCount,
		}
		chanWriter(reportChan, nodeType, getReport(report, "Failed"), numberOfNodes)

	} else {
		report := report{
			ipaddress:             ipaddress,
			parameter:             result.Report.Parameter,
			message:               result.Report.StatusMessage.MainMessage,
			errors:                errors,
			resolutions:           resolutions,
			failedChecksCount:     failedCount,
			successfulChecksCount: successCount,
		}
		chanWriter(reportChan, nodeType, getReport(report, "Success"), numberOfNodes)
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
		},
		SummaryInfo: &reporting.SummaryInfo{
			ToResolve:       testReport.resolutions,
			SuccessfulCount: testReport.successfulChecksCount,
			FailedCount:     testReport.failedChecksCount,
		},
	}
}

func startReportModule(nodeNumbers *numberOfNodes) {
	wr := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
	tb := createTables(nodeNumbers)
	rp := reporting.NewReportingModule(wr, tb)
	go reporting.VerificationReports(reportChan, rp, nodeInfoMap, doneChan)
}

func createTables(nodeNumbers *numberOfNodes) map[string]*reporting.Table {
	automateSummaryTableTitle := fmt.Sprintf("Summary: Automate <Nodes> - %d", nodeNumbers.numberOfAutomateNodes)
	chefServerSummaryTableTitle := fmt.Sprintf("Summary: Chef Infra Server <Nodes> - %d", nodeNumbers.numberOfChefServerNodes)
	postgresSQLSummaryTableTitle := fmt.Sprintf("Summary: PostgresSQL <Nodes> - %d", nodeNumbers.numberOfPostgresSQLNodes)
	openSearchSummaryTableTitle := fmt.Sprintf("Summary: Opensearch <Nodes> - %d", nodeNumbers.numberOfOpenSearchNodes)
	tb := make(map[string]*reporting.Table)
	tb["AutomateStatusTable"] = getStatusTable("Automate")
	tb["AutomateSummaryTable"] = getSummaryTable(automateSummaryTableTitle)
	tb["ChefServerStatusTable"] = getStatusTable("Chef Infra Server")
	tb["ChefServerSummaryTable"] = getSummaryTable(chefServerSummaryTableTitle)
	tb["PostgresSQLStatusTable"] = getStatusTable("PostgresSQL")
	tb["PostgresSQLSummaryTable"] = getSummaryTable(postgresSQLSummaryTableTitle)
	tb["OpenSearchStatusTable"] = getStatusTable("OpenSearch")
	tb["OpenSearchSummaryTable"] = getSummaryTable(openSearchSummaryTableTitle)
	return tb
}

func getStatusTable(title string) *reporting.Table {
	return &reporting.Table{
		Title:     title,
		Header:    table.Row{"No.", "Identifier", "Parameter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 1, WidthMax: 5, WidthMin: 5}, {Number: 2, WidthMax: 15, WidthMin: 15}, {Number: 3, WidthMax: 25, WidthMin: 25}, {Number: 4, WidthMax: 15, WidthMin: 15}, {Number: 5, WidthMax: 60, WidthMin: 60}},
	}
}

func getSummaryTable(title string) *reporting.Table {
	return &reporting.Table{
		Title:     title,
		Header:    table.Row{"Parameter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 1, WidthMax: 30, WidthMin: 30}, {Number: 2, WidthMax: 15, WidthMin: 15}, {Number: 3, WidthMax: 15, WidthMin: 15}, {Number: 4, WidthMax: 60, WidthMin: 60}},
	}
}

func chanWriter(reportChan chan reporting.VerificationReport, nodeType string, report reporting.Info, nodeNumbers *numberOfNodes) {
	total := 3 * nodeNumbers.numberOfAutomateNodes
	if nodeType == "ChefServer" {
		total = 3 * nodeNumbers.numberOfChefServerNodes
	} else if nodeType == "OpenSearch" {
		total = 4 * nodeNumbers.numberOfOpenSearchNodes
	} else if nodeType == "PostgresSQL" {
		total = 4 * nodeNumbers.numberOfPostgresSQLNodes
	}
	msg := reporting.VerificationReport{
		TableKey:     nodeType,
		Report:       report,
		TotalReports: total,
	}
	reportChan <- msg
}
