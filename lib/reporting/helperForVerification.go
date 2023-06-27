package reporting

import (
	"fmt"
	"sort"
	"strings"

	"github.com/jedib0t/go-pretty/v5/table"
)

type VerificationReport struct {
	TableKey string
	Report   Info
}

type Info struct {
	Hostip        string         `json:"hostip,omitempty"`
	Parameter     string         `json:"parameter,omitempty"`
	Status        string         `json:"status,omitempty"`
	StatusMessage *StatusMessage `json:"statusMessage,omitempty"`
	SummaryInfo   *SummaryInfo   `json:"summaryInfo,omitempty"`
}

type StatusMessage struct {
	MainMessage string   `json:"mainMessage,omitempty"`
	SubMessage  []string `json:"subMessage,omitempty"`
}

type SummaryInfo struct {
	SuccessfulCount int      `json:"successfulCount,omitempty"`
	FailedCount     int      `json:"failedCount,omitempty"`
	ToResolve       []string `json:"toResolve,omitempty"`
}

func VerificationReports(reports []VerificationReport, reporting Reporting, nodeInfoMap map[string][]Info) {
	keyMap := make(map[string][]string)
	tableKeys := reporting.GetAllTableKeys()

	for _, report := range reports {
		info := report.Report
		node := report.TableKey
		var nodeinfo []Info
		if _, ok := nodeInfoMap[node]; ok {
			nodeinfo := nodeInfoMap[node]
			nodeinfo = append(nodeinfo, info)
			nodeInfoMap[node] = nodeinfo
		} else {
			nodeinfo = append(nodeinfo, info)
			nodeInfoMap[node] = nodeinfo
			keyMap[node] = matchNodeNameWithRespectiveTablesName(node, tableKeys)
		}
	}

	updateTablesWithData(reporting, nodeInfoMap, keyMap)
	printTables(reporting, nodeInfoMap, keyMap)
}

func printTables(reporting Reporting, nodeInfo map[string][]Info, keyMap map[string][]string) {
	var summaryTables []*Table
	keys := make([]string, len(nodeInfo))
	i := 0
	for key := range nodeInfo {
		keys[i] = key
		i++
	}
	sort.Strings(keys)
	updateTableTitle(reporting, nodeInfo, keyMap)
	for _, key := range keys {
		statusTb := reporting.GetTable(keyMap[key][0])
		summaryTb := reporting.GetTable(keyMap[key][1])
		summaryTables = append(summaryTables, summaryTb)
		printSingleTable(reporting, statusTb)
	}
	for _, summaryTable := range summaryTables {
		printSingleTable(reporting, summaryTable)
	}
}

func printSingleTable(reporting Reporting, table *Table) {
	reporting.GenerateTableOutputAndPrint(table)
	fmt.Print("\n")
}

func updateTablesWithData(reporting Reporting, nodeInfo map[string][]Info, keyMap map[string][]string) {
	for key, value := range nodeInfo {
		summaryMap := make(map[string]SummaryInfo)
		statusTableRows, summaryTableRows := createSingleNodeRowsForBothTables(key, value, reporting, summaryMap)
		if _, ok := keyMap[key]; !ok {
			continue
		}
		matchingKeys := keyMap[key]
		if len(matchingKeys) < 2 {
			continue
		}
		reporting.GetTable(matchingKeys[0]).Rows = statusTableRows
		reporting.GetTable(matchingKeys[1]).Rows = summaryTableRows
	}
}

func createSingleNodeRowsForBothTables(title string, nodeInfo []Info, reporting Reporting, summaryMap map[string]SummaryInfo) ([]table.Row, []table.Row) {
	var statusTableRows []table.Row
	var summaryTableRows []table.Row
	for idx, value := range nodeInfo {
		singleRowEntry := createStatusTableRows(idx+1, &value, reporting)
		statusTableRows = append(statusTableRows, singleRowEntry...)
		summaryMap = createSummaryTableRowData(summaryMap, &value)
	}
	summaryTableRows = createSummaryTableRows(summaryMap, reporting)
	return statusTableRows, summaryTableRows
}

func createStatusTableRows(index int, rowData *Info, reporting Reporting) []table.Row {
	var row table.Row
	var rows []table.Row

	if rowData.Status == "Success" {
		row = successRow(index, rowData, reporting)
		rows = append(rows, row)
	} else if rowData.Status == "Failed" {

		row = failedRow(index, rowData, reporting)
		rows = append(rows, row)
	} else if rowData.Status == "Skipped" {

		row = skippedRow(index, rowData, reporting)
		rows = append(rows, row)
	}
	if len(rowData.StatusMessage.SubMessage) > 0 {
		for _, value := range rowData.StatusMessage.SubMessage {
			row = subMessageRow(value, rowData, reporting)
			rows = append(rows, row)
		}
	}
	return rows
}

func successRow(index int, rowData *Info, reporting Reporting) table.Row {
	hostip := rowData.Hostip
	parameter := rowData.Parameter
	status := reporting.ChangeColour(Green, reporting.AppendSpecialCharater(Success, rowData.Status))
	message := rowData.StatusMessage.MainMessage
	row := table.Row{index, hostip, parameter, status, message}
	return row
}

func failedRow(index int, rowData *Info, reporting Reporting) table.Row {
	hostip := reporting.ChangeColour(Red, rowData.Hostip)
	parameter := reporting.ChangeColour(Red, rowData.Parameter)
	status := reporting.ChangeColour(Red, reporting.AppendSpecialCharater(Failed, rowData.Status))
	message := reporting.ChangeColour(Red, rowData.StatusMessage.MainMessage)

	row := table.Row{index, hostip, parameter, status, message}
	return row
}

func skippedRow(index int, rowData *Info, reporting Reporting) table.Row {
	hostip := rowData.Hostip
	parameter := rowData.Parameter
	status := reporting.ChangeColour(Grey, reporting.AppendSpecialCharater(Skipped, rowData.Status))
	message := rowData.StatusMessage.MainMessage
	row := table.Row{index, hostip, parameter, status, message}
	return row
}

func subMessageRow(subMsg string, rowData *Info, reporting Reporting) table.Row {
	var newSubMessage string
	if rowData.Status == "Failed" {
		key := reporting.AppendSpecialCharater(Failed, reporting.AppendSpecialCharater(SquareBracket, rowData.Status))
		newSubMessage = reporting.ChangeColour(Red, key+subMsg)
	} else {
		key := reporting.ChangeColour(Yellow, reporting.AppendSpecialCharater(Warning, reporting.AppendSpecialCharater(SquareBracket, "Warning")))
		newSubMessage = key + subMsg
	}
	row := table.Row{"", "", "", "", newSubMessage}
	return row
}

func createSummaryTableRowData(summary map[string]SummaryInfo, rowData *Info) map[string]SummaryInfo {
	var toResolve []string
	if rowData.SummaryInfo.ToResolve != nil && len(rowData.SummaryInfo.ToResolve) > 0 {
		toResolve = []string{fmt.Sprintf("%s:", rowData.Hostip)}
		rowData.SummaryInfo.ToResolve = createIndexForToResolve(rowData.SummaryInfo.ToResolve)
		toResolve = append(toResolve, rowData.SummaryInfo.ToResolve...)
	}
	if _, ok := summary[rowData.Parameter]; ok {
		singleParameterSummary := summary[rowData.Parameter]
		singleParameterSummary.SuccessfulCount = singleParameterSummary.SuccessfulCount + rowData.SummaryInfo.SuccessfulCount
		singleParameterSummary.FailedCount = singleParameterSummary.FailedCount + rowData.SummaryInfo.FailedCount
		singleParameterSummary.ToResolve = append(singleParameterSummary.ToResolve, toResolve...)

		summary[rowData.Parameter] = singleParameterSummary
		return summary
	}

	summary[rowData.Parameter] = SummaryInfo{
		SuccessfulCount: rowData.SummaryInfo.SuccessfulCount,
		FailedCount:     rowData.SummaryInfo.FailedCount,
		ToResolve:       toResolve,
	}

	return summary
}

func createSummaryTableRows(rowData map[string]SummaryInfo, reporting Reporting) []table.Row {
	var rows []table.Row
	for key, value := range rowData {
		if key != "" {
			singleParameterRow := createSingleParameterRows(key, value, reporting)
			rows = append(rows, singleParameterRow...)
		}
	}
	return rows
}

func createSingleParameterRows(parameter string, parameterSummary SummaryInfo, reporting Reporting) []table.Row {
	var row table.Row
	var rows []table.Row
	successCount := reporting.ChangeColour(Green, fmt.Sprintf("%d", parameterSummary.SuccessfulCount))
	failedCount := reporting.ChangeColour(Red, fmt.Sprintf("%d", parameterSummary.FailedCount))
	toResolve := parameterSummary.ToResolve
	if len(toResolve) > 1 {
		row = table.Row{parameter, successCount, failedCount, toResolve[0]}
		rows = append(rows, row)
		for _, value := range toResolve[1:] {
			row = table.Row{"", "", "", value}
			rows = append(rows, row)
		}
	} else {
		row = table.Row{parameter, successCount, failedCount, ""}
		rows = append(rows, row)
	}
	return rows
}

func updateTableTitle(reporting Reporting, nodeInfo map[string][]Info, keyMap map[string][]string) {
	var failed bool
	for key, value := range nodeInfo {
		if _, ok := keyMap[key]; !ok {
			continue
		}
		title := reporting.GetTable(keyMap[key][0]).Title
		if checkStatusNotEmpty(value) {
			return
		}
		failed = checkFailedStatus(value)
		if failed {
			updatedTile := reporting.ChangeColour(Red, reporting.AppendSpecialCharater(Failed, title))
			reporting.GetTable(keyMap[key][0]).Title = updatedTile
		} else {
			updatedTile := reporting.ChangeColour(Green, reporting.AppendSpecialCharater(Success, title))
			reporting.GetTable(keyMap[key][0]).Title = updatedTile
		}
	}
}

func checkStatusNotEmpty(info []Info) bool {
	for _, v := range info {
		if v.Status == "" {
			return true
		}
	}
	return false
}

func checkFailedStatus(info []Info) bool {
	for _, v := range info {
		if v.Status == "Failed" {
			return true
		}
	}
	return false
}

func matchNodeNameWithRespectiveTablesName(substring string, keys []string) []string {
	var matchingKeys []string
	for _, key := range keys {
		if strings.Contains(strings.ToUpper(key), strings.ToUpper(substring)) {
			matchingKeys = append(matchingKeys, key)
		}
	}
	sort.Strings(matchingKeys)
	return matchingKeys
}

func createIndexForToResolve(data []string) []string {
	for key, value := range data {
		data[key] = fmt.Sprintf("%d. %s", key+1, value)
	}
	return data
}
