package reporting

import (
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/fatih/color"
	"github.com/jedib0t/go-pretty/table"
)

type Reporting interface {
	GetTable(key string) *Table
	SetTable(key string, tables *Table)
	AppendSpecialCharater(int, string) string
	ChangeColour(fgColor string, msg string) string
	GenerateTableOutputAndPrint(tb *Table)
	StartSpinnerForService()
	StopSpinnerForService(symbol string, service string)
}

type Table struct {
	Rows      []table.Row
	Header    table.Row
	Footer    table.Row
	ColConfig []table.ColumnConfig
}

type ReportingModule struct {
	writer         *cli.Writer
	spinner        *spinner.Spinner
	spinnerTimeout time.Duration
	tables         map[string]*Table
}

func NewReportingModule(wr *cli.Writer, spinnerTimeout time.Duration, tables map[string]*Table) Reporting {
	newReportingModule := &ReportingModule{
		writer:         wr,
		spinnerTimeout: spinnerTimeout,
		tables:         tables,
	}
	return newReportingModule
}

func (r *ReportingModule) GetTable(key string) *Table {
	return r.tables[key]
}

func (r *ReportingModule) SetTable(key string, tables *Table) {
	r.tables[key] = tables
}

func (r *ReportingModule) AppendSpecialCharater(symbol int, str string) string {
	failedSymbol := "✖" + " "
	successSymbol := "✔" + " "
	warningSymbol := "!" + " "

	switch symbol {
	case 1:
		msg := failedSymbol + str
		return msg
	case 2:
		msg := successSymbol + str
		return msg
	case 3:
		msg := warningSymbol + str
		return msg
	case 4:
		msg := "[ " + str + " ] "
		return msg
	}

	return str
}

func (r *ReportingModule) ChangeColour(fgColor string, msg string) string {
	switch fgColor {
	case "red":
		return color.New(color.FgRed).Sprint(msg)
	case "green":
		return color.New(color.FgGreen).Sprint(msg)
	case "yellow":
		return color.New(color.FgHiYellow).Sprint(msg)
	default:
		return msg
	}
}

func (r *ReportingModule) GenerateTableOutputAndPrint(tb *Table) {
	tbWriter := table.NewWriter()
	tbWriter.SetColumnConfigs(tb.ColConfig)
	tbWriter.AppendHeader(tb.Header)
	tbWriter.AppendRows(tb.Rows)
	r.writer.Println(tbWriter.Render())
}

func (r *ReportingModule) StartSpinnerForService() {
	r.spinner = r.writer.NewSpinner()
	r.spinner.Suffix = fmt.Sprintf("  Waiting")
	r.spinner.Start()
	time.Sleep(r.spinnerTimeout)
}

func (r *ReportingModule) StopSpinnerForService(symbol string, service string) {
	symbol = "✔" + symbol
	r.spinner.FinalMSG = " " + color.New(color.FgGreen).Sprint(symbol) + " " + service
	r.spinner.Stop()
	r.writer.Println("")
}
