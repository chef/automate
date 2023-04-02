package reporting

import (
	"sort"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/fatih/color"
	"github.com/jedib0t/go-pretty/table"
)

const (
	Red = iota
	Green
	Yellow
)

const (
	Failed = iota
	Success
	Warning
	SquareBracket
)

type Reporting interface {
	GetTable(key string) *Table
	SetTable(key string, table *Table)
	GetAllTableKeys() []string
	GetAllTables() map[string]*Table
	AppendSpecialCharater(symbol int, str string) string
	ChangeColour(fgColor int, msg string) string
	GenerateTableOutputAndPrint(table *Table)
}

type Table struct {
	Title     string
	Rows      []table.Row
	Header    table.Row
	Footer    table.Row
	ColConfig []table.ColumnConfig
}

type ReportingModule struct {
	writer *cli.Writer
	tables map[string]*Table
}

func NewReportingModule(wr *cli.Writer, tables map[string]*Table) Reporting {
	newReportingModule := &ReportingModule{
		writer: wr,
		tables: tables,
	}
	return newReportingModule
}

func (r *ReportingModule) GetTable(key string) *Table {
	return r.tables[key]
}

func (r *ReportingModule) GetAllTableKeys() []string {
	keys := make([]string, len(r.tables))
	i := 0
	for key := range r.tables {
		keys[i] = key
		i++
	}
	sort.Strings(keys)
	return keys
}

func (r *ReportingModule) GetAllTables() map[string]*Table {
	return r.tables
}

func (r *ReportingModule) SetTable(key string, tables *Table) {
	r.tables[key] = tables
}

func (r *ReportingModule) AppendSpecialCharater(symbol int, str string) string {
	failedSymbol := "✖ "
	successSymbol := "✔ "
	warningSymbol := "! "

	switch symbol {
	case Failed:
		msg := failedSymbol + str
		return msg
	case Success:
		msg := successSymbol + str
		return msg
	case Warning:
		msg := warningSymbol + str
		return msg
	case SquareBracket:
		msg := "[ " + str + " ] "
		return msg
	}

	return str
}

func (r *ReportingModule) ChangeColour(fgColor int, msg string) string {
	switch fgColor {
	case Red:
		return color.New(color.FgRed).Sprint(msg)
	case Green:
		return color.New(color.FgGreen).Sprint(msg)
	case Yellow:
		return color.New(color.FgYellow).Sprint(msg)
	default:
		return msg
	}
}

func (r *ReportingModule) GenerateTableOutputAndPrint(tb *Table) {
	tbWriter := table.NewWriter()
	tbWriter.SetColumnConfigs(tb.ColConfig)
	tbWriter.AppendHeader(tb.Header)
	tbWriter.AppendRows(tb.Rows)
	r.writer.Println(tb.Title)
	r.writer.Println(tbWriter.Render())
}
