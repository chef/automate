package processor

import (
	"context"
	"time"

	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/sirupsen/logrus"
)

type Control struct {
	ControlID   string        `json:"control_id"`
	Title       string        `json:"title"`
	WaivedStr   string        `json:"waived_str"`
	WaiverData  []interface{} `json:"waiver_data"`
	Impact      float64       `json:"impact"`
	EndTime     time.Time     `json:"end_time"`
	DailyLatest bool          `json:"daily_latest"`
	DayLatest   bool          `json:"day_latest"`
	Status      string        `json:"status"`
	Nodes       []Node        `json:"nodes"`
	Profile     Profile       `json:"profile"`
}

type Node struct {
	NodeUUID    string    `json:"node_uuid"`
	NodeEndTime time.Time `json:"node_end_time"`
	Status      string    `json:"status"`
	DayLatest   bool      `json:"day_latest"`
	DailyLatest bool      `json:"daily_latest"`
	ReportUUID  string    `json:"report_uuid"`
}

type Profile struct {
	ProfileID string `json:"profile_id"`
}

func ParseReportCtrlStruct(client *ingestic.ESClient, data *relaxting.ESInSpecReportA2v2) ([]Control, error) {
	var controls []Control
	inspecReport, err := client.GetDocByUUID(context.Background(), data)
	if err != nil {
		logrus.Errorf("cannnot find inspec report: %v", err)
		return nil, err
	}

	// Range over insepec report
	for _, val := range inspecReport.Profiles {
		ctrl := Control{}
		for _, v := range val.Controls {
			ctrl.ControlID = v.ID
			ctrl.Title = val.Title
			// .
			// .
			// .
		}

		controls = append(controls, ctrl)
	}

	return controls, err
}
