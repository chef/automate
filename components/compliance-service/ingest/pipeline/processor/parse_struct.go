package processor

import (
	"context"
	"time"

	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/sirupsen/logrus"
)

type Control struct {
	ControlID   string      `json:"control_id"`
	Title       string      `json:"title"`
	WaivedStr   string      `json:"waived_str"`
	WaiverData  interface{} `json:"waiver_data"`
	Impact      float64     `json:"impact"`
	EndTime     time.Time   `json:"end_time"`
	DailyLatest bool        `json:"daily_latest"`
	DayLatest   bool        `json:"day_latest"`
	Status      string      `json:"status"`
	Nodes       Node        `json:"nodes"`
	Profile     Profile     `json:"profile"`
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

func ParseReportCtrlStruct(client *ingestic.ESClient, data *relaxting.ESInSpecReport) ([]Control, error) {
	var controls []Control
	inspecReport, err := client.GetDocByUUID(context.Background(), data)
	if err != nil {
		logrus.Errorf("cannnot find inspec report: %v", err)
		return nil, err
	}

	// Get the nodes
	node := Node{}
	node.NodeUUID = inspecReport.NodeID
	node.DailyLatest = inspecReport.DailyLatest
	node.DayLatest = inspecReport.DayLatest
	node.NodeEndTime = inspecReport.EndTime
	node.Status = inspecReport.Status
	node.ReportUUID = inspecReport.ReportID

	for _, value := range inspecReport.Profiles {
		for _, value2 := range value.Controls {
			ctrl := Control{}
			ctrl.ControlID = value2.ID
			ctrl.Title = value2.Title
			ctrl.WaivedStr = value2.WaivedStr
			ctrl.WaiverData = value2.WaiverData
			ctrl.Impact = float64(value2.Impact)
			ctrl.EndTime = inspecReport.EndTime
			ctrl.DailyLatest = inspecReport.DailyLatest
			ctrl.DayLatest = inspecReport.DailyLatest
			ctrl.Status = inspecReport.Status
			ctrl.Nodes = node
			ctrl.Profile = Profile{ProfileID: ""}
			controls = append(controls, ctrl)
		}
	}
	return controls, err
}
