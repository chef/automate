package processor

import (
	"context"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/sirupsen/logrus"
)

func ParseReportCtrlStruct(ctx context.Context, client *ingestic.ESClient, reportUuid string, index string) ([]relaxting.Control, error) {
	var controls []relaxting.Control
	inspecReport, err := client.GetDocByReportUUId(context.Background(), reportUuid, index)
	if err != nil {
		logrus.Errorf("cannnot find inspec report: %v", err)
		return nil, err
	}

	controls, err = MapStructsESInSpecReportToControls(inspecReport)
	if err != nil {
		return nil, err
	}
	return controls, err
}

func MapStructsESInSpecReportToControls(inspecReport *relaxting.ESInSpecReport) ([]relaxting.Control, error) {
	var controls []relaxting.Control

	nodes := make([]relaxting.Node, 0)
	// Get the nodes
	node := relaxting.Node{NodeUUID: inspecReport.NodeID,
		DailyLatest:      true,
		DayLatest:        true,
		NodeEndTime:      inspecReport.EndTime,
		Status:           inspecReport.Status,
		ReportUUID:       inspecReport.ReportID,
		NodeName:         inspecReport.NodeName,
		Environment:      inspecReport.Environment,
		PolicyGroup:      inspecReport.PolicyGroup,
		PolicyName:       inspecReport.PolicyName,
		Platform:         inspecReport.Platform,
		Recipes:          inspecReport.Recipes,
		Roles:            inspecReport.Roles,
		OrganizationName: inspecReport.OrganizationName,
		SourceFQDN:       inspecReport.SourceFQDN,
		ChefTags:         inspecReport.ChefTags,
	}

	nodes = append(nodes, node)

	for _, value := range inspecReport.Profiles {
		for _, value2 := range value.Controls {
			ctrl := relaxting.Control{}
			ctrl.ControlID = value2.ID
			ctrl.Title = value2.Title
			ctrl.WaivedStr = value2.WaivedStr
			ctrl.WaiverData = value2.WaiverData
			ctrl.Impact = float64(value2.Impact)
			ctrl.EndTime = inspecReport.EndTime
			ctrl.DailyLatest = true
			ctrl.DayLatest = true
			ctrl.StringTags = value2.StringTags

			if checkIfControlWaivedStatus(value2.WaivedStr, value2.WaiverData) {
				ctrl.Status = "waived"
			} else {
				ctrl.Status = value2.Status
			}
			ctrl.Nodes = nodes
			ctrl.Profile = relaxting.Profile{ProfileID: value.Profile, Title: value.Title}
			controls = append(controls, ctrl)
		}
	}
	return controls, nil
}

func checkIfControlWaivedStatus(waivedStr string, waivedData interface{}) bool {
	if (waivedStr == "yes" || waivedStr == "yes_run") && waivedData != nil {
		return true
	}

	return false

}
