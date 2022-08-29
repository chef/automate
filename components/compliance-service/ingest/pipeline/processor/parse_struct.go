package processor

import (
	"context"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/sirupsen/logrus"
)

func ParseReportCtrlStruct(ctx context.Context, client *ingestic.ESClient, reportUuid string, index string) ([]relaxting.Control, []string, error) {
	var controls []relaxting.Control
	var docIds []string
	logrus.Infof("Getting the report for report uuid %s", reportUuid)
	inspecReport, err := client.GetDocByReportUUId(ctx, reportUuid, index)
	if err != nil {
		logrus.Errorf("cannnot find inspec report: %v", err)
		return nil, nil, err
	}

	controls, docIds, err = MapStructsESInSpecReportToControls(inspecReport)
	if err != nil {
		return nil, nil, err
	}
	return controls, docIds, err
}

func MapStructsESInSpecReportToControls(inspecReport *relaxting.ESInSpecReport) ([]relaxting.Control, []string, error) {
	var controls []relaxting.Control
	var docIds []string

	var nodeStatus string

	for _, value := range inspecReport.Profiles {
		for _, value2 := range value.Controls {
			nodes := make([]relaxting.Node, 0)
			// Get the nodes
			node := &relaxting.Node{NodeUUID: inspecReport.NodeID,
				DailyLatest:      true,
				DayLatest:        true,
				NodeEndTime:      inspecReport.EndTime,
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
				JobID:            inspecReport.JobID,
			}
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
				nodeStatus = "waived"
			} else {
				nodeStatus = value2.Status
			}
			ctrl.Status = nodeStatus
			node.Status = nodeStatus
			nodes = append(nodes, *node)
			ctrl.Nodes = nodes
			ctrl.Profile = relaxting.Profile{ProfileID: value.SHA256, Title: value.Title}
			controls = append(controls, ctrl)
			docIds = append(docIds, ingestic.GetDocIdByControlIdAndProfileID(ctrl.ControlID, ctrl.Profile.ProfileID))
		}
	}
	return controls, docIds, nil
}

func checkIfControlWaivedStatus(waivedStr string, waivedData interface{}) bool {
	if (waivedStr == "yes" || waivedStr == "yes_run") && waivedData != nil {
		return true
	}

	return false

}
