package processor

import (
	"context"
	"encoding/json"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/olivere/elastic/v7"
	"time"

	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/sirupsen/logrus"
)

type Control struct {
	ControlID   string                                      `json:"control_id"`
	Title       string                                      `json:"title"`
	WaivedStr   string                                      `json:"waived_str"`
	WaiverData  interface{}                                 `json:"waiver_data"`
	Impact      float64                                     `json:"impact"`
	EndTime     time.Time                                   `json:"end_time"`
	DailyLatest bool                                        `json:"daily_latest"`
	DayLatest   bool                                        `json:"day_latest"`
	Status      string                                      `json:"status"`
	Nodes       []Node                                      `json:"nodes"`
	StringTags  []relaxting.ESInSpecReportControlStringTags `json:"string_tags"`
	Profile     Profile                                     `json:"profile"`
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

func ParseReportCtrlStruct(ctx context.Context, client *ingestic.ESClient, reportUuid string, index string) ([]Control, error) {
	var controls []Control
	inspecReport, err := GetDocByReportUUId(context.Background(), reportUuid, index, client)
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

func MapStructsESInSpecReportToControls(inspecReport *relaxting.ESInSpecReport) ([]Control, error) {
	var controls []Control

	nodes := make([]Node, 0)
	// Get the nodes
	node := Node{NodeUUID: inspecReport.NodeID,
		DailyLatest: inspecReport.DailyLatest,
		DayLatest:   inspecReport.DayLatest,
		NodeEndTime: inspecReport.EndTime,
		Status:      inspecReport.Status,
		ReportUUID:  inspecReport.ReportID}

	nodes = append(nodes, node)

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
			ctrl.StringTags = value2.StringTags

			if checkIfControlWaivedStatus(value2.WaivedStr, value2.WaiverData) {
				ctrl.Status = "waived"
			} else {
				ctrl.Status = value2.Status
			}
			ctrl.Nodes = nodes
			ctrl.Profile = Profile{ProfileID: value.Profile}
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

func GetDocByReportUUId(ctx context.Context, reportUuid string, index string, client *ingestic.ESClient) (*relaxting.ESInSpecReport, error) {
	logrus.Debug("Fetching project by UUID")

	var item relaxting.ESInSpecReport
	boolQuery := elastic.NewBoolQuery()

	idsQuery := elastic.NewIdsQuery()
	idsQuery.Ids(reportUuid)
	boolQuery = boolQuery.Must(idsQuery)
	fsc := elastic.NewFetchSourceContext(true)
	searchSource := elastic.NewSearchSource().
		FetchSourceContext(fsc).
		Query(boolQuery).
		Size(1)

	searchResult, err := client.Client.Search().
		SearchSource(searchSource).
		Index(index).
		FilterPath(
			"took",
			"hits.total",
			"hits.hits._id",
			"hits.hits._source",
			"hits.hits.inner_hits").
		Do(ctx)

	if err != nil {
		return nil, err
	}

	if searchResult.TotalHits() > 0 {
		logrus.Printf("Found a total of %d ESInSpecReport\n", searchResult.TotalHits())
		// Iterate through results
		for _, hit := range searchResult.Hits.Hits {
			// hit.Index contains the name of the index
			if hit.Source != nil {
				// Deserialize hit.Source into a ESInSpecReport (could also be just a map[string]interface{}).
				err := json.Unmarshal(hit.Source, &item)
				if err != nil {
					logrus.Errorf("Received error while unmarshling %+v", err)
				}

			}

		}
	}

	return &item, nil
}
