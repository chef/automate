package usagegenerator

import (
	"context"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"strconv"
	"time"

	cli "github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/gocarina/gocsv"
	elastic "github.com/olivere/elastic/v7"
)

type ConvergeInfo struct {
	NodeID             string `json:"entity_uuid"`
	Environment        string `json:"environment"`
	FQDN               string `json:"fqdn"`
	HostName           string `json:"hostname"`
	IPAddress          string `json:"ipaddress"`
	MacAddress         string `json:"macaddress"`
	Platform           string `json:"platform"`
	PlatformFamily     string `json:"platform_family"`
	PlatformVersion    string `json:"platform_version"`
	UptimeSeconds      int64  `json:"uptime_seconds"`
	Status             string `json:"client_run_status"`
	StartTime          string `json:"start_time"`
	EndTime            string `json:"end_time"`
	SerialNumber       string `json:"dmi_system_serial_number"`
	TotalResourceCount int    `json:"total_resource_count"`
}

type ComplianceInfo interface{}

var url = "https://%s:%s"
var errorcsv = "Error while writing csv: "
var errorQuery = "Error in query: "
var osGatewayPort = 10144

func elasticSearchConnection(url string, esHostName string, esPort string, esUserName string, esPassword string) *elastic.Client {
	tr := &http.Transport{
		TLSClientConfig: &tls.Config{
			MinVersion:         tls.VersionTLS12,
			InsecureSkipVerify: true,
		},
	}
	elasticSearchURL, err := getElasticSearchURL(esHostName, esPort, url)
	if err != nil {
		fmt.Println("Failed to resolve OpenSearch connection: ", err.Error())
		os.Exit(1)
	}
	client := &http.Client{Transport: tr}
	esclient, err := elastic.NewClient(
		elastic.SetHttpClient(client),
		elastic.SetURL(elasticSearchURL),
		elastic.SetSniff(false),
		elastic.SetBasicAuth(esUserName, esPassword),
	)
	if err != nil {
		fmt.Println("Elastic error : ", err)
		os.Exit(1)
	}
	return esclient
}

func getElasticSearchURL(esHostName string, esPort string, url string) (string, error) {
	var configTimeOut int64 = 10
	res, err := cli.GetAutomateConfig(configTimeOut)
	if err != nil {
		return "", fmt.Errorf("error while trying to get Automate Config: %s", err.Error())
	}
	externalOsEnabled := res.Config.GetGlobal().GetV1().GetExternal().GetOpensearch().GetEnable().GetValue()

	// If OpenSearch is deployed externally, connect to the os-gateway instead of OpenSearch directly
	if externalOsEnabled {
		return fmt.Sprintf("http://localhost:%d", osGatewayPort), nil
	}

	if esPort == strconv.Itoa(osGatewayPort) {
		url = "http://%s:%s"
	}
	return fmt.Sprintf(url, esHostName, esPort), nil
}

func GenerateNodeCount(esHostName string, esPort string, esUserName string, esPassword string, startTime time.Time, endTime time.Time, fileName string) {
	client := elasticSearchConnection(url, esHostName, esPort, esUserName, esPassword)
	queryElasticSearchNodeCount(client, startTime, endTime, fileName)
}

func GenerateNodeRunReport(esHostName string, esPort string, esUsername string, esPassword string, startTime time.Time, endTime time.Time, fileName string) {
	client := elasticSearchConnection(url, esHostName, esPort, esUsername, esPassword)
	queryElasticSearchNodeReport(client, startTime, endTime, fileName)
}

func queryElasticSearchNodeCount(client *elastic.Client, startTime time.Time, endTime time.Time, fileName string) {
	f, err := os.OpenFile(fileName, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		fmt.Println("Failed to open Node Count report")
		os.Exit(1)
	}
	defer f.Close()
	writer := gocsv.DefaultCSVWriter(f)
	err = writer.Write([]string{"Start Time", "End Time", "Unique Node Count"})
	if err != nil {
		fmt.Println(errorcsv, err)
		os.Exit(1)
	}

	st := endTime.Add(24 * time.Hour)
	for {
		et := st
		st = et.Add(24 * -time.Hour)

		err = writer.Write([]string{"Day Count"})
		if err != nil {
			fmt.Println(errorcsv, err)
			os.Exit(1)
		}
		dayCount, ok := getUniqueCounts(client, st, et)
		if ok {
			err = writer.Write([]string{st.Format(time.RFC3339), et.Format(time.RFC3339), fmt.Sprintf("%f", *dayCount.Value)})
			if err != nil {
				fmt.Println(errorcsv, err)
				os.Exit(1)
			}
		}
		t := et.Add(-time.Hour)
		if t.Before(st) {
			t = st
		}
		err = writer.Write([]string{"Hourly Count"})
		if err != nil {
			fmt.Println(errorcsv, err)
			os.Exit(1)
		}
		writer.Flush()
		for {
			metric, ok := getUniqueCounts(client, t, et)
			if ok && *metric.Value > 0 {
				err = writer.Write([]string{t.Format(time.RFC3339), et.Format(time.RFC3339), fmt.Sprintf("%f", *metric.Value)})
				if err != nil {
					fmt.Println(errorcsv, err)
					os.Exit(1)
				}
				writer.Flush()
			}

			if t == st {
				break
			}

			et = t
			t = et.Add(-time.Hour)
			if t.Before(st) {
				t = st
			}
		}
		err = writer.Write([]string{"", "", ""})
		if err != nil {
			fmt.Println(errorcsv, err)
			os.Exit(1)
		}
		writer.Flush()
		if st == startTime {
			break
		}
	}
	writer.Flush()
	fmt.Println("The node count report is available in: ", fileName)
}

func getUniqueCounts(client *elastic.Client, startTime time.Time, endTime time.Time) (*elastic.AggregationValueMetric, bool) {
	rangeQuery := elastic.NewRangeQuery("end_time")
	rangeQuery.Gte(startTime.Format(time.RFC3339))
	rangeQuery.Lte(endTime.Format(time.RFC3339))

	aggr := elastic.NewCardinalityAggregation().Field("entity_uuid")
	searchService := client.Search().
		Query(rangeQuery).
		Index("converge-history-*").
		Size(100).
		Aggregation("nodes_count", aggr)

	searchResult, err := searchService.Do(context.Background())
	if err != nil {
		fmt.Println(errorQuery, err)
		os.Exit(1)
	}

	metric, ok := searchResult.Aggregations.ValueCount("nodes_count")
	return metric, ok
}

func get(key string, s interface{}) interface{} {
	return s.(map[string]interface{})[key]
}

func queryElasticSearchNodeReport(client *elastic.Client, startTime time.Time, endTime time.Time, fileName string) {
	f, err := os.OpenFile(fileName, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		fmt.Println("Failed to open Node Detail report")
		os.Exit(1)
	}
	defer f.Close()

	writer := gocsv.DefaultCSVWriter(f)

	var sourceFields = []string{
		"entity_uuid",
		"environment",
		"fqdn",
		"hostname",
		"ipaddress",
		"macaddress",
		"platform",
		"platform_family",
		"platform_version",
		"uptime_seconds",
		"client_run_status",
		"start_time",
		"end_time",
		"dmi_system_serial_number",
		"total_resource_count",
	}

	t := endTime.Add(24 * time.Hour)
	if t.Before(startTime) {
		t = startTime
	}

	for {
		rangeQuery := elastic.NewRangeQuery("end_time")
		rangeQuery.Gte(t.Format(time.RFC3339))
		rangeQuery.Lte(endTime.Format(time.RFC3339))

		fetchSource := elastic.NewFetchSourceContext(true).Include(sourceFields...)
		searchService := client.Search().
			Query(rangeQuery).
			Index("converge-history-*").
			Size(10000).
			FetchSourceContext(fetchSource)

		searchResult, err := searchService.Do(context.Background())
		if err != nil {
			fmt.Println(errorQuery, err)
			os.Exit(1)
		}

		if searchResult.TotalHits() > 0 {

			for ind, hit := range searchResult.Hits.Hits {
				var s ConvergeInfo
				err = json.Unmarshal(hit.Source, &s)
				if err != nil {
					fmt.Println("Json marshal err :", err)
					os.Exit(1)
				}

				if ind == 0 {
					err = gocsv.Marshal([]ConvergeInfo{s}, f)
					if err != nil {
						fmt.Println(errorcsv, err)
						os.Exit(1)
					}
				} else {
					err = gocsv.MarshalWithoutHeaders([]ConvergeInfo{s}, f)
					if err != nil {
						fmt.Println(errorcsv, err)
						os.Exit(1)
					}
					writer.Flush()
				}
			}
		}
		if t == startTime {
			break
		}

		endTime = t
		t = endTime.Add(-24 * time.Hour)
		if t.Before(startTime) {
			t = startTime
		}
	}
	fmt.Println("The details of the runs can be found in : ", fileName)
}

func GenerateComplianceResourceRunCount(esHostName string, esPort string, esUsername string, esPassword string, startTime time.Time, endTime time.Time, fileName string) {
	client := elasticSearchConnection(url, esHostName, esPort, esUsername, esPassword)
	queryElasticSearchComplianceResourceCount(client, startTime, endTime, fileName)
}

func GenerateComplianceResourceRunReport(esHostName string, esPort string, esUsername string, esPassword string, startTime time.Time, endTime time.Time, fileName string) {
	client := elasticSearchConnection(url, esHostName, esPort, esUsername, esPassword)
	queryElasticSearchComplianceResourceRunReport(client, startTime, endTime, fileName)
}

func queryElasticSearchComplianceResourceCount(client *elastic.Client, startTime time.Time, endTime time.Time, fileName string) {
	f, err := os.OpenFile(fileName, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	errorMessage("Failed to open Resource Count report", err)

	defer f.Close()
	writer := gocsv.DefaultCSVWriter(f)
	err = writer.Write([]string{"Start Time", "End Time", "Unique Resource Count"})
	errorMessage(errorcsv, err)

	st := endTime.Add(24 * time.Hour)
	for {
		et := st
		st = et.Add(24 * -time.Hour)

		err = writer.Write([]string{"Day Count"})
		errorMessage(errorcsv, err)
		dayCount, ok := getUniqueComplianceCounts(client, st, et)
		if ok {
			err = writer.Write([]string{st.Format(time.RFC3339), et.Format(time.RFC3339), fmt.Sprintf("%f", *dayCount.Value)})
			errorMessage(errorcsv, err)
		}
		t := et.Add(-time.Hour)
		endTimeBeforeStartTime(t, st)
		err = writer.Write([]string{"Hourly Count"})
		errorMessage(errorcsv, err)

		writer.Flush()
		for {
			metric, ok := getUniqueComplianceCounts(client, t, et)
			if ok && *metric.Value > 0 {
				err = writer.Write([]string{t.Format(time.RFC3339), et.Format(time.RFC3339), fmt.Sprintf("%f", *metric.Value)})
				errorMessage(errorcsv, err)
				writer.Flush()
			}

			if t == st {
				break
			}

			et = t
			t = et.Add(-time.Hour)
			endTimeBeforeStartTime(t, st)
		}
		err = writer.Write([]string{"", "", ""})
		errorMessage(errorcsv, err)

		writer.Flush()
		if st == startTime {
			break
		}
	}
	writer.Flush()
	fmt.Println("The resource count report is available in: ", fileName)
}

func getUniqueComplianceCounts(client *elastic.Client, startTime time.Time, endTime time.Time) (*elastic.AggregationValueMetric, bool) {
	rangeQuery := elastic.NewRangeQuery("end_time")
	rangeQuery.Gte(startTime.Format(time.RFC3339))
	rangeQuery.Lte(endTime.Format(time.RFC3339))

	aggr := elastic.NewCardinalityAggregation().Field("node_uuid")
	searchService := client.Search().
		Query(rangeQuery).
		Index("comp-7-r-*").
		Size(100).
		Aggregation("nodes_count", aggr)

	searchResult, err := searchService.Do(context.Background())
	errorMessage(errorQuery, err)

	metric, ok := searchResult.Aggregations.ValueCount("nodes_count")
	return metric, ok
}

func queryElasticSearchComplianceResourceRunReport(client *elastic.Client, startTime time.Time, endTime time.Time, fileName string) {
	f, err := os.OpenFile(fileName, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	errorMessage("Failed to open Compliance scan detail report", err)
	defer f.Close()

	w := gocsv.DefaultCSVWriter(f)

	var sourceField = []string{
		"node_uuid",
		"version",
		"node_name",
		"environment",
		"platform.name",
		"controls_sums.total",
		"controls_sums.passed.total",
		"controls_sums.skipped.total",
		"controls_sums.failed.total",
		"profiles.name",
		"profiles.title",
		"profiles.version",
		"end_time",
	}

	t := endTime.Add(24 * time.Hour)

	if t.Before(startTime) {
		t = startTime
	}
	header := true
	for {
		rangeQuery := elastic.NewRangeQuery("end_time")
		rangeQuery.Gte(t.Format(time.RFC3339))
		rangeQuery.Lte(endTime.Format(time.RFC3339))

		fetchSource := elastic.NewFetchSourceContext(true).Include(sourceField...)

		searchService := client.Search().
			Query(rangeQuery).
			Index("comp-7-r-*").
			Size(10000).
			FetchSourceContext(fetchSource)

		searchResult, err := searchService.Do(context.Background())
		errorMessage(errorQuery, err)

		if searchResult.TotalHits() > 0 {
			if header == true {
				headers := []string{"Resource_ID", "Version", "Resource_Name", "Environment", "End_Time", "Platform__Name", "Controls_Sums__Total", "Controls_Sums__Passed", "Controls_Sums__Skipped", "Controls_Sums__Failed", "Profiles__Count"}
				err := w.Write(headers)
				errorMessage(errorcsv, err)
				w.Flush()
				err = w.Error()
				errorMessage(errorcsv, err)
			}

			var s ComplianceInfo

			for _, hit := range searchResult.Hits.Hits {

				err = json.Unmarshal(hit.Source, &s)
				errorMessage("Json marshal err :", err)

				record := []string{
					get("node_uuid", s).(string),
					get("version", s).(string),
					get("node_name", s).(string),
					get("environment", s).(string),
					get("end_time", s).(string),
					get("name", get("platform", s)).(string),
					fmt.Sprint(get("total", get("controls_sums", s))),
					fmt.Sprint(get("total", get("passed", get("controls_sums", s)))),
					fmt.Sprint(get("total", get("skipped", get("controls_sums", s)))),
					fmt.Sprint(get("total", get("failed", get("controls_sums", s)))),
					strconv.Itoa(len(get("profiles", s).([]interface{}))),
				}
				err := w.Write(record)
				errorMessage(errorcsv, err)
				w.Flush()
				header = false
				err = w.Error()
				errorMessage(errorcsv, err)
			}
		}
		if t == startTime {
			break
		}

		endTime = t
		t = endTime.Add(-24 * time.Hour)
		if t.Before(startTime) {
			t = startTime
		}
	}
	fmt.Println("The details of the runs can be found in : ", fileName)
}

func errorMessage(message string, err error) {
	if err != nil {
		fmt.Println(message, err)
		os.Exit(1)
	}
}

func endTimeBeforeStartTime(t time.Time, st time.Time) {
	if t.Before(st) {
		t = st
	}
}
