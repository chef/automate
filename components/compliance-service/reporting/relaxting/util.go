package relaxting

import (
	"context"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"net/http"
	"strings"
	"sync"
	"time"

	"github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"

	structpb "github.com/golang/protobuf/ptypes/struct"
	elastic "github.com/olivere/elastic/v7"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/grpc/auth_context"
)

type ES2Backend struct {
	ESUrl             string
	Enterprise        string
	ChefDeliveryUser  string
	ChefDeliveryToken string
	PGdb              *pgdb.DB
}

// ReportingTransport structure for Automate login
type ReportingTransport struct {
	Enterprise        string
	ChefDeliveryUser  string
	ChefDeliveryToken string
}

var once sync.Once
var esClient *elastic.Client

var err error

const (
	InvalidLevel = iota - 1
	ReportLevel
	ProfileLevel
	ControlLevel
)

func (backend *ES2Backend) getHttpClient() *http.Client {
	var httpClient *http.Client
	//We need to create a httpClient with a custom transport if we want to pass in headers with each request
	if len(backend.ChefDeliveryUser) > 0 {
		httpClient = &http.Client{
			Transport: &ReportingTransport{
				Enterprise:        backend.Enterprise,
				ChefDeliveryUser:  backend.ChefDeliveryUser,
				ChefDeliveryToken: backend.ChefDeliveryToken,
			},
		}
	} else {
		// if we pass in nil, elastic lib will set it to http.DefaultClient which is what we want if
		// not setting Token in header.. this means we are running on localhost.
		httpClient = nil
	}
	return httpClient
}

func (backend *ES2Backend) ES2Client() (*elastic.Client, error) {
	//this is now a singleton as per best practice as outlined in the comment section of the elastic.NewClient
	tr := &http.Transport{
		TLSClientConfig: &tls.Config{
			MinVersion:         tls.VersionTLS12,
			InsecureSkipVerify: true,
		},
	}
	client := &http.Client{Transport: tr}
	once.Do(func() {
		esClient, err = elastic.NewClient(
			elastic.SetHttpClient(client),
			elastic.SetURL(backend.ESUrl),
			elastic.SetSniff(false),
			elastic.SetBasicAuth("admin", "admin"),
		)
	})
	return esClient, err
}

func (backend *ES2Backend) ReindexStatus(ctx context.Context, taskID string) (bool, error) {
	tasksGetTaskResponse, err := elastic.NewTasksGetTaskService(esClient).
		TaskId(taskID).
		WaitForCompletion(false).
		Do(ctx)
	if err != nil {
		return false, err
	}

	return tasksGetTaskResponse.Completed, nil
}

// Removes element with index i from array arr
func Remove(arr *[]string, i int) {
	if len(*arr) <= i {
		return
	}
	s := *arr
	s = append(s[:i], s[i+1:]...)
	*arr = s
}

// Splits a string by its rightmost delimiter
func rightSplit(stringToSplit string, delimiter string) (string, string) {
	n := strings.LastIndex(stringToSplit, delimiter)
	if n >= 0 {
		return stringToSplit[0:n], stringToSplit[n+1:]
	}
	return stringToSplit, ""
}

// Splits a string by its leftmost delimiter
func leftSplit(stringToSplit string, delimiter string) (string, string) {
	n := strings.Index(stringToSplit, delimiter)
	if n >= 0 {
		return stringToSplit[0:n], stringToSplit[n+1:]
	}
	return stringToSplit, ""
}

func (tr *ReportingTransport) RoundTrip(r *http.Request) (*http.Response, error) {
	r.Header.Add("chef-delivery-enterprise", tr.Enterprise)
	r.Header.Add("chef-delivery-user", tr.ChefDeliveryUser)
	r.Header.Add("chef-delivery-token", tr.ChefDeliveryToken)
	return http.DefaultTransport.RoundTrip(r)
}

func LogQueryPartMin(indices string, partToPrint interface{}, name string) {
	part, err := json.Marshal(partToPrint)
	if err != nil {
		logrus.Errorf("%s", err)
	}
	stringPart := string(part)
	if stringPart == "null" {
		stringPart = ""
	} else {
		stringPart = "\n" + stringPart
	}
	logrus.Infof("\n------------------ %s-(start)--[%s]---------------%s \n------------------ %s-(end)-----------------------------------\n",
		name, indices, stringPart, name)
}

// firstOrEmpty returns the first element of an array of empty string for an empty array
func firstOrEmpty(arr []string) string {
	if len(arr) > 0 {
		return arr[0]
	}
	return ""
}

// MapKeys returns the keys of a map as an array
func MapKeys(m map[string]string) []string {
	keys := make([]string, len(m))
	i := 0
	for k := range m {
		keys[i] = k
		i++
	}
	return keys
}

// MapValues returns the values of a map as an array
func MapValues(m map[string]string) []string {
	values := make([]string, len(m))
	i := 0
	for _, v := range m {
		values[i] = v
		i++
	}
	return values
}

// GetEsIndex returns the index(s) to query based on the end_time filter
// A good reason would be when you pass a job_id and you don't know when it ran so you want to search all indices
func GetEsIndex(filters map[string][]string, useSummaryIndex bool) (esIndex string, err error) {
	// Extract end_time from filters or set it to today's UTC day if not specified
	startDateAsString, endDateAsString, err := getStartDateAndEndDateAsString(filters)
	if err != nil {
		return esIndex, err
	}

	logrus.Debugf("GetEsIndex called with (filters=%+v), using startDateAsString=%s, endDateAsString=%s", filters, startDateAsString, endDateAsString)

	if useSummaryIndex {
		esIndex, err = IndexDates(CompDailySumIndexPrefix, startDateAsString, endDateAsString)
	} else {
		esIndex, err = IndexDates(CompDailyRepIndexPrefix, startDateAsString, endDateAsString)
	}
	logrus.Debugf("GetEsIndex, using indices: %s", esIndex)
	return esIndex, err
}

func getControlIndex(filters map[string][]string) (esIndex string, err error) {
	startDateAsString, endDateAsString, err := getStartDateAndEndDateAsString(filters)
	if err != nil {
		return esIndex, err
	}
	logrus.Debugf("GetEsIndex called with (filters=%+v), using startDateAsString=%s, endDateAsString=%s", filters, startDateAsString, endDateAsString)
	esIndex, err = IndexDates(CompDailyControlIndexPrefix, startDateAsString, endDateAsString)
	return esIndex, err
}

func getStartDateAndEndDateAsString(filters map[string][]string) (startDateAsString string, endDateAsString string, err error) {
	endDateAsString, err = computeIndexDate(firstOrEmpty(filters["end_time"]))
	if err != nil {
		return "", "", err
	}
	if len(filters["start_time"]) == 0 && len(filters["end_time"]) == 0 {
		// With `start_time` and `end_time` filters, we use start_date as yesterday's UTC date and `end_date` as today's UTC day.
		// This way, we have the indices to query the last 24 hours worth of reports
		startDateAsString = time.Now().Add(-24 * time.Hour).UTC().Format(time.RFC3339)
	} else if len(filters["start_time"]) == 0 {
		// If we have an end_time, and no start_time, setting start_time with the same value as end_time
		startDateAsString = endDateAsString
	} else {
		// Using the start_time specified in the filters
		startDateAsString, err = computeIndexDate(filters["start_time"][0])
		if err != nil {
			return "", "", err
		}
	}
	return startDateAsString, endDateAsString, nil

}

func GetFilterDepth(filters map[string][]string) int {
	if len(filters["profile_id"]) == 1 && len(filters["control"]) == 1 {
		return ControlLevel
	} else if len(filters["profile_id"]) == 1 && len(filters["control"]) == 0 {
		return ProfileLevel
	}
	//right now, if provide no profiles and no control or more than profile or control.. don't go deep.. we'll stay on the report level in this case
	//this may need to be clarified so that customers do not get confused.
	//todo - in the future, it will be best to support multiple profiles/controls while staying deep, which is more difficult than it would seem.
	return ReportLevel

	//todo - decide if more than one profile or control is invalid and if so use what we have below instead
	//return InvalidLevel, errors.New("currently, for filter, only 1 profile and 1 control are allowed Or 1 profile and no control Or no profile and no control")
}

func computeIndexDate(endTime string) (string, error) {
	var indexDate time.Time

	if len(endTime) > 0 {
		endTimeAsTime, err := time.Parse(time.RFC3339, endTime)
		if err != nil {
			return "", errors.New(fmt.Sprintf("computeIndexDate - could not parse end_time %s.", endTime))
		}
		indexDate = endTimeAsTime
	} else {
		indexDate = time.Now().UTC()
	}

	return indexDate.Format(time.RFC3339), nil
}

func FilterByProjects(ctx context.Context, filters map[string][]string) (map[string][]string, error) {
	projectsFilter, err := auth_context.ProjectsFromIncomingContext(ctx)
	if err != nil {
		return nil, err
	}
	if auth_context.AllProjectsRequested(projectsFilter) {
		return filters, nil
	}

	filters["projects"] = projectsFilter
	return filters, nil
}

// StringTagsFromProtoFields extracts tags supported from generic protobuf struct
func StringTagsFromProtoFields(tKey string, tValue *structpb.Value) *ESInSpecReportControlStringTags {
	// A null value tag is turned into an empty values array
	if _, isNullValue := tValue.GetKind().(*structpb.Value_NullValue); isNullValue {
		return &ESInSpecReportControlStringTags{
			Key:    tKey,
			Values: make([]string, 0),
		}
	}

	// A tag with one string value returns a Values array with one element
	if _, isStringValue := tValue.GetKind().(*structpb.Value_StringValue); isStringValue {
		return &ESInSpecReportControlStringTags{
			Key:    tKey,
			Values: []string{tValue.GetStringValue()},
		}
	}

	// A tag with multiple string values returns a Values array with multiple elements
	if _, isListValue := tValue.GetKind().(*structpb.Value_ListValue); isListValue {
		stringValues := make([]string, 0)
		for _, listValue := range tValue.GetListValue().Values {
			if _, isStringValue := listValue.GetKind().(*structpb.Value_StringValue); isStringValue {
				stringValues = append(stringValues, listValue.GetStringValue())
			}
		}
		if len(stringValues) > 0 {
			return &ESInSpecReportControlStringTags{
				Key:    tKey,
				Values: stringValues,
			}
		}
	}
	return nil
}

func FetchLatestDataOrNot(filters map[string][]string) bool {
	latestOnly := true
	if filters["job_id"] != nil {
		latestOnly = false
	}
	return latestOnly
}

func impactName(impactValue float64) (impact string) {
	impact = inspec.ControlImpactCritical
	if impactValue < 0.4 {
		impact = inspec.ControlImpactMinor
	} else if impactValue < 0.7 {
		impact = inspec.ControlImpactMajor
	}
	return impact
}

func getRunInfoIndex() string {
	return mappings.ComplianceRunInfo.Index
}

func ValidateTimeRangeForFilters(startTime string, endTime string) error {
	if len(startTime) <= 0 {
		logrus.Errorf("Startime cannot be null")
		return errors.Errorf("StartTime cannot be null")
	} else if startTime > endTime {
		logrus.Errorf("Start time cannot be greater than end time")
		return errors.Errorf("Start time cannot be greater than end time")
	}
	eTime, err := time.Parse(time.RFC3339, endTime)
	sTime, err := time.Parse(time.RFC3339, startTime)
	diff := int(eTime.Sub(sTime).Hours() / 24)
	if err != nil {
		logrus.Errorf("Error while getting the start time and end time diffrence:  %v", err)
		return err
	}
	if diff > 90 {
		logrus.Errorf("The diffrence between the startTime and endTime should not exceed 90 Days")
		return errors.Errorf("The diffrence between the startTime and endTime should not exceed 90 Days")
	}
	if diff == 0 {
		logrus.Errorf("The start time and end time should not be equal")
		return errors.Errorf("The start time and end time should not be equal")
	}
	return nil
}
