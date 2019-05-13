package relaxting

import (
	"encoding/json"
	"fmt"
	"net/http"
	"strings"
	"time"

	"github.com/chef/automate/lib/grpc/auth_context"
	"golang.org/x/net/context"

	"github.com/olivere/elastic"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type ES2Backend struct {
	ESUrl             string
	Enterprise        string
	ChefDeliveryUser  string
	ChefDeliveryToken string
}

// ReportingTransport structure for Automate login
type ReportingTransport struct {
	Enterprise        string
	ChefDeliveryUser  string
	ChefDeliveryToken string
}

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
	//logrus.Debugf("Creating ES client...")
	return elastic.NewClient(
		elastic.SetHttpClient(backend.getHttpClient()),
		elastic.SetURL(backend.ESUrl),
		elastic.SetSniff(false),
		//elastic.SetTraceLog(logrus.New()), // logging from the elastic library. We could enable this when the log level is debug
	)
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

func rightSplit(stringToSplit string, delimiter string) (string, string) {
	n := strings.LastIndex(stringToSplit, delimiter)
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
	logrus.Debugf("\n------------------ %s-(start)--[%s]---------------%s \n------------------ %s-(end)-----------------------------------\n",
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
// useStartTime should normally be false unless you have a good reason to make it true.
// A good reason would be when you pass a job_id and you don't know when it ran so you want to search all indices
func GetEsIndex(filters map[string][]string, useSummaryIndex bool, useStartTime bool) (esIndex string, err error) {
	var startDateAsString string
	endDateAsString, err := computeIndexDate(firstOrEmpty(filters["end_time"]))
	if err != nil {
		return esIndex, err
	}

	if useStartTime {
		startDateAsString = firstOrEmpty(filters["start_time"])
	} else {
		startDateAsString = endDateAsString
	}

	jobId := firstOrEmpty(filters["job_id"])

	////A job_id filter forces us to use the timeseries as there is no guarantee that the latest index will have it
	////todo if we at least have a jobid->date map (store in dedicated index), we can at least know which timeseries to search making it fast
	if len(jobId) == 0 {
		if useSummaryIndex {
			esIndex, err = IndexDates(CompDailySumIndexPrefix, startDateAsString, endDateAsString)
		} else {
			esIndex, err = IndexDates(CompDailyRepIndexPrefix, startDateAsString, endDateAsString)
		}

	} else {
		if useSummaryIndex {
			esIndex = ComplianceDailySumTwenty
		} else {
			esIndex = ComplianceDailyRepTwenty
		}
	}

	logrus.Debugf("GetEsIndex, using indices: %s", esIndex)
	return esIndex, err
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
