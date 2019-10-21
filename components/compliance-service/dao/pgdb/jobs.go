package pgdb

import (
	"context"
	"database/sql"
	"encoding/json"
	"time"

	"fmt"

	"github.com/pkg/errors"

	uuid "github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	tspb "github.com/golang/protobuf/ptypes/timestamp"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/lib/errorutils"
	"github.com/chef/automate/lib/stringutils"
)

// N.B. - Constants in go use camelCase and NOT UPPER_CASE like some other languages use.
// For exported constants, go uses UpperCamelCase.  This can be seen throughout the go std lib.
// This convention makes sense when considering the fact that any var or const that begins with a capital char
// will be exported; which would not be cool!
// It's a best practice to separate our sql from our code.. here are a few options:
// todo - we could extract these statements to a separate go file but I think they make sense in here (with the model)
// todo - we could use a tool like purse to externalize all sql statements to a .sql file. nice bc/easy to test/edit etc.
// todo - look at purse: https://github.com/smotes/purse
// todo - look at dotsql https://github.com/gchaincl/dotsql for grouping all related queries in an external .sql file
// For now, at least getting in into this form of separation is good. It's simple to copy/paste to/from an sql editor,
// nicely formatted etc.. for quick testing/editing.
const selectJob = `
SELECT
  j.id,
  j.name,
  j.type,
  j.status,
  j.start_time,
  j.end_time,
  j.timeout,
  j.retries,
	j.retries_left,
	j.node_selectors,
	j.scheduled_time,
	j.recurrence,
	j.parent_id,
	j.job_count,
	j.deleted,
	COALESCE(j.node_count, 0) AS node_count,
  COALESCE(ta.tags_agg, '[]') :: JSON      AS tags,
  COALESCE(nag.nodes_agg, '[]') :: JSON    AS nodes,
  COALESCE(pag.profiles_agg, '[]') :: JSON AS profiles,
  COALESCE(rag.results_agg, '[]') :: JSON  AS results
FROM jobs j
  LEFT JOIN (SELECT
               ('[' || string_agg('{"key":"' || tags.key || '"' || ',"value": "' || tags.value || '"}', ',') ||
                ']') :: JSON AS tags_agg,
               jobs.id       AS job_id
             FROM tags, jobs, jobs_tags
             WHERE tags.id = jobs_tags.tag_id
                   AND jobs.id = jobs_tags.job_id
                   AND jobs_tags.job_id = jobs.id
             GROUP BY jobs.id) AS ta ON ta.job_id = j.id
  LEFT JOIN (SELECT
               json_agg((SELECT x
                         FROM (SELECT node_id) x)) AS nodes_agg,
               jobs.id                              AS job_id
			 FROM jobs, jobs_nodes
			 WHERE jobs.id = jobs_nodes.job_id
             GROUP BY jobs.id) AS nag ON nag.job_id = j.id
  LEFT JOIN (SELECT
               json_agg((SELECT x
                         FROM (SELECT profiles.url) x)
               ORDER BY profiles.url) AS profiles_agg,
               count(profiles.url)    AS profile_count,
               jobs.id                AS job_id
             FROM profiles, jobs, jobs_profiles
             WHERE profiles.id = jobs_profiles.profile_id AND jobs.id = jobs_profiles.job_id
             GROUP BY jobs.id) AS pag ON pag.job_id = j.id
  LEFT JOIN (SELECT
               json_agg((SELECT x
                         FROM (SELECT
                                 r.node_id,
                                 r.report_id,
                                 r.status,
                                 r.result,
                                 r.start_time,
                                 r.end_time) x)) AS results_agg,
               r.job_id
             FROM results r, jobs
             WHERE jobs.id = r.job_id
             GROUP BY r.job_id) AS rag ON j.id = rag.job_id
WHERE j.id = $1
`

const selectJobs = `
SELECT
  j.id,
  j.name,
  j.type,
  j.status,
  j.start_time,
	j.end_time,
	j.parent_id,
	j.recurrence,
	j.node_count,
	j.deleted,
  COALESCE(t.tags_agg, '[]') :: JSON AS tags,
  COALESCE(p.profile_count, 0)       AS profile_count,
  COUNT(*)
  OVER ()                            AS total_count
FROM jobs j
  LEFT JOIN (SELECT
               ('[' || string_agg('{"key":"' || tags.key || '"' || ',"value": "' || tags.value || '"}', ',') ||
                ']') :: JSON AS tags_agg,
               jobs.id       AS job_id
             FROM tags, jobs, jobs_tags
             WHERE tags.id = jobs_tags.tag_id
                   AND
                   jobs.id = jobs_tags.job_id
                   AND jobs_tags.job_id = jobs.id
             GROUP BY jobs.id) AS t ON t.job_id = j.id
  LEFT JOIN (SELECT
               jobs.id         AS job_id
             FROM nodes, jobs, jobs_nodes
             WHERE nodes.id = jobs_nodes.node_id
                   AND
                   jobs.id = jobs_nodes.job_id
             GROUP BY jobs.id) AS n ON n.job_id = j.id
  LEFT JOIN (SELECT
               count(profiles.id) AS profile_count,
               jobs.id            AS job_id
             FROM profiles, jobs, jobs_profiles
             WHERE profiles.id = jobs_profiles.profile_id
                   AND
                   jobs.id = jobs_profiles.job_id
             GROUP BY jobs.id) AS p ON p.job_id = j.id
%s
ORDER BY %s %s
LIMIT $1
OFFSET $2;
`

const selectNodeIdsFromJobsNodesByJobId = `
SELECT
  node_id
FROM jobs_nodes
WHERE job_id = $1
`

const selectResultByJobAndNodeIds = `
SELECT job_id, status, result, start_time, end_time, report_id
FROM results
WHERE job_id=$1 AND node_id=$2
ORDER BY start_time DESC
LIMIT 1;
`

// see https://github.com/chef/automate/issues/3915, https://github.com/chef/a2/pull/3992
// for more information on this query syntax
const sqlGetOWCAScans = `
SELECT
j.id
FROM jobs j
WHERE recurrence = '' AND type = 'exec' AND (end_time >= $1 OR status = 'running');
`

const selectProfileIdsFromJobsProfilesByJobId = `
SELECT
profile_id
FROM jobs_profiles
WHERE job_id = $1
`

const deleteJobNodesByJobId = `
DELETE
FROM jobs_nodes
WHERE job_id = $1
			AND node_id = $2;
`

const deleteJobProfilesByJobId = `
DELETE
FROM jobs_profiles
WHERE job_id = $1
			AND profile_id = $2;
`

// $1 and $2 not working for ORDER BY so ended up using %s + Sprintf, but this requires solid input validation to avoid SQL injection
// Is there a better way to do it?

const deleteJobTags = `
DELETE FROM tags
WHERE id IN (SELECT tag_id
             FROM jobs_tags
             WHERE job_id = $1);
`

const softDeleteJobById = `
UPDATE jobs
SET deleted=TRUE
WHERE id=$1
`

const sqlGetJobName = `
SELECT
name
FROM jobs
WHERE id=$1;
`

const postgresTimeFormat = "2006-01-02T15:04:05"

var jobsSortFields = map[string]string{
	"name":       "LOWER(j.name)",
	"type":       "LOWER(j.type)",
	"status":     "LOWER(j.status)",
	"start_time": "j.start_time",
	"end_time":   "j.end_time",
}

var jobFilterField = map[string]string{
	"deleted":    "deleted",
	"job_type":   "type",
	"name":       "name",
	"parent_job": "parent_id",
	"profile":    "profile",
	"status":     "status",
}

// job used to insert to db
type job struct {
	ID            string          `db:"id"`
	Name          string          `db:"name"`
	Tags          json.RawMessage `db:"-"`
	Nodes         []string        `db:"-"`
	Type          string          `db:"type"`
	Profiles      json.RawMessage `db:"-"`
	Timeout       int32           `db:"timeout"`
	Retries       int32           `db:"retries"`
	RetriesLeft   int32           `db:"retries_left"`
	Status        string          `db:"status"`
	Startime      time.Time       `db:"start_time"`
	Endtime       time.Time       `db:"end_time"`
	NodeSelectors json.RawMessage `db:"node_selectors"`
	ScheduledTime time.Time       `db:"scheduled_time"`
	Recurrence    string          `db:"recurrence"`
	ParentID      string          `db:"parent_id"`
	JobCount      int32           `db:"job_count"`
	NodeCount     sql.NullInt64   `db:"node_count"`
	Deleted       bool            `db:"deleted"`
}

// jobSelectDetail used to read from DB one complete job
type jobSelectDetail struct {
	job
	Tags     json.RawMessage `db:"tags"`
	Nodes    json.RawMessage `db:"nodes"`
	Profiles json.RawMessage `db:"profiles"`
	Results  json.RawMessage `db:"results"`
}

// jobSelectDetail used to read from DB a list of jobs
type jobSelectSummary struct {
	job
	Tags         json.RawMessage `db:"tags"`
	ProfileCount int32           `db:"profile_count"`
	TotalCount   int64           `db:"total_count"`
}

// JobTag used only to (de)serialize database access
type JobTag struct {
	JobID string `db:"job_id"`
	TagID string `db:"tag_id"`
}

// JobNode used only to (de)serialize database access
type JobNode struct {
	JobID  string `db:"job_id"`
	NodeID string `db:"node_id"`
}

type JobProfile struct {
	JobID     string `db:"job_id"`
	ProfileID string `db:"profile_id"`
}

type ResultsRow struct {
	JobID     string    `db:"job_id" json:"job_id,omitempty"`
	NodeID    string    `db:"node_id" json:"node_id"`
	ReportID  string    `db:"report_id" json:"report_id"`
	Status    string    `db:"status" json:"status"`
	Result    string    `db:"result" json:"result"`
	StartTime time.Time `db:"start_time" json:"start_time"`
	EndTime   time.Time `db:"end_time" json:"end_time"`
}

func toDBJob(inJob *jobs.Job) (job, error) {
	newJob := job{}
	newJob.ID = inJob.Id
	newJob.Name = inJob.Name
	jsonTags, err := json.Marshal(inJob.Tags)
	if err != nil {
		return newJob, errors.Wrap(err, "toDBJob unable to marshal tags")
	}
	newJob.Tags = jsonTags
	newJob.Nodes = inJob.Nodes
	newJob.Type = inJob.Type
	jsonProfiles, err := json.Marshal(inJob.Profiles)
	if err != nil {
		return newJob, errors.Wrap(err, "toDBJob unable to marshal profiles")
	}
	newJob.Profiles = jsonProfiles
	newJob.Timeout = inJob.Timeout
	newJob.Retries = inJob.Retries
	newJob.RetriesLeft = inJob.RetriesLeft
	newJob.Status = inJob.Status
	newJobStarttime, err := ptypes.Timestamp(inJob.StartTime)
	if err != nil {
		if inJob.StartTime != nil {
			return newJob, errors.Wrap(err, "toDBJob unable to convert start_time to time.Time")
		}
	} else {
		newJob.Startime = newJobStarttime
	}
	newJobEndtime, err := ptypes.Timestamp(inJob.EndTime)
	if err != nil {
		if inJob.EndTime != nil {
			return newJob, errors.Wrap(err, "toDBJob unable to convert end_time to time.Time")
		}
	} else {
		newJob.Endtime = newJobEndtime
	}
	jsonNodeSelectors, err := json.Marshal(inJob.NodeSelectors)
	if err != nil {
		return newJob, errors.Wrap(err, "toDBJob unable to marshal node selectors")
	}
	newJob.NodeSelectors = jsonNodeSelectors
	newJob.Recurrence = inJob.Recurrence
	newJob.ParentID = inJob.ParentId
	newJob.JobCount = inJob.JobCount

	newJob.NodeCount = sql.NullInt64{Int64: int64(inJob.NodeCount), Valid: true}

	return newJob, nil
}

func fromDBSelectJob(inSelectJob *jobSelectDetail) (jobs.Job, error) {
	jobSelect := jobs.Job{}
	jobSelect.Id = inSelectJob.ID
	jobSelect.Name = inSelectJob.Name
	var tags []*common.Kv
	err := json.Unmarshal(inSelectJob.Tags, &tags)
	if err != nil {
		return jobSelect, errors.Wrap(err, "fromDBSelectJob unable to unmarshal tags")
	}
	jobSelect.Tags = tags
	nodes := make([]map[string]string, 0)
	err = json.Unmarshal(inSelectJob.Nodes, &nodes)
	if err != nil {
		return jobSelect, errors.Wrap(err, "fromDBSelectJob unable to unmarshal nodes")
	}
	newNodes := make([]string, 0)
	for _, item := range nodes {
		newNodes = append(newNodes, item["node_id"])
	}
	jobSelect.Nodes = newNodes
	jobSelect.Type = inSelectJob.Type

	profiles := make([]map[string]string, 0)
	err = json.Unmarshal(inSelectJob.Profiles, &profiles)
	if err != nil {
		return jobSelect, errors.Wrap(err, "fromDBSelectJob unable to unmarshal profiles")
	}
	newProfiles := make([]string, 0)
	for _, item := range profiles {
		newProfiles = append(newProfiles, item["url"])
	}
	jobSelect.Profiles = newProfiles
	jobSelect.Status = inSelectJob.Status
	jobStartTime, err := ptypes.TimestampProto(inSelectJob.Startime)
	if err != nil {
		return jobSelect, errors.Wrap(err, "fromDBSelectJob unable to translate start_time to timestamp")
	}
	jobSelect.StartTime = jobStartTime
	jobEndTime, err := ptypes.TimestampProto(inSelectJob.Endtime)
	if err != nil {
		return jobSelect, errors.Wrap(err, "fromDBSelectJob unable to translate end_time to timestamp")
	}
	jobSelect.EndTime = jobEndTime
	jobSelect.Timeout = inSelectJob.Timeout
	jobSelect.RetriesLeft = inSelectJob.RetriesLeft
	jobSelect.Retries = inSelectJob.Retries

	results := make([]map[string]string, 0)
	err = json.Unmarshal(inSelectJob.Results, &results)
	if err != nil {
		return jobSelect, errors.Wrap(err, "fromDBSelectJob unable to unmarshal results")
	}

	resultsNew := make([]*jobs.ResultsRow, len(results))
	for i, item := range results {
		startTime, err := time.Parse(postgresTimeFormat, item["start_time"])
		if err != nil {
			logrus.Errorf("Error parsing result start_time using format %s: %s", postgresTimeFormat, err.Error())
			continue
		}

		endTime, err := time.Parse(postgresTimeFormat, item["end_time"])
		if err != nil {
			logrus.Errorf("Error parsing result end_time using format %s: %s", postgresTimeFormat, err.Error())
			continue
		}
		resultsNew[i] = &jobs.ResultsRow{
			NodeId:   item["node_id"],
			ReportId: item["report_id"],
			Status:   item["status"],
			Result:   item["result"],
		}

		timestampStartTime, err := ptypes.TimestampProto(startTime)
		if err != nil {
			logrus.Errorf("fromDBSelectJob error: %s", err.Error())
		} else {
			resultsNew[i].StartTime = timestampStartTime
		}
		timestampEndTime, err := ptypes.TimestampProto(endTime)
		if err != nil {
			logrus.Errorf("fromDBSelectJob error: %s", err.Error())
		} else {
			resultsNew[i].EndTime = timestampEndTime
		}
	}
	jobSelect.Results = resultsNew

	var nodeSelectors []*jobs.ManagerFilter
	err = json.Unmarshal(inSelectJob.NodeSelectors, &nodeSelectors)
	if err != nil {
		return jobSelect, errors.Wrap(err, "fromDBSelectJob unable to unmarshal node selectors")
	}
	jobSelect.NodeSelectors = nodeSelectors

	scheduledTimeTimestamp, err := ptypes.TimestampProto(inSelectJob.ScheduledTime)
	if err != nil {
		return jobSelect, errors.Wrap(err, "fromDBSelectJob unable to translate scheduled_time to timestamp")
	}
	jobSelect.ScheduledTime = scheduledTimeTimestamp
	jobSelect.Recurrence = inSelectJob.Recurrence
	jobSelect.ParentId = inSelectJob.ParentID
	jobSelect.JobCount = inSelectJob.JobCount
	if inSelectJob.NodeCount.Valid {
		// I would have imagined coalesce to be enough here, but apparently not :/
		// as I still get this error: sql: Scan error on column index 8: converting driver.Value type <nil> ("<nil>") to a int32: invalid syntax
		// https://github.com/go-gorp/gorp/issues/77
		jobSelect.NodeCount = int32(inSelectJob.NodeCount.Int64)
	}
	jobSelect.Deleted = inSelectJob.Deleted
	return jobSelect, nil
}

func fromDBSelectAllJobs(inSelectJob *jobSelectSummary) (*jobs.Job, error) {
	jobSelect := jobs.Job{}
	jobSelect.Id = inSelectJob.ID
	jobSelect.Name = inSelectJob.Name
	var tags []*common.Kv
	err := json.Unmarshal(inSelectJob.Tags, &tags)
	if err != nil {
		return nil, errors.Wrap(err, "fromDBSelectAllJobs unable to unmarshal tags")
	}
	jobSelect.Tags = tags
	jobSelect.Type = inSelectJob.Type
	jobSelect.Status = inSelectJob.Status
	jobStartTime, err := ptypes.TimestampProto(inSelectJob.Startime)
	if err != nil {
		return nil, errors.Wrap(err, "fromDBSelectAllJobs unable to translate start_time to timestamp")
	}
	jobSelect.StartTime = jobStartTime
	jobEndTime, err := ptypes.TimestampProto(inSelectJob.Endtime)
	if err != nil {
		return nil, errors.Wrap(err, "fromDBSelectAllJobs unable to translate end_time to timestamp")
	}
	jobSelect.EndTime = jobEndTime
	jobSelect.ProfileCount = inSelectJob.ProfileCount
	jobSelect.Recurrence = inSelectJob.Recurrence
	scheduledTimeTimestamp, err := ptypes.TimestampProto(inSelectJob.ScheduledTime)
	if err != nil {
		return nil, errors.Wrap(err, "fromDBSelectAllJobs unable to translate scheduled_time to timestamp")
	}
	jobSelect.ScheduledTime = scheduledTimeTimestamp
	jobSelect.ParentId = inSelectJob.ParentID
	if inSelectJob.NodeCount.Valid {
		// I would have imagined coalesce to be enough here, but apparently not :/
		// as I still get this error: sql: Scan error on column index 8: converting driver.Value type <nil> ("<nil>") to a int32: invalid syntax
		// https://github.com/go-gorp/gorp/issues/77
		jobSelect.NodeCount = int32(inSelectJob.NodeCount.Int64)
	}
	jobSelect.Deleted = inSelectJob.Deleted
	return &jobSelect, nil
}

// given a job id and node id, get the result from the results table
func (db *DB) GetJobResultByNodeId(ctx context.Context, in *jobs.GetJobResultByNodeIdRequest) (*jobs.ResultsRow, error) {
	logrus.Debugf("Picking result for last_job: %s", in.JobId)
	var result *ResultsRow
	err := db.SelectOne(&result, selectResultByJobAndNodeIds, in.JobId, in.NodeId)
	if err != nil {
		if err != sql.ErrNoRows {
			return nil, errors.Wrap(err, "GetJobResultByNodeId unable to handle node last_job result")
		}
		// We don't want to fail if the LastScan is not found. We still return the node, but without LastScan details, like it was never scanned.
		logrus.Errorf("Unable to find result for node %s with job %s", in.NodeId, in.JobId)
	} else {
		// Ok, we have LastScan details to return for the node
		startTime, err := ptypes.TimestampProto(result.StartTime)
		if err != nil {
			return nil, errors.Wrap(err, "GetJobResultByNodeId error translating start_time to timestamp")
		}
		endTime, err := ptypes.TimestampProto(result.EndTime)
		if err != nil {
			return nil, errors.Wrap(err, "GetJobResultByNodeId error translating end_time to timestamp")
		}

		var failedResult string
		if result.Status == "failed" {
			failedResult = result.Result
		}
		return &jobs.ResultsRow{
			NodeId:    in.NodeId,
			ReportId:  result.ReportID,
			JobId:     result.JobID,
			Status:    result.Status,
			Result:    failedResult,
			StartTime: startTime,
			EndTime:   endTime,
		}, nil
	}
	return &jobs.ResultsRow{}, nil
}

func (db *DB) GetJobName(id string) (string, error) {
	var name string
	err := db.QueryRow(sqlGetJobName, id).Scan(&name)
	if err != nil {
		return "", errors.Wrapf(err, "GetNodeName unable to query node id for name %s", id)
	}
	return name, nil
}

func (trans *DBTrans) nodeJob(jobID string, nodeIDs []string) error {
	links := make([]interface{}, 0, len(nodeIDs))

	for _, nodeID := range nodeIDs {
		link := JobNode{
			JobID:  jobID,
			NodeID: nodeID,
		}
		links = append(links, &link)
	}
	return trans.Insert(links...)
}

func (db *DB) AddJob(inJob *jobs.Job) (string, error) {
	if err := validateJob(inJob); err != nil {
		return "", errors.Wrap(err, "AddJob unable to validate job")
	}

	job, err := toDBJob(inJob)
	if err != nil {
		return "", errors.Wrap(err, "AddJob unable to translate job to db struct")
	}

	job.ID = uuid.Must(uuid.NewV4()).String()

	err = Transact(db, func(tx *DBTrans) error {
		err = tx.Insert(&job)
		if err != nil {
			return errors.Wrap(err, "AddJob unable to insert job in db")
		}

		tags, err := tx.addTags(inJob.Tags)
		if err != nil {
			return errors.Wrap(err, "AddJob unable to add job tags")
		}

		err = tx.tagJob(job.ID, tags)
		if err != nil {
			return errors.Wrap(err, "AddJob unable to tag job")
		}

		err = tx.nodeJob(job.ID, inJob.Nodes)
		if err != nil {
			return errors.Wrap(err, "AddJob unable to associate job with nodes")
		}

		err = tx.addProfilesToJob(job.ID, inJob.Profiles)
		if err != nil {
			return errors.Wrap(err, "AddJob")
		}

		return nil
	})
	return job.ID, err
}

func isValidJobType(jobType string) bool {
	switch jobType {
	case "exec":
		return true
	case "detect":
		return true
	default:
		return false
	}
}

func isValidJobStatus(status string) bool {
	switch status {
	case "completed", "failed", "new", "running", "scheduled":
		return true
	default:
		return false
	}
}

func validateJobFilters(filters []*common.Filter) error {
	for _, filter := range filters {
		switch filter.Key {
		case "job_type":
			for _, item := range filter.Values {
				if !isValidJobType(item) {
					return &errorutils.InvalidError{Msg: fmt.Sprintf("Invalid job_type filter: %s. job_type must be one of the following: 'detect' or 'exec'", item)}
				}
			}
		case "parent_job":
			for _, item := range filter.Values {
				if item != "" {
					if !utils.IsSafeUUID(item) {
						return &errorutils.InvalidError{Msg: fmt.Sprintf("Invalid parent_job uuid filter: %s", item)}
					}
				}
			}
		case "status":
			for _, item := range filter.Values {
				if item != "" {
					if !isValidJobStatus(item) {
						return &errorutils.InvalidError{Msg: fmt.Sprintf("Invalid status filter: %s. status must be one of the following: 'completed', 'failed', 'new', 'running', 'scheduled'", item)}
					}
				}
			}
		}
	}
	return nil
}

func (db *DB) GetJobs(sortField string, insortOrder jobs.Query_OrderType, pageNr int32, perPage int32, filters []*common.Filter) ([]*jobs.Job, int64, error) {
	var sortOrder string
	sortField = valueOrDefaultStr(sortField, "name")
	if insortOrder == 1 {
		sortOrder = "desc"
	} else {
		sortOrder = "asc"
	}
	pageNr = valueOrDefaultInt(pageNr, 1) - 1
	perPage = valueOrDefaultInt(perPage, 100)

	if jobsSortFields[sortField] == "" {
		return nil, 0, &errorutils.InvalidError{Msg: fmt.Sprintf("Invalid sort field, valid ones are: %v", getMapKeys(jobsSortFields))}
	}
	if !stringutils.SliceContains(validOrderFields, sortOrder) {
		return nil, 0, &errorutils.InvalidError{Msg: fmt.Sprintf("Invalid order, valid ones are: %v", validOrderFields)}
	}
	var jobDaos []*jobSelectSummary

	filters = append(filters, &common.Filter{Key: "deleted", Values: []string{"f"}})
	err := validateJobFilters(filters)
	if err != nil {
		return nil, 0, errors.Wrapf(err, "GetJobs error validating jobs filters")
	}
	whereFilter, err := buildWhereFilter(filters, "j", jobFilterField)
	if err != nil {
		return nil, 0, errors.Wrap(err, "GetJobs error building where filter")
	}

	query := fmt.Sprintf(selectJobs, whereFilter, jobsSortFields[sortField], sortOrder)
	logrus.Debugf("SQL: %s %d %d", query, perPage, pageNr*perPage)
	_, err = db.Select(&jobDaos, query, perPage, pageNr*perPage)
	if err != nil {
		return nil, 0, errors.Wrapf(err, "GetJobs error selecting jobs with filters:\n%+v\nquery:\n%s", filters, query)
	}

	totalCount := int64(0)
	if len(jobDaos) > 0 {
		totalCount = jobDaos[0].TotalCount
	}

	jobs := make([]*jobs.Job, 0)
	for _, jobDao := range jobDaos {
		job, err := fromDBSelectAllJobs(jobDao)
		if err != nil {
			return jobs, totalCount, errors.Wrap(err, "GetJobs error translating all jobs from db structs")
		}
		jobs = append(jobs, job)
	}

	return jobs, totalCount, nil
}

func (db *DB) ListInitiatedScans(ctx context.Context, startTime *tspb.Timestamp) ([]string, error) {
	var sqlIds []string
	translatedTime, err := ptypes.Timestamp(startTime)
	if err != nil {
		return sqlIds, errors.Wrap(err, "unable to translate start time in request")
	}
	_, err = db.Select(&sqlIds, sqlGetOWCAScans, translatedTime)
	if err != nil {
		return sqlIds, err
	}
	return sqlIds, nil
}

func (db *DB) GetJob(id string) (*jobs.Job, error) {
	var job jobSelectDetail
	var newJob jobs.Job

	err := db.SelectOne(&job, selectJob, id)
	if err != nil {
		return &newJob, errorutils.ProcessSQLNotFound(err, id, "GetJob error")
	}

	newJob, err = fromDBSelectJob(&job)
	if err != nil {
		return &newJob, errors.Wrap(err, "GetJob error translating job from db struct")
	}
	return &newJob, nil
}

func (db *DB) DeleteJob(id string) error {
	_, err := db.Exec(softDeleteJobById, id)
	if err != nil {
		return errors.Wrap(err, "DeleteJob unable to delete job")
	}

	return nil
}

func (db *DB) UpdateJob(inJob *jobs.Job) error {
	if err := validateJob(inJob); err != nil {
		return errorutils.ProcessInvalid(err, "UpdateJob error validating job")
	}

	job, err := toDBJob(inJob)
	if err != nil {
		return errors.Wrap(err, "Update Job unable to translate job to db struct")
	}

	err = Transact(db, func(tx *DBTrans) error {
		err = tx.processJobsTagsUpdate(inJob)
		if err != nil {
			return errors.Wrap(err, "Update Job unable to process jobs tags update")
		}

		err = tx.processJobsNodesUpdate(inJob)
		if err != nil {
			return errors.Wrap(err, "Update Job unable to process jobs nodes update")
		}

		err = tx.processJobsProfilesUpdate(inJob)
		if err != nil {
			return errors.Wrap(err, "Update Job unable to process jobs profiles update")
		}

		_, err = tx.Update(&job)
		if err != nil {
			return errors.Wrap(err, "Update Job unable to update job")
		}

		return nil
	})

	return err
}

func (trans *DBTrans) processJobsTagsUpdate(inJob *jobs.Job) error {
	//get list of tags from current job
	_, err := trans.Exec(deleteJobTags, inJob.Id)
	if err != nil {
		return errors.Wrap(err, "processJobsTagsUpdate unable to delete jobs tags")
	}

	tags, err := trans.addTags(inJob.Tags)
	if err != nil {
		return errors.Wrap(err, "processJobsTagsUpdate unable to add job tags")
	}

	err = trans.tagJob(inJob.Id, tags)
	if err != nil {
		return errors.Wrap(err, "processJobsTagsUpdate unable to tag job")
	}
	return nil

}
func (trans *DBTrans) processJobsProfilesUpdate(inJob *jobs.Job) error {
	var profileIDsByJobID []string

	_, err := trans.Select(&profileIDsByJobID, selectProfileIdsFromJobsProfilesByJobId, inJob.Id)
	if err != nil {
		return errors.Wrap(err, "processJobsProfilesUpdate unable to select profile ids")
	}

	profilesToAdd := utils.DiffArray(inJob.Profiles, profileIDsByJobID)
	jobProfilesToAdd := make([]interface{}, 0)
	for _, profile := range profilesToAdd {
		jobProfilesToAdd = append(jobProfilesToAdd, &JobProfile{JobID: inJob.Id, ProfileID: profile})
	}

	profilesToDelete := utils.DiffArray(profileIDsByJobID, inJob.Profiles)
	jobProfilesToDelete := make([]*JobProfile, 0)
	for _, profile := range profilesToDelete {
		jobProfilesToDelete = append(jobProfilesToDelete, &JobProfile{JobID: inJob.Id, ProfileID: profile})
	}

	if len(profilesToDelete) > 0 {
		for _, jobProfileToDelete := range jobProfilesToDelete {
			_, err := trans.Exec(deleteJobProfilesByJobId, inJob.Id, jobProfileToDelete.ProfileID)
			if err != nil {
				return errors.Wrap(err, "processJobsProfilesUpdate unable to delete profiles")
			}
		}
	}

	err = trans.addProfilesToJob(inJob.Id, profilesToAdd)
	if err != nil {
		return errors.Wrap(err, "processJobsProfilesUpdate")
	}

	return nil
}

func (trans *DBTrans) processJobsNodesUpdate(inJob *jobs.Job) error {
	var nodeIDsByJobID []string

	_, err := trans.Select(&nodeIDsByJobID, selectNodeIdsFromJobsNodesByJobId, inJob.Id)
	if err != nil {
		return errors.Wrap(err, "processJobsNodesUpdate unable to select nodes for job")
	}

	nodesToAdd := utils.DiffArray(inJob.Nodes, nodeIDsByJobID)
	jobNodesToAdd := make([]interface{}, 0)
	for _, node := range nodesToAdd {
		jobNodesToAdd = append(jobNodesToAdd, &JobNode{JobID: inJob.Id, NodeID: node})
	}

	nodesToDelete := utils.DiffArray(nodeIDsByJobID, inJob.Nodes)
	jobNodesToDelete := make([]*JobNode, 0)
	for _, node := range nodesToDelete {
		jobNodesToDelete = append(jobNodesToDelete, &JobNode{JobID: inJob.Id, NodeID: node})
	}

	if len(nodesToDelete) > 0 {
		for _, jobNodeToDelete := range jobNodesToDelete {
			_, err = trans.Exec(deleteJobNodesByJobId, inJob.Id, jobNodeToDelete.NodeID)
			if err != nil {
				return errors.Wrap(err, "processJobsNodesUpdate unable to delete nodes for job")
			}
		}
	}

	if len(nodesToAdd) > 0 {
		err = trans.Insert(jobNodesToAdd...)
		if err != nil {
			return errors.Wrap(err, "processJobsNodesUpdate unable to associates nodes with job")
		}
	}
	return nil
}

func validateJob(in *jobs.Job) error {
	if in.Timeout == 0 {
		if in.Type == types.JobTypeDetect {
			in.Timeout = 600
		} else {
			in.Timeout = 7200
		}
	}

	in.RetriesLeft = in.Retries

	if in.Name == "" {
		return &errorutils.InvalidError{Msg: "Invalid job, 'name' is a required parameter"}
	}
	in.Status = types.StatusNew

	return nil
}
