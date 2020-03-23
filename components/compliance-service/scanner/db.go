package scanner

/* All the code in this file is db interactions made by the inspec agent.
You will notice many of the calls in this file simply log the db error;
that is because the inspec-agent's primary job is to take job details,
run an inspec exec on the appropriate nodes, and send the report to ingest.
DB interactions are secondary, and should not block a job from being executed/reporting.
*/
import (
	"context"
	"fmt"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/golang/protobuf/ptypes/timestamp"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"

	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/compliance/jobs"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
)

type Scanner struct {
	managerClient manager.NodeManagerServiceClient
	nodesClient   nodes.NodesServiceClient
	DB            *pgdb.DB
}

func New(managerClient manager.NodeManagerServiceClient, nodesClient nodes.NodesServiceClient, db *pgdb.DB) *Scanner {
	return &Scanner{managerClient, nodesClient, db}
}

const sqlUpdateJobStatus = `
UPDATE jobs
SET status = $1, start_time = COALESCE($2, start_time), end_time = COALESCE($3, end_time)
WHERE id = $4;
`
const sqlUpdateJobNodeCount = `
UPDATE jobs
SET node_count = $1
WHERE id = $2;
`
const sqlUpdateJobStatusOnly = `
UPDATE jobs
SET status = $1
WHERE id = $2;
`
const sqlUpdateJobStatusAndCount = `
UPDATE jobs
SET status = $1, job_count = $2
WHERE id = $3 AND recurrence != '';
`
const sqlUpdateJobScheduledDatetimeAndIncreaseCount = `
UPDATE jobs
SET scheduled_time = $1, status = $2, job_count = $3, recurrence = $4
WHERE id = $5;
`

const sqlGetReadyJobsIds = `
SELECT
j.id
FROM jobs j
WHERE status = $1 AND parent_id = '' AND recurrence != '' AND scheduled_time < $2 AND deleted=FALSE;
`

const sqlGetNodeStatus = `
SELECT
j.status
FROM jobs j
WHERE id = $1;
`

const sqlGetJobDeletedVal = `
SELECT
j.deleted
FROM jobs j
WHERE id=$1;
`

func (s *Scanner) IsJobDeleted(jobID string) (bool, error) {
	var deleted bool // default val of a boolean is false
	err := s.DB.QueryRow(sqlGetJobDeletedVal, jobID).Scan(&deleted)
	if err != nil {
		return deleted, err
	}
	return deleted, nil
}

func isStatusUpdateValid(oldStatus, newStatus string) bool {
	if oldStatus == "running" && newStatus == "scheduled" {
		return false
	}
	if oldStatus == "completed" || oldStatus == "failed" {
		if newStatus == "running" || newStatus == "scheduled" {
			return false
		}
	}
	return true
}

func (s *Scanner) UpdateJobStatus(job_id string, newStatus string, startTime *time.Time, endTime *time.Time) error {
	// get current status of job
	var status string
	err := s.DB.QueryRow(sqlGetNodeStatus, job_id).Scan(&status)
	if err != nil {
		return errors.Wrapf(err, "UpdateJobStatus unable to get current job status")
	}

	// prevent the inspec-agent from updating a job status "backwards"
	// a job may go new -> scheduled -> running -> completed/failed. it may never go backwards.
	if !isStatusUpdateValid(status, newStatus) {
		return errors.Wrapf(err, "attempt to update job with current status %s to status %s has been blocked", status, newStatus)
	}

	logrus.Debugf("UpdateJobStatus job %s with status %s, start_time %v, end_time %v", job_id, newStatus, startTime, endTime)
	_, err = s.DB.Exec(sqlUpdateJobStatus, newStatus, startTime, endTime, job_id)
	if err != nil {
		return errors.Wrapf(err, "UpdateJobStatus DB error")
	}
	return nil
}

func (s *Scanner) UpdateJobNodeCount(jobID string, nodeCount int) error {
	logrus.Debugf("Update job %s with node count %d", jobID, nodeCount)
	_, err := s.DB.Exec(sqlUpdateJobNodeCount, nodeCount, jobID)
	if err != nil {
		return errors.Wrapf(err, "UpdateJobNodeCount DB error")
	}
	return nil
}

func (s *Scanner) GetDueJobs(nowTime time.Time) ([]*jobs.Job, error) {
	dueJobs := make([]*jobs.Job, 0)
	var id string
	var sqlIds []string
	rows, err := s.DB.Query(sqlGetReadyJobsIds, "scheduled", nowTime)
	if err != nil {
		return dueJobs, errors.Wrapf(err, "GetDueJobs unable to query for job ids")
	}
	defer rows.Close() // nolint: errcheck
	for rows.Next() {
		err := rows.Scan(&id)
		if err != nil {
			logrus.Error(err)
			continue
		}
		sqlIds = append(sqlIds, id)
	}
	err = rows.Err()
	if err != nil {
		return dueJobs, errors.Wrapf(err, "GetDueJobs db error")
	}

	if len(sqlIds) > 0 {
		logrus.Debugf("GetDueJobs, these are the ready jobs job_ids %v", sqlIds)
	}
	// get each job, update parent job
	for _, id := range sqlIds {
		job, err := s.DB.GetJob(id)
		if err != nil {
			logrus.Errorf("GetDueJobs, unable to retrieve job for job_id %s", id)
			continue
		}
		dueJobs = append(dueJobs, job)
	}
	return dueJobs, nil
}

func (s *Scanner) UpdateNode(ctx context.Context, job *types.InspecJob, detectInfo *inspec.OSInfo) error {
	if job.NodeID == "" {
		return fmt.Errorf("no node id included in node update for node %s ", job.NodeName)
	}
	var err error
	if detectInfo == nil {
		detectInfo = &inspec.OSInfo{}
	}
	endTimeTimestamp, err := ptypes.TimestampProto(*job.EndTime)
	if err != nil {
		logrus.Errorf("UpdateNode unable to parse job end time")
		endTimeTimestamp = ptypes.TimestampNow()
	}
	logrus.Debugf("UpdateNode %s with status %s", job.NodeID, job.NodeStatus)
	_, err = s.nodesClient.UpdateNodeDetectInfo(ctx, &nodes.NodeDetectJobInfo{
		NodeId:          job.NodeID,
		PlatformName:    detectInfo.OSName,
		PlatformRelease: detectInfo.OSRelease,
		NodeStatus:      job.NodeStatus,
		JobEndTime:      endTimeTimestamp,
		JobId:           job.JobID,
		JobType:         job.JobType,
		NodeName:        job.NodeName,
	})
	if err != nil {
		return errors.Wrapf(err, "UpdateNode unable to update detect info for node %s", job.NodeName)
	}
	return nil
}

func (s *Scanner) UpdateResult(ctx context.Context, job *types.InspecJob, output []byte, inspecErr *inspec.Error, reportID string) error {
	result := pgdb.ResultsRow{
		JobID:     job.JobID,
		NodeID:    job.NodeID,
		ReportID:  reportID,
		Status:    job.NodeStatus,
		StartTime: *job.StartTime,
		EndTime:   *job.EndTime,
	}

	if job.NodeStatus == types.StatusFailed || job.NodeStatus == types.StatusAborted {
		if inspecErr != nil {
			result.Result = inspecErr.Message
		} else {
			result.Result = fmt.Sprintf("unable to complete scan on node. please check the logs for more information. node id ref: %s", job.NodeID)
		}
		var connectionErr, errMsg string
		if inspecErr != nil {
			if len(inspecErr.Message) > 300 {
				errMsg = fmt.Sprintf("%s [truncated for length]", inspecErr.Message[0:300])
			} else {
				errMsg = inspecErr.Message
			}
			connectionErr = fmt.Sprintf("%s\n\n%s", inspecErr.Type, errMsg)
		}
		if connectionErr != "" {
			_, err := s.nodesClient.UpdateNodeConnectionError(ctx, &nodes.NodeError{
				NodeId:          job.NodeID,
				ConnectionError: connectionErr,
			})
			if err != nil {
				return errors.Wrapf(err, "UpdateResult encountered an error updating node connection error")
			}
		}
	} else {
		result.Result = string(output)
	}
	err := s.DB.Insert(&result)
	if err != nil {
		return errors.Wrap(err, "UpdateResult encountered an error inserting into the database")
	}

	return nil
}

func (s *Scanner) GetNodeUUID(ctx context.Context, sourceID string, region string, acctID string) (string, error) {
	logrus.Debugf("Looking for node with id '%s' in '%s' region in DB", sourceID, region)
	nodes, err := s.nodesClient.List(ctx, &nodes.Query{
		Page:    1,
		PerPage: 100000,
		Filters: []*common.Filter{
			{Key: "region", Values: []string{region}},
			{Key: "source_id", Values: []string{sourceID}},
			{Key: "account_id", Values: []string{acctID}}}})
	if err != nil {
		return "", err
	}
	if len(nodes.Nodes) > 0 {
		if len(nodes.Nodes) == 1 {
			return nodes.Nodes[0].Id, nil
		} else {
			return "", fmt.Errorf("unexpected error: got more than one result when searching for node")
		}
	}
	return "", nil
}

func (s *Scanner) createDbJob(inJob *jobs.Job) (string, error) {
	return s.DB.AddJob(inJob)
}

// UpdateParentJobSchedule updates recurrent(parent) jobs fields:
// status: "new" -> "scheduled" -> "completed" if no more runs are required based on the recurrent fields and job count
// job_count: how many child jobs have been created for this recurrent job
// scheduled_time: when is the next run due
func (s *Scanner) UpdateParentJobSchedule(jobId string, jobCount int32, recurrence string, lastScheduledTime *timestamp.Timestamp) error {
	logrus.Debugf("UpdateParentJobSchedule started for jobId=%s, jobCount=%d, recurrence=%s, lastScheduledTime=%s", jobId, jobCount, recurrence, lastScheduledTime)
	// let's open up a DB transaction
	trans := &pgdb.DBTrans{}
	transaction, err := s.DB.Begin()
	if err != nil {
		return errors.Wrapf(err, "UpdateParentJobSchedule db error")
	}
	trans.Transaction = transaction
	// Parsing the recurrence RRULE
	r, err := rrule.StrToRRule(recurrence)
	if err != nil {
		return errors.Wrapf(err, "UpdateParentJobSchedule unable to parse recurrence string")
	}

	nowTimeUtc := time.Now().UTC()
	// We've never run the job before and we ensure that the recurrence rule has a DTSTART
	// This is important in order to get correct future scheduled times
	if lastScheduledTime == nil {
		logrus.Debugf("lastScheduledTime is nil for jobId=%s", jobId)
		if r.OrigOptions.Dtstart.IsZero() {
			logrus.Debugf("lastScheduledTime is nil for jobId=%s, setting DTSTART to now()", jobId)
			// Setting a recurrence DTSTART if one is not provided
			r.OrigOptions.Dtstart = nowTimeUtc
			// Setting the variable in order to store recurrence in the database with DTSTART
			recurrence = r.String()
		}
	}
	// take five seconds off of now time to ensure a job scheduled for now with a count of 1 actually gets run
	fiveSecAgo := nowTimeUtc.Add(-5 * time.Second)
	// Asking rrule for the next time to run this job in the future/now
	utcScheduledTime := r.After(fiveSecAgo, true).UTC()

	logrus.Debugf("utcScheduledTime = %s", utcScheduledTime)

	// Recurrence count(r.OrigOptions.Count) needs to be > 0 in order to take into account the jobCount of the job. If we reached we recurrence count, we complete the job
	// utcScheduledTime will be zero if no future runs are needed
	// (r.OrigOptions.Interval == 0 && jobCount == 1) <- when no recurrence frequency is specified, rrule defaults to
	// yearly recurrence. if the user did not specify interval, we know they did not specify frequency. in this case,
	// the user only wanted to run this one job in the future, so if job count is 1 and interval is 0, job is complete
	if r.OrigOptions.Count > 0 && jobCount >= int32(r.OrigOptions.Count) || utcScheduledTime.IsZero() || (r.OrigOptions.Interval == 0 && jobCount == 1) {
		logrus.Debugf("jobCount for jobId=%s, if jobCount=%d, int32(r.OrigOptions.Count=%d", jobId, jobCount, int32(r.OrigOptions.Count))
		// update the status to completed, b/c we have reached the count limit for jobs
		_, err = trans.Exec(sqlUpdateJobStatusAndCount, "completed", jobCount, jobId)
		if err != nil {
			return errors.Wrapf(err, "UpdateParentJobSchedule unable to update job status and count")
		}
	} else {
		logrus.Debugf("jobCount for jobId=%s, else jobCount=%d", jobId, jobCount)
		// update the status to scheduled, update scheduled_time, update jobCount
		_, err = trans.Exec(sqlUpdateJobScheduledDatetimeAndIncreaseCount, utcScheduledTime, "scheduled", jobCount, recurrence, jobId)
		if err != nil {
			return errors.Wrapf(err, "UpdateParentJobSchedule unable to update job schedule")
		}
	}
	err = trans.Commit()
	if err != nil {
		return errors.Wrapf(err, "UpdateParentJobSchedule db error")
	}
	return nil
}

func (s *Scanner) CreateChildJob(job *jobs.Job) (*jobs.Job, error) {
	// create a child job with parent job id association
	childJob := job
	job.Name = fmt.Sprintf("%s - run %d", job.Name, job.JobCount)
	childJob.Recurrence = ""
	childJob.ParentId = job.Id
	childJob.JobCount = 0

	id, err := s.createDbJob(childJob)
	if err != nil {
		return nil, err
	}
	_, err = s.DB.Exec(sqlUpdateJobStatusOnly, "scheduled", id)
	if err != nil {
		return nil, errors.Wrapf(err, "CreateChildJob, unable to set job status to scheduled, %s", job.Id)
	}
	childJob.Id = id

	return childJob, nil
}
