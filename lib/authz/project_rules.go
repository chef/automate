package authz

const (
	// ProjectUpdateIDTag - The tag used to designate the project update job
	ProjectUpdateIDTag = "ProjectUpdateID"
)

// JobStatus - the current status of a job
type JobStatus struct {
	Completed             bool
	PercentageComplete    float32
	EstimatedEndTimeInSec int64
}

// FindSlowestJobStatus - merge multiple job statuses into one.
// Return the status that is going to finish last.
func FindSlowestJobStatus(jobStatuses []JobStatus) JobStatus {
	combinedJobStatus := JobStatus{
		Completed:          true,
		PercentageComplete: 1.0,
	}
	for _, jobStatus := range jobStatuses {
		if !jobStatus.Completed {
			combinedJobStatus.Completed = false
			if jobStatus.EstimatedEndTimeInSec > combinedJobStatus.EstimatedEndTimeInSec {
				combinedJobStatus.EstimatedEndTimeInSec = jobStatus.EstimatedEndTimeInSec
				combinedJobStatus.PercentageComplete = jobStatus.PercentageComplete
			}
		}
	}

	return combinedJobStatus
}
