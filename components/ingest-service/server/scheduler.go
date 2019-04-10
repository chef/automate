package server

import (
	"time"

	"github.com/chef/automate/api/interservice/ingest"
	"github.com/robfig/cron"
	log "github.com/sirupsen/logrus"
)

const (
	defaultJobPollTime = "1m"
)

type scheduleJob struct {
	every       string
	lastRun     time.Time
	startedOn   time.Time
	lastElapsed time.Duration
	active      bool
	threshold   string
	running     bool
	cmd         func()
}

func (job *scheduleJob) updateLastRun() {
	job.lastRun = time.Now()
}

func (job *scheduleJob) restarting() {
	job.startedOn = time.Now()
	job.running = true
}

func (job *scheduleJob) stop() {
	if job.running {
		job.startedOn = time.Time{}
		job.running = false
	}
}

func (job *scheduleJob) getNextRunTime() (time.Time, error) {
	if job.running {
		duration, err := time.ParseDuration(job.every)
		if err != nil {
			return time.Time{}, err
		}
		if job.lastRun.IsZero() || job.startedOn.After(job.lastRun) {
			return job.startedOn.Add(duration), nil
		}
		return job.lastRun.Add(duration), nil
	}

	return time.Time{}, nil
}

func getTimeString(dateTime time.Time) string {
	if dateTime.IsZero() {
		return ""
	}
	return dateTime.Format(time.RFC3339Nano)
}

func getDurationString(duration time.Duration) string {
	if duration == 0 {
		return ""
	}
	return duration.String()
}

type scheduler struct {
	cron    *cron.Cron
	jobs    map[string]*scheduleJob
	running bool
}

func (scheduler *scheduler) reconfigure() {
	if scheduler.running {
		scheduler.cron.Stop()
		scheduler.cron = cron.New()

		for _, job := range scheduler.jobs {
			if job.active {
				scheduler.cron.AddFunc("@every "+job.every, job.cmd)
				job.restarting()
			} else {
				job.stop()
			}
		}
		scheduler.cron.Start()
	} else {
		scheduler.cron.Stop()
		for _, job := range scheduler.jobs {
			job.stop()
		}
	}
}

type queueItem struct {
	updater func(*scheduler) *scheduler
	done    chan<- struct{}
}

// JobScheduler - this manages jobs that run on a periodic schedule (daily, hourly, or every minute.)
//
// Multiple goroutines can use this object. Data updates are performed asynchronously, where updates
// are pushed as a function on to the updateQueue channel.
// This is patterned after Akka's Agents (https://doc.akka.io/docs/akka/2.5.6/java/agents.html)
type JobScheduler struct {
	scheduler   *scheduler
	updateQueue chan<- queueItem
}

// NewJobScheduler creates a new JobScheduler and starts it in the background
func NewJobScheduler() *JobScheduler {
	updateQueue := make(chan queueItem, 100)
	js := &JobScheduler{
		scheduler: &scheduler{
			cron:    cron.New(),
			jobs:    make(map[string]*scheduleJob),
			running: false,
		},
		updateQueue: updateQueue,
	}

	go func() {
		for queueItem := range updateQueue {
			scheduler := queueItem.updater(js.scheduler)

			js.scheduler = scheduler
			if queueItem.done != nil {
				queueItem.done <- struct{}{}
			}
		}
	}()

	log.WithFields(log.Fields{
		"queue_size": 100,
	}).Info("Starting JobScheduler")

	js.Start()

	return js
}

// GetJobsStatus - collection information about each job running.
func (jobScheduler *JobScheduler) GetJobsStatus() ([]*ingest.Job, error) {
	jobs := make([]*ingest.Job, len(jobScheduler.scheduler.jobs))

	index := 0
	for name, job := range jobScheduler.scheduler.jobs {
		var iJob ingest.Job
		nextRun, err := job.getNextRunTime()
		if err != nil {
			return jobs, err
		}

		iJob = ingest.Job{
			Running:     job.running,
			Name:        name,
			Every:       job.every,
			Threshold:   job.threshold,
			LastRun:     getTimeString(job.lastRun),
			NextRun:     getTimeString(nextRun),
			LastElapsed: getDurationString(job.lastElapsed),
			StartedOn:   getTimeString(job.startedOn),
		}

		jobs[index] = &iJob
		index++
	}

	return jobs, nil
}

// Close - to close out the channel for this object.
// This should only be called when the service is being shutdown
func (jobScheduler *JobScheduler) Close() {
	jobScheduler.Stop()
	updateFunc := func(scheduler *scheduler) *scheduler {
		close(jobScheduler.updateQueue)
		return scheduler
	}
	jobScheduler.send(updateFunc)

}

// Start - starts the scheduler and all running jobs
func (jobScheduler *JobScheduler) Start() {
	updateFunc := func(scheduler *scheduler) *scheduler {
		if !scheduler.running {
			scheduler.running = true
			scheduler.reconfigure()
		}
		return scheduler
	}
	jobScheduler.send(updateFunc)
}

// Stop - stops the scheduler and all jobs
func (jobScheduler *JobScheduler) Stop() {
	updateFunc := func(scheduler *scheduler) *scheduler {
		if scheduler.running {
			scheduler.running = false
			scheduler.reconfigure()
		}
		return scheduler
	}
	jobScheduler.send(updateFunc)
}

// StartJob - start a job
func (jobScheduler *JobScheduler) StartJob(jobName string) {
	updateFunc := func(scheduler *scheduler) *scheduler {
		log.WithFields(log.Fields{
			"job_name": jobName,
		}).Info("Start Job")

		job, ok := scheduler.jobs[jobName]

		if ok {
			if !job.active {
				job.active = true
				scheduler.reconfigure()
			}
		} else {
			log.WithFields(log.Fields{
				"job_name": jobName,
			}).Error("Job Not Found")
		}

		return scheduler
	}
	jobScheduler.send(updateFunc)
}

// StopJob - stop a job
func (jobScheduler *JobScheduler) StopJob(jobName string) {
	updateFunc := func(scheduler *scheduler) *scheduler {
		log.WithFields(log.Fields{
			"job_name": jobName,
		}).Info("Stop Job")

		job, ok := scheduler.jobs[jobName]

		if ok {
			if job.active {
				job.active = false
				scheduler.reconfigure()
			}
		} else {
			log.WithFields(log.Fields{
				"job_name": jobName,
			}).Error("Job Not Found")
		}

		return scheduler
	}
	jobScheduler.send(updateFunc)
}

// AddUpdateJob - add or update a job to the scheduler
func (jobScheduler *JobScheduler) AddUpdateJob(jobName, threshold, every string, active bool, cmd func()) {
	if every == "" {
		every = defaultJobPollTime
	}

	updateFunc := func(scheduler *scheduler) *scheduler {
		log.WithFields(log.Fields{
			"every":    every,
			"job_name": jobName,
			"active":   active,
		}).Info("Job Added")

		job, ok := scheduler.jobs[jobName]

		if ok {
			job.active = active
			job.every = every
			job.threshold = threshold
		} else {
			job = &scheduleJob{
				active:    active,
				every:     every,
				threshold: threshold,
			}
		}

		job.cmd = func() {
			start := time.Now()
			cmd()
			end := time.Now()
			job.lastElapsed = end.Sub(start)

			job.updateLastRun()
		}

		scheduler.jobs[jobName] = job

		// Run the job when it is updated
		if active {
			job.cmd()
		}

		scheduler.reconfigure()
		return scheduler
	}

	jobScheduler.sendAndWait(updateFunc)
}

func (jobScheduler *JobScheduler) sendAndWait(updateFunc func(*scheduler) *scheduler) {
	done := make(chan struct{})
	jobScheduler.updateQueue <- queueItem{updater: updateFunc, done: done}

	// do not return until the job is updated or created.
	<-done

	close(done)
}

func (jobScheduler *JobScheduler) send(updateFunc func(*scheduler) *scheduler) {
	jobScheduler.updateQueue <- queueItem{updateFunc, nil}
}
