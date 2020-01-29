package generator

import (
	"fmt"
	"strings"
	"time"

	"go.uber.org/atomic"
)

type RunnerStats struct {
	messagesSentSuccess atomic.Uint64
	messagesSentFailed  atomic.Uint64
	collectionStart     time.Time
}

func (r *RunnerStats) TotalMessages() uint64 {
	return r.messagesSentSuccess.Load() + r.messagesSentFailed.Load()
}

func (r *RunnerStats) TotalRate() float64 {
	return float64(r.TotalMessages()) / time.Since(r.collectionStart).Seconds()
}

func NewStatsKeeper() *RunnerStatsKeeper {
	now := time.Now()

	return &RunnerStatsKeeper{
		lifetimeStats:  &RunnerStats{collectionStart: now},
		lastCycleStats: &RunnerStats{collectionStart: now},
	}
}

type RunnerStatsKeeper struct {
	supervisorsRunning atomic.Uint64
	supervisorsFailed  atomic.Uint64
	lifetimeStats      *RunnerStats
	lastCycleStats     *RunnerStats
}

const statsPrintPerMinute = 2

func (r *RunnerStatsKeeper) RunCollectAndPrintLoop() {
	r.Report()
	ticker := time.NewTicker(r.Interval())
	for range ticker.C {
		r.Report()
		r.ResetCycleStats()
	}
}

func (r *RunnerStatsKeeper) Interval() time.Duration {
	return time.Minute / statsPrintPerMinute
}

func (r *RunnerStatsKeeper) SupStarted() {
	r.supervisorsRunning.Inc()
}

func (r *RunnerStatsKeeper) SupDied() {
	r.supervisorsRunning.Dec()
	r.supervisorsFailed.Inc()
}

func (r *RunnerStatsKeeper) SuccessfulPublish() {
	r.lifetimeStats.messagesSentSuccess.Inc()
	r.lastCycleStats.messagesSentSuccess.Inc()
}

func (r *RunnerStatsKeeper) FailedPublish() {
	r.lifetimeStats.messagesSentFailed.Inc()
	r.lastCycleStats.messagesSentFailed.Inc()
}

func (r *RunnerStatsKeeper) ResetCycleStats() {
	r.lastCycleStats = &RunnerStats{collectionStart: time.Now()}
}

func (r *RunnerStatsKeeper) Report() {
	heading := fmt.Sprintf("Load Generator Statistics %s", time.Now().Format(time.RFC3339))
	hr := strings.Repeat("-", len(heading))

	intervalSeconds := int(r.Interval().Round(time.Second).Seconds())

	fmt.Printf(`
%s
%s
Supervisors Running:          %d
Supervisors Died:             %d
Messages Sent    (lifetime):  %d
Message Rate /s  (lifetime):  %.2f 
Messages Sent    (last %2ds):  %d
Message Rate /s  (last %2ds):  %.2f
`,
		heading,
		hr,
		r.supervisorsRunning.Load(),
		r.supervisorsFailed.Load(),
		r.lifetimeStats.TotalMessages(),
		r.lifetimeStats.TotalRate(),
		intervalSeconds,
		r.lastCycleStats.TotalMessages(),
		intervalSeconds,
		r.lastCycleStats.TotalRate(),
	)

}
