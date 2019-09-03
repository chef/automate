package cereal

import (
	"context"
	"math/rand"
	"sync"
	"testing"
	"time"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/lib/cereal/backend"
)

var errTestDequeueNoTasks = errors.New("no tasks in test dequeuer")

type sleepyDequeuer struct {
	maxMS int
}

func (s *sleepyDequeuer) DequeueTask(ctx context.Context, name string) (*backend.Task, backend.TaskCompleter, error) {
	if s.maxMS > 0 {
		time.Sleep(time.Duration(rand.Intn(s.maxMS)) * time.Millisecond)
	}
	return nil, nil, errTestDequeueNoTasks
}

type checkedMaxDequeuer struct {
	max   int
	t     *testing.T
	inner backend.TaskDequeuer

	active int
	mu     sync.Mutex
}

func (w *checkedMaxDequeuer) DequeueTask(ctx context.Context, name string) (*backend.Task, backend.TaskCompleter, error) {
	w.checkWorkerLimit()
	defer w.done()

	return w.inner.DequeueTask(ctx, name)
}

func (w *checkedMaxDequeuer) checkWorkerLimit() {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.active++
	if w.active > w.max {
		w.t.Errorf("worker count went over expected max of %d", w.max)
	}
}

func (w *checkedMaxDequeuer) done() {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.active--
}

func TestDequeuePoolCanDequeue(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	d := &checkedMaxDequeuer{
		t:     t,
		max:   2,
		inner: &sleepyDequeuer{},
	}
	pool := newTaskDequeuePool(2, d)
	pool.Start(ctx)
	defer pool.Stop()

	_, _, err := pool.DequeueTask(ctx, "some task")
	assert.Equal(t, errTestDequeueNoTasks, err)
}

func TestDequeuePoolLimitsWorkers(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())

	defer cancel()
	d := &checkedMaxDequeuer{
		t:     t,
		max:   2,
		inner: &sleepyDequeuer{maxMS: 5},
	}
	wg := &sync.WaitGroup{}
	pool := newTaskDequeuePool(2, d)
	pool.Start(ctx)
	defer pool.Stop()
	for i := 0; i < 100; i++ {
		wg.Add(1)
		go func() {
			_, _, _ = pool.DequeueTask(ctx, "some task")
			wg.Done()
		}()
	}
	wg.Wait()
}

func TestDequeuePoolWaitingRequestsRespectContexts(t *testing.T) {
	tCtx, tCancel := context.WithCancel(context.Background())
	defer tCancel()

	d := &checkedMaxDequeuer{
		t:     t,
		max:   2,
		inner: &sleepyDequeuer{},
	}

	pool := newTaskDequeuePool(0, d) // no worker, job should time out
	pool.Start(tCtx)
	defer pool.Stop()

	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Millisecond)
	defer cancel()
	_, _, err := pool.DequeueTask(ctx, "some task")
	assert.Equal(t, context.DeadlineExceeded, err)
}
