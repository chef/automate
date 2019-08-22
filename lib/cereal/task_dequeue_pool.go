package cereal

import (
	"context"
	"sync"

	"github.com/chef/automate/lib/cereal/backend"
)

// taskDequeuePool is a pool of workers calling DequeueTask on the
// given TaskDequeuer. The purpose of the pool is to limit concurrent
// dequeue operations being send to the backend.
type taskDequeuePool struct {
	size     int
	dequeuer backend.TaskDequeuer

	reqChan chan taskDequeueReq
	ctx     context.Context
	cancel  context.CancelFunc
	wg      sync.WaitGroup
	wgStart sync.WaitGroup
}

type taskDequeueReq struct {
	name     string
	ctx      context.Context
	respChan chan taskDequeueResp
}

type taskDequeueResp struct {
	err       error
	task      *backend.Task
	completer backend.TaskCompleter
}

func newTaskDequeuePool(size int, dequeuer backend.TaskDequeuer) *taskDequeuePool {
	p := &taskDequeuePool{
		size:     size,
		dequeuer: dequeuer,
		reqChan:  make(chan taskDequeueReq),
	}
	p.wgStart.Add(1)
	return p
}

func (d *taskDequeuePool) Start(ctx context.Context) {
	d.wgStart.Done() // panic if start is called twice

	ctx, cancel := context.WithCancel(ctx)
	d.ctx = ctx
	d.cancel = cancel

	d.wg.Add(d.size)
	for i := 0; i < d.size; i++ {
		go d.worker(d.ctx)
	}
}

func (d *taskDequeuePool) Stop() {
	if d.cancel != nil {
		d.cancel()
	}
	d.wg.Wait()
}

func (d *taskDequeuePool) DequeueTask(ctx context.Context, taskName string) (*backend.Task, backend.TaskCompleter, error) {
	respChan := make(chan taskDequeueResp)
	req := taskDequeueReq{
		name:     taskName,
		ctx:      ctx,
		respChan: respChan,
	}
	select {
	case d.reqChan <- req:
		// TODO(ssd) 2019-08-23: We don't wait on the context
		// here. We depend on the worker to respect the
		// context we sent in the dequeueReq.
		//
		// If we were to select on the context here, the
		// worker could dequeued the task and try to send it
		// to us after we've exited. This would be bad because
		// then nothing would stop the taskPinger in the pg
		// case, or close the grpc connection in the GRPC
		// case.
		//
		// At the moment, this wouldn't be a big deal since if
		// those contexts get cancelled it likely means the
		// entire service is shutting down. But, let's avoid
		// it anyway.
		//
		// We could alternatively avoid it by adding an
		// Abandon() on the task completer. Then if we were to
		// exit here early, we could have a goroutine that ate
		// the last respond and called Abandon() on it.
		resp := <-respChan
		return resp.task, resp.completer, resp.err
	case <-ctx.Done():
		return nil, nil, ctx.Err()
	}
}

func (d *taskDequeuePool) worker(ctx context.Context) {
	for {
		select {
		case req := <-d.reqChan:
			// NOTE(ssd) 2019-08-23: In-flight dequeues
			// currently cause Stop() to block until they
			// are complete, because the context used here
			// is from the client, not the worker. We
			// could create a combined context from the
			// worker and the client contexts, but then we
			// would either need to leak the resources
			// related to that context or somehow untangle
			// the TaskCompleter which will stop pinging
			// if the context we pass here is ever
			// cancelled.
			t, taskCompleter, err := d.dequeuer.DequeueTask(req.ctx, req.name)
			req.respChan <- taskDequeueResp{
				task:      t,
				completer: taskCompleter,
				err:       err,
			}
		case <-d.ctx.Done():
			d.wg.Done()
			return
		}
	}
}
