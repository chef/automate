//This file was originally part of:
//https://github.com/brandur/sorg/blob/master/pool/pool.go
//It has the following license:
//
//MIT License
//
//Copyright (c) 2017 Brandur Leach
//
//Permission is hereby granted, free of charge, to any person obtaining a copy
//of this software and associated documentation files (the "Software"), to deal
//in the Software without restriction, including without limitation the rights
//to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//copies of the Software, and to permit persons to whom the Software is
//furnished to do so, subject to the following conditions:
//
//The above copyright notice and this permission notice shall be included in all
//copies or substantial portions of the Software.
//
//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//SOFTWARE.

package pool

import (
	"sync"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
)

type TaskResult interface{}

// Task encapsulates a work item that should go in a work pool.
type Task struct {
	// Err holds an error that occurred during a task. Its result is only
	// meaningful after Run has been called for the pool that holds it.
	Err    error
	Result TaskResult

	f func() (TaskResult, error)
}

// NewTask initializes a new task based on a given work function.
func NewTask(f func() (TaskResult, error)) *Task {
	return &Task{f: f}
}

// Run runs a Task and does appropriate accounting via a given sync.WorkGroup.
func (t *Task) Run(wg *sync.WaitGroup) {
	t.Result, t.Err = t.f()
	wg.Done()
}

// Pool is a worker group that runs a number of tasks at a configured
// concurrency.
type Pool struct {
	Tasks []*Task

	concurrency int
	tasksChan   chan *Task
	wg          sync.WaitGroup
}

// NewPool initializes a new pool with the given tasks and at the given
// concurrency.
func NewPool(tasks []*Task, concurrency int) *Pool {
	return &Pool{
		Tasks:       tasks,
		concurrency: concurrency,
		tasksChan:   make(chan *Task),
	}
}

// HasErrors indicates whether there were any errors from tasks run. Its result
// is only meaningful after Run has been called.
func (p *Pool) HasErrors() bool {
	for _, task := range p.Tasks {
		if task.Err != nil {
			return true
		}
	}
	return false
}

// GetErrors returns all errors from tasks run. Its result
// is only meaningful after Run has been called.
func (p *Pool) GetErrors() error {
	var err error
	for _, task := range p.Tasks {
		if task.Err != nil {
			err = errors.Wrap(err, "pool task error")
		}
	}
	return err
}

// Run runs all work within the pool and blocks until it's finished.
func (p *Pool) Run() {
	log.Debugf("Running %v task(s) at concurrency %v.",
		len(p.Tasks), p.concurrency)

	for i := 0; i < p.concurrency; i++ {
		go p.work()
	}

	p.wg.Add(len(p.Tasks))
	for _, task := range p.Tasks {
		p.tasksChan <- task
	}

	// all workers return
	close(p.tasksChan)

	p.wg.Wait()
}

// The work loop for any single goroutine.
func (p *Pool) work() {
	for task := range p.tasksChan {
		task.Run(&p.wg)
	}
}
