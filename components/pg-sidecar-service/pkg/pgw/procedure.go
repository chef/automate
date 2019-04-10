package pgw

import (
	"context"
	"sync"
)

// Procedure in an interface that describes a unit of work that the
// SerialProcedureRunner can run.
type Procedure interface {
	// Ctx returns the context for deadlines and cancellation
	Ctx() context.Context
	// FinishedC returns a channel that will be closed if the procedure
	// completes successfully
	FinishedC() *StructChan
	// ErrC returns a channel which will return an error if the procedure
	// fails
	ErrC() *ErrChan
	// Run runs the procedure's work function
	Run() error
	// Wait takes a timeout waits for the procedure to complete. It will return
	// any error that it encounters on the ErrC or if the timeout is rea
	// an error if the Ctx deadline is reached.
	Wait() error
	// Close performs any cleanup actions that need to be performed
	Close()
}

// procedure represents a unit of work for the SerialProcedureRunner. It
// takes a request ctx, two channels to send finished or error notifications to,
// and a function of the work to be performed.
type procedure struct {
	ctx       context.Context
	finishedC *StructChan
	errC      *ErrChan
	proc      func() error
}

// NewProcedure takes a request context and procedure work function and returns
// a runnable procedure
func NewProcedure(ctx context.Context, proc func() error) Procedure {
	finishedC := NewStructChan(1)
	errC := NewErrChan(1)
	return &procedure{
		ctx:       ctx,
		finishedC: finishedC,
		errC:      errC,
		proc:      proc,
	}
}

// Ctx returns the procedures Context
func (s *procedure) Ctx() context.Context {
	return s.ctx
}

// FinishedC returns the procedures finished channel
func (s *procedure) FinishedC() *StructChan {
	return s.finishedC
}

// ErrC returns the procedures error channel
func (s *procedure) ErrC() *ErrChan {
	return s.errC
}

// Run execute the procedures work function
func (s *procedure) Run() error {
	return s.proc()
}

// Wait waits for procedures work function to complete
func (s *procedure) Wait() error {
	select {
	case <-s.finishedC.C:
		return nil
	case err := <-s.errC.C:
		return err
	}
}

// Close closes the procedures channels
func (s *procedure) Close() {
	s.finishedC.Close()
	s.errC.Close()
}

// ErrChan is a close() safe error chan
type ErrChan struct {
	C    chan error
	once sync.Once
}

// NewErrChan takes a buffer length and returns a buffered ErrChan
func NewErrChan(buffer int) *ErrChan {
	return &ErrChan{C: make(chan error, buffer)}
}

// Close safely closes the embedded channel once to prevent a panic if it is
// called multiple times.
func (c *ErrChan) Close() {
	c.once.Do(func() { close(c.C) })
}

// StructChan is a close() safe struct chan
type StructChan struct {
	C    chan struct{}
	once sync.Once
}

// NewStructChan takes a buffer length and returns a buffered StructChan
func NewStructChan(buffer int) *StructChan {
	return &StructChan{C: make(chan struct{}, buffer)}
}

// Close safely closes the embedded channel once to prevent a panic if it is
// called multiple times.
func (c *StructChan) Close() {
	c.once.Do(func() { close(c.C) })
}

// ProcedureChan is a close() safe SerialProcedure chan
type ProcedureChan struct {
	C    chan Procedure
	once sync.Once
}

// NewProcedureChan takes a buffer length and returns a buffered ProcedureChan
func NewProcedureChan(buffer int) *ProcedureChan {
	return &ProcedureChan{C: make(chan Procedure, buffer)}
}

// Close safely closes the embedded channel once to prevent a panic if it is
// called multiple times.
func (c *ProcedureChan) Close() {
	c.once.Do(func() { close(c.C) })
}
