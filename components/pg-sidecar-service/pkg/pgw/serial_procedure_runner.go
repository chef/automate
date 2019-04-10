package pgw

// SerialProcedureRunner runs SerialProcedure's in sequentially and serially.
// This allows us to queue database operations and wait for them to complete
// using channels instead of mutexes.
type SerialProcedureRunner struct {
	C    *ProcedureChan
	stop *StructChan
}

// Start starts the SerialProcedureRunner. It listens on it's channel and processes
// SerialProcedure's serially without requiring a mutex.
func (r *SerialProcedureRunner) Start() {
	// nextProcedure pulls from the runners Procedure queue or returns nil if
	// the queue is empty
	nextProcedure := func() Procedure {
		select {
		case p, valid := <-r.C.C:
			if !valid {
				return nil
			}
			return p
		default:
			return nil
		}
	}

	// runProcedure first checks if the Procedure's context has been cancelled or
	// if the deadline has been exceeded. If it has, it sends the context's err
	// to the error channel. If the context is still valid it will run the
	// the procedure and reports any error encountered into the procedures
	// error channel. If the procedure completes successfully it'll close
	// the finished channel.
	runProcedure := func(p Procedure) {
		select {
		case <-p.Ctx().Done():
			p.ErrC().C <- p.Ctx().Err()
		default:
			if err := p.Run(); err != nil {
				p.ErrC().C <- err
			} else {
				p.FinishedC().Close()
			}
		}
	}

	// runRemainingProcedures processes procedures serially until the queue
	// is empty.
	runRemainingProcedures := func() {
		for {
			p := nextProcedure()
			if p == nil {
				break
			}

			runProcedure(p)
		}
	}

	go func() {
		for {
			// Do a non-blocking check of the stop channel on each iteration to
			// make sure we don't starve the stop channel.
			select {
			case <-r.stop.C:
				// If we're stopping, close the input channel and process any
				// remaining Procedure's
				r.C.Close()
				runRemainingProcedures()
				break
			default:
			}

			// Now block until we're signaled to stop or we have incoming procedures
			// to run.
			select {
			case <-r.stop.C:
				// If we're stopping, close the input channel and process any
				// remaining Procedure's
				r.C.Close()
				runRemainingProcedures()
				break
			case p, valid := <-r.C.C:
				if valid {
					runProcedure(p)
				}
			}
		}
	}()
}

// Stop terminates the SerialProcedureRunner's processing goroutine
func (r *SerialProcedureRunner) Stop() {
	r.stop.Close()
}

// NewSerialProcedureRunner returns a default SerialProcedureRunner
func NewSerialProcedureRunner() *SerialProcedureRunner {
	return &SerialProcedureRunner{
		// 35 should be more than enough for all services to start at the same
		// and start making pg-sidecar requests without running into resource
		// constraints.
		C:    NewProcedureChan(35),
		stop: NewStructChan(1),
	}
}
