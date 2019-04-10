package pgw

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

func TestSerialProcedureRunner(t *testing.T) {
	t.Run("processes work serially", func(t *testing.T) {
		spr := NewSerialProcedureRunner()
		spr.Start()

		results := []int{}

		queueWork := func(spr *SerialProcedureRunner, i int) {
			ctx := context.Background()
			work := func() error {
				results = append(results, i)
				return nil
			}
			proc := NewProcedure(ctx, work)

			spr.C.C <- proc

			workCompleted := false
			select {
			case <-time.After(2 * time.Second):
			case <-proc.FinishedC().C:
				workCompleted = true
			}

			require.True(t, workCompleted, "work was completed")
		}

		// Add procedures to the queue
		for i := 1; i < 4; i++ {
			queueWork(spr, i)
		}

		// Assert that the work was completed
		require.Equal(t, []int{1, 2, 3}, results)
	})

	t.Run("gracefully drains work queue when stopping", func(t *testing.T) {
		spr := NewSerialProcedureRunner()

		// Stop() closes the spr's stop channel which is only a signal to
		// procedure goroutine. Since we haven't started it yet this only
		// tells the procedure goroutine to stop _after_ it processes queued
		// work and only _after_ we've started it.
		spr.Stop()

		// Add procedure to the queue
		sum := 0
		ctx := context.Background()
		work := func() error {
			time.Sleep(2 * time.Second)
			sum++
			return nil
		}
		proc := NewProcedure(ctx, work)

		spr.C.C <- proc

		// Actually start our goroutine
		spr.Start()

		workCompleted := false
		select {
		case <-time.After(3 * time.Second):
		case <-proc.FinishedC().C:
			workCompleted = true
		}

		// Assert that the work was completed
		require.True(t, workCompleted, "work was completed")
		require.Equal(t, 1, sum)

		// Assert that the spr channel is closed so that no new work can be
		// completed
		_, ok := <-spr.C.C
		require.False(t, ok, "spr channel was not closed")
	})
}
