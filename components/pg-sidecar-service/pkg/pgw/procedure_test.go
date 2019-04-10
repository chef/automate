package pgw

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

func TestProcedure(t *testing.T) {
	t.Run("notifies error channel upon failure", func(t *testing.T) {
		spr := NewSerialProcedureRunner()
		spr.Start()

		ctx := context.Background()
		work := func() error {
			return errors.New("failed")
		}

		proc := NewProcedure(ctx, work)

		spr.C.C <- proc

		var err error
		select {
		case <-time.After(3 * time.Second):
			require.True(t, false, "timed out waiting for procedure to run")
		case err = <-proc.ErrC().C:
		}

		// Assert that the err channel received the failure
		require.Error(t, err, "err channel was not notified")
	})
}
