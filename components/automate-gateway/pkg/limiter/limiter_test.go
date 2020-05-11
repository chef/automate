package limiter_test

import (
	"errors"
	"sync"
	"testing"
	"time"

	"github.com/chef/automate/components/automate-gateway/pkg/limiter"
	"github.com/stretchr/testify/require"
)

func TestInflightRequestLimiter(t *testing.T) {
	t.Run("can take a ticket", func(t *testing.T) {
		l := limiter.NewInflightRequestLimiter("test", 1)
		_, err := l.TakeTicket()
		require.NoError(t, err)
	})

	t.Run("can return a ticket", func(t *testing.T) {
		l := limiter.NewInflightRequestLimiter("test", 1)
		ticket, err := l.TakeTicket()
		require.NoError(t, err)
		ticket.Return()
		_, err = l.TakeTicket()
		require.NoError(t, err)
	})

	t.Run("returns an error when there are no available tickets", func(t *testing.T) {
		l := limiter.NewInflightRequestLimiter("test", 1)
		ticket, err := l.TakeTicket()
		require.NoError(t, err)
		_, err = l.TakeTicket()
		require.True(t, errors.Is(err, limiter.ErrLimitExceeded))
		ticket.Return()
		ticket, err = l.TakeTicket()
		require.NoError(t, err)
	})

	t.Run("concurrency test", func(t *testing.T) {
		l := limiter.NewInflightRequestLimiter("test", 10)
		wg := sync.WaitGroup{}
		wg.Add(1000)
		for i := 0; i < 1000; i++ {
			go func() {
				var ticket limiter.Ticket
				for {
					var err error
					ticket, err = l.TakeTicket()
					if err == nil {
						break
					}
					time.Sleep(1 * time.Millisecond) // let another goroutine run
				}
				time.Sleep(time.Microsecond)
				ticket.Return()
				wg.Done()
			}()
		}
		wg.Wait()
		for i := 0; i < 10; i++ {
			_, err := l.TakeTicket()
			require.NoError(t, err)
		}
		_, err := l.TakeTicket()
		require.True(t, errors.Is(err, limiter.ErrLimitExceeded))
	})
}
