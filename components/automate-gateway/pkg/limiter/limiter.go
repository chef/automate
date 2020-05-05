package limiter

import (
	"errors"
	"fmt"
	"sync"
)

var ErrLimitExceeded = errors.New("Resource limit exceeded")

type Limiter interface {
	TakeTicket() (Ticket, error)
}

type Ticket interface {
	Return()
}

type inflightRequestLimiter struct {
	lock        sync.Mutex
	maxRequests int
	inflight    int
	name        string
}

type inflightRequestLimiterTicket struct {
	limiter  *inflightRequestLimiter
	returned bool
}

func NewInflightRequestLimiter(name string, maxRequests int) Limiter {
	return &inflightRequestLimiter{
		name:        name,
		maxRequests: maxRequests,
	}
}

func (l *inflightRequestLimiter) TakeTicket() (Ticket, error) {
	taken := false
	cur := 0
	max := 0
	l.lock.Lock()
	cur = l.inflight
	max = l.maxRequests
	if cur < max {
		l.inflight++
		taken = true
	}
	l.lock.Unlock()

	if !taken {
		err := fmt.Errorf("resource=%s cur=%d max=%d: %w",
			l.name, cur, max, ErrLimitExceeded)
		return nil, err
	}
	return &inflightRequestLimiterTicket{
		limiter: l,
	}, nil
}

func (ticket *inflightRequestLimiterTicket) Return() {
	if ticket.returned {
		return
	}
	l := ticket.limiter
	ticket.returned = true
	l.lock.Lock()
	l.inflight--
	l.lock.Unlock()
}

type noopRequestLimiter struct {
}

type noopRequestLimiterTicket struct {
}

func NewNoopRequestLimiter() Limiter {
	return noopRequestLimiter{}
}

func (noopRequestLimiter) TakeTicket() (Ticket, error) {
	return noopRequestLimiterTicket{}, nil
}

func (noopRequestLimiterTicket) Return() {

}
