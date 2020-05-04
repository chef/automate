package limiter

import (
	"errors"
	"sync"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
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
		err := status.Errorf(codes.ResourceExhausted,
			"Resource limit exceeded for %s: cur=%d max=%d", l.name, cur, max)
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

func (l noopRequestLimiter) TakeTicket() (Ticket, error) {
	return noopRequestLimiterTicket{}, nil
}

func (noopRequestLimiterTicket) Return() {

}
