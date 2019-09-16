package cereal

import "sync"

// StartGuard is used to enforce that Start is only called once for
// various structures inside cereal.
//
// We were previously using sync.WaitGroup for this, but this allows
// us to customize the error message a bit.
//
// The zero-value of this should work as expected, but you can create
// a StartGuard with a custom message using NewStartGuard.
type StartGuard struct {
	msg     string
	started bool
	mu      sync.Mutex
}

func NewStartGuard(msg string) StartGuard {
	return StartGuard{
		msg: msg,
	}
}

func (s *StartGuard) Started() {
	s.mu.Lock()
	if s.started {
		if s.msg != "" {
			panic(s.msg)
		} else {
			panic("Started() on StartGuard called more than once.")
		}
	}
	s.started = true
	s.mu.Unlock()
}
