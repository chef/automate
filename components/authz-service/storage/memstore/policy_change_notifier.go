package memstore

import (
	"sync"

	v2 "github.com/chef/automate/components/authz-service/storage/v2"
)

type policyChangeNotifierManager struct {
	lock     sync.Mutex
	channels []*policyChangeNotifier
}

type policyChangeNotifier struct {
	manager *policyChangeNotifierManager
	c       chan v2.PolicyChangeNotification
}

func newPolicyChangeNotifierManager() *policyChangeNotifierManager {
	return &policyChangeNotifierManager{}
}

func (manager *policyChangeNotifierManager) notifyChange() {
	manager.lock.Lock()
	defer manager.lock.Unlock()

	for _, c := range manager.channels {
		select {
		case c.c <- v2.PolicyChangeNotification{}:
		default:
		}
	}
}

func (manager *policyChangeNotifierManager) register() *policyChangeNotifier {
	manager.lock.Lock()
	defer manager.lock.Unlock()

	c := &policyChangeNotifier{
		c:       make(chan v2.PolicyChangeNotification, 1),
		manager: manager,
	}
	manager.channels = append(manager.channels, c)
	return c
}

func (*policyChangeNotifier) Close() error {
	// This is test code, don't need to worry about cleaning stuff up
	return nil
}

func (p *policyChangeNotifier) C() <-chan v2.PolicyChangeNotification {
	return p.c
}
