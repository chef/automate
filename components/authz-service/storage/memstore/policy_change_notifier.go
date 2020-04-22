package memstore

import (
	"sync"

	"github.com/chef/automate/components/authz-service/storage"
)

type policyChangeNotifierManager struct {
	lock     sync.Mutex
	channels []*policyChangeNotifier
}

type policyChangeNotifier struct {
	manager *policyChangeNotifierManager
	c       chan storage.PolicyChangeNotification
}

func newPolicyChangeNotifierManager() *policyChangeNotifierManager {
	return &policyChangeNotifierManager{}
}

func (manager *policyChangeNotifierManager) notifyChange() {
	manager.lock.Lock()
	defer manager.lock.Unlock()

	for _, c := range manager.channels {
		select {
		case c.c <- storage.PolicyChangeNotification{}:
		default:
		}
	}
}

func (manager *policyChangeNotifierManager) register() *policyChangeNotifier {
	manager.lock.Lock()
	defer manager.lock.Unlock()

	c := &policyChangeNotifier{
		c:       make(chan storage.PolicyChangeNotification, 1),
		manager: manager,
	}
	manager.channels = append(manager.channels, c)
	return c
}

func (*policyChangeNotifier) Close() error {
	// This is test code, don't need to worry about cleaning stuff up
	return nil
}

func (p *policyChangeNotifier) C() <-chan storage.PolicyChangeNotification {
	return p.c
}
