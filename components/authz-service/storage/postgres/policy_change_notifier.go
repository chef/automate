package postgres

import (
	"context"
	"time"

	"github.com/lib/pq"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/authz-service/storage"
)

type policyChangeNotifier struct {
	conninfo             string
	minReconnectInterval time.Duration
	maxReconnectInterval time.Duration
	pingInterval         time.Duration
	notificationChan     chan storage.PolicyChangeNotification
	shutdown             func()
}

func newPolicyChangeNotifier(ctx context.Context, conninfo string) (storage.PolicyChangeNotifier, error) {
	ctx, cancel := context.WithCancel(ctx)
	p := &policyChangeNotifier{
		conninfo:             conninfo,
		minReconnectInterval: 10 * time.Second,
		maxReconnectInterval: time.Minute,
		pingInterval:         10 * time.Second,
		notificationChan:     make(chan storage.PolicyChangeNotification, 1),
		shutdown:             cancel,
	}
	listener := pq.NewListener(p.conninfo, p.minReconnectInterval, p.maxReconnectInterval, nil)
	err := listener.Listen("policychange")
	if err != nil {
		return nil, err
	}

	go p.run(ctx, listener)
	return p, nil
}

func (p *policyChangeNotifier) C() <-chan storage.PolicyChangeNotification {
	return p.notificationChan
}

func (p *policyChangeNotifier) Close() error {
	p.shutdown()
	return nil
}

func (p *policyChangeNotifier) run(ctx context.Context, listener *pq.Listener) {

RUNLOOP:
	for {
		select {
		case <-ctx.Done():
			break RUNLOOP
		case n := <-listener.Notify:
			if n == nil {
				continue
			}
			select {
			case p.notificationChan <- storage.PolicyChangeNotification{}:
				logrus.Info("Accepted notification from postgres")
			default:
				logrus.Debug("Notification listener mailbox full")
			}
		case <-time.After(p.pingInterval):
			err := listener.Ping()
			if err != nil {
				logrus.WithError(err).Warn("Notification listener failed to ping database")
			}
		}
	}
	if err := listener.Close(); err != nil {
		logrus.WithError(err).Warn("Failed to close notification listener")
	}
}
