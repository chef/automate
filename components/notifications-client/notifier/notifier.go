package notifier

import (
	"github.com/chef/automate/components/notifications-client/api"
	log "github.com/sirupsen/logrus"
	context "golang.org/x/net/context"
	"google.golang.org/grpc"
)

type notifierOptions struct {
	concurrency int
	backlog     int
}

var defaultNotifierOptions = notifierOptions{
	concurrency: 2,
	backlog:     10,
}

type NotifierOption func(*notifierOptions)

// WithConcurrency returns a NotifierOption which sets how many notification requests can be
// in flight at the same time. The minimum value allowed is 1
func WithConcurrency(c int) NotifierOption {
	return func(o *notifierOptions) {
		if c >= 1 {
			o.concurrency = c
		}
	}
}

// WithBacklog returns a NotifierOption which sets how requests are allowed to be queued up
// at the same time.
func WithBacklog(b int) NotifierOption {
	return func(o *notifierOptions) {
		if b >= 0 {
			o.backlog = b
		}
	}
}

// Notifier an interface that will allow us to create different notifier implementations
type Notifier interface {
	Send(context.Context, *api.Event)
	QueueSize() int
}

type notifier struct {
	client api.NotificationsClient
	opts   notifierOptions
	queue  chan job
}

func New(grpc_endpoint *grpc.ClientConn, opts ...NotifierOption) Notifier {
	return NewWithClient(api.NewNotificationsClient(grpc_endpoint), opts...)
}

func NewWithClient(client api.NotificationsClient, opts ...NotifierOption) Notifier {
	n := new(notifier)
	n.client = client

	n.opts = defaultNotifierOptions
	for _, opt := range opts {
		opt(&n.opts)
	}

	// The queue can ge as big as the backlog option
	n.queue = make(chan job, n.opts.backlog)

	for i := 0; i < n.opts.concurrency; i++ {
		go worker(n)
	}
	return n

}

type job struct {
	ctx context.Context
	ev  *api.Event
}

func worker(n *notifier) {
	for job := range n.queue {
		_, err := n.client.Notify(job.ctx, job.ev)
		if err != nil {
			log.WithFields(log.Fields{"id": job.ev.Id}).WithError(err).Error("Failed to send event")
		}
	}
}

func (n *notifier) Send(_ context.Context, ev *api.Event) {
	//TODO: We may need to grab values from the context, but we must
	// create a new one as this is a background task. Using context.TODO
	// to make sure it gets looked at again
	select {
	case n.queue <- job{ctx: context.TODO(), ev: ev}:
	default:
		log.WithFields(log.Fields{"id": ev.Id}).Error("Dropping event")
	}
}

// QueueSize returns the queue channel buffer_size
// NOTE: The queue can ge as big as the backlog option
func (n *notifier) QueueSize() int {
	return len(n.queue)
}
