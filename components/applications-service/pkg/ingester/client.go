package ingester

type Client interface {
	Run()
	Connect() error
	IngestMessage([]byte)

	ResetStats()
	GetEventStats() (int64, int64, int64)
	EventsSuccessful() int64
	EventsFailed() int64
	EventsProcessed() int64
	QueueLen() int
}
