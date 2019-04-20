package history

var (
	interceptors []EventInterceptor
)

type EventInterceptor interface {
	LogStart(*StartEvent)
	LogComplete(*CompleteEvent)
}

func RegisterInterceptor(interceptor EventInterceptor) {
	interceptors = append(interceptors, interceptor)
}

func logStart(startEvent *StartEvent) {
	for _, interceptor := range interceptors {
		interceptor.LogStart(startEvent)
	}
}

func logComplete(completeEvent *CompleteEvent) {
	for _, interceptor := range interceptors {
		interceptor.LogComplete(completeEvent)
	}
}
