package history

import "context"

type historyKeyType int

const metadataKey historyKeyType = iota

type metadata struct {
	cause      *metadata
	id         EventID
	eventClass EventClass
	done       bool
}

func (m *metadata) GetID() EventID {
	if m == nil {
		return noEventID
	}
	return m.id
}

func (m *metadata) toEvent() Event {
	return Event{
		ID:         m.GetID(),
		Timestamp:  nowProvider(),
		EventClass: m.eventClass,
	}
}

// accessMetadata is a guard for accessing the history metadata that is stored
// in a context. If the context has the metadata, f is called, and true is returned.
// Otherwise, f is not called and false is returned
func accessMetadata(ctx context.Context, f func(*metadata)) bool {
	if m, ok := ctx.Value(metadataKey).(*metadata); ok {
		f(m)
		return true
	}
	return false
}

func attachMetadataToContext(ctx context.Context, m *metadata) context.Context {
	return context.WithValue(ctx, metadataKey, m)
}
