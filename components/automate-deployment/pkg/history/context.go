package history

import "context"

type historyKeyType int

const currentRequestKey historyKeyType = iota

type historyContext struct {
	cause *historyContext
	id    EventID
	done  bool
}

func (hCtx *historyContext) GetID() EventID {
	if hCtx == nil {
		return noEventID
	}
	return hCtx.id
}

func (hCtx *historyContext) toEvent() Event {
	return Event{
		ID:        hCtx.GetID(),
		Timestamp: nowProvider(),
	}
}

func accessInternalCtx(ctx context.Context, f func(*historyContext)) bool {
	if hctx, ok := ctx.Value(currentRequestKey).(*historyContext); ok {
		f(hctx)
		return true
	}
	return false
}

func toContextContext(ctx context.Context, hCtx *historyContext) context.Context {
	return context.WithValue(ctx, currentRequestKey, hCtx)
}
