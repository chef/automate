package history

import (
	"context"
	"errors"
	"time"

	"github.com/sirupsen/logrus"
)

var nowProvider = func() time.Time {
	return time.Now()
}

// Recorder records an event
type Recorder struct {
	tags []Tag
	ctx  context.Context
}

// WithContext returns a recorder that will use the given context as the parent context
// to record events. If the context has history metadata, recorded events will be linked
// to the ID contained in that metadata
func WithContext(ctx context.Context) *Recorder {
	return &Recorder{ctx: ctx}
}

// WithTags modifies the recorder to use the given tags to record an event
func (recorder *Recorder) WithTags(tags ...Tag) *Recorder {
	recorder.tags = tags
	return recorder
}

// RecordEvent starts recording an event with default options. Success or
// Fail/FailErr should be called with the returned context to complete the event
func RecordEvent(ctx context.Context, eventClass EventClass) context.Context {
	return WithContext(ctx).RecordEvent(eventClass)
}

// RecordEvent starts recording an event with the options set in the recorder. Success or
// Fail/FailErr should be called with the returned context to complete the event
func (recorder *Recorder) RecordEvent(eventClass EventClass) context.Context {
	ctx := recorder.ctx
	id, err := genID()
	if err != nil {
		logrus.WithError(err).Debug("Failed to generate event id")
		return ctx
	}

	m := &metadata{
		id:         id,
		eventClass: eventClass,
	}

	accessMetadata(ctx, func(req *metadata) {
		m.cause = req
	})

	logStart(&StartEvent{
		Event: m.toEvent(),
		Cause: m.cause.GetID(),
		Tags:  recorder.tags,
	})
	return attachMetadataToContext(ctx, m)
}

// Fail fails the event being recorded in the given context
func Fail(ctx context.Context, msg string) {
	FailErr(ctx, errors.New(msg))
}

// FailErr fails the event being recorded in the given context
func FailErr(ctx context.Context, err error) {
	accessMetadata(ctx, func(m *metadata) {
		m.done = true
		logComplete(&CompleteEvent{
			Event: m.toEvent(),
			Err:   err,
		})
	})
}

// Succeed marks the event being recorded in the given context as successful
func Succeed(ctx context.Context) {
	accessMetadata(ctx, func(m *metadata) {
		m.done = true
		logComplete(&CompleteEvent{
			Event: m.toEvent(),
		})
	})
}

// Record records the start and end of an event defined by the function provided. If the function
// returns an error, the event is marked as failed.
func Record(ctx context.Context, eventClass EventClass, f func(context.Context) error) error {
	return WithContext(ctx).Record(eventClass, f)
}

// Record records the start and end of an event defined by the function provided. If the function
// returns an error, the event is marked as failed.
func (recorder *Recorder) Record(eventClass EventClass, f func(context.Context) error) error {
	childCtx := recorder.RecordEvent(eventClass)
	err := f(childCtx)
	if err != nil {
		FailErr(childCtx, err)
		return err
	}
	Succeed(childCtx)
	return nil
}
