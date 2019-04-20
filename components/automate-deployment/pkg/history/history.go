package history

import (
	"context"
	"errors"
	"time"

	"github.com/gofrs/uuid"
	"github.com/sirupsen/logrus"
)

var nowProvider = func() time.Time {
	return time.Now()
}

type Logger struct {
	Tags []Tag
}

func WithTags(tags ...Tag) *Logger {
	return &Logger{
		Tags: tags,
	}
}

func LogEvent(ctx context.Context, name EventClass, description string) context.Context {
	lopts := Logger{}
	return lopts.LogEvent(ctx, name, description)
}

func (lopts *Logger) LogEvent(ctx context.Context, name EventClass, description string) context.Context {
	id, err := genID()
	if err != nil {
		logrus.WithError(err).Debug("Failed to generate event id")
		return ctx
	}

	hCtx := &historyContext{
		id: id,
	}

	hasParent := accessInternalCtx(ctx, func(req *historyContext) {
		hCtx.cause = req
	})

	if !hasParent {
		logrus.Warnf("Parent not found for %s", name)
	}

	logStart(&StartEvent{
		Event:       hCtx.toEvent(),
		Cause:       hCtx.cause.GetID(),
		Name:        name,
		Description: description,
		Tags:        lopts.Tags,
	})
	return toContextContext(ctx, hCtx)
}

func Fail(ctx context.Context, msg string) {
	FailErr(ctx, errors.New(msg))
}

func FailErr(ctx context.Context, err error) {
	accessInternalCtx(ctx, func(hCtx *historyContext) {
		hCtx.done = true
		logComplete(&CompleteEvent{
			Event: hCtx.toEvent(),
			Err:   err,
		})
	})
}

func Succeed(ctx context.Context) {
	hasHistory := accessInternalCtx(ctx, func(hCtx *historyContext) {
		hCtx.done = true
		logComplete(&CompleteEvent{
			Event: hCtx.toEvent(),
		})
	})
	if !hasHistory {
		logrus.Error("History context not found")
	}
}

func Do(ctx context.Context, name EventClass, description string, f func(context.Context) error) error {
	logger := Logger{}
	return logger.Do(ctx, name, description, f)
}
func (logger *Logger) Do(ctx context.Context, name EventClass, description string, f func(context.Context) error) error {
	childCtx := logger.LogEvent(ctx, name, description)
	err := f(childCtx)
	if err != nil {
		FailErr(childCtx, err)
		return err
	}
	Succeed(childCtx)
	return nil
}

func genID() (EventID, error) {
	id, err := uuid.NewV4()
	if err != nil {
		return errEventID, err
	}

	return EventID(id.String()), nil
}
