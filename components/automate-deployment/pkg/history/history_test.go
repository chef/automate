package history

import (
	"context"
	"errors"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type mockInterceptor struct {
	startEvents    []*StartEvent
	completeEvents []*CompleteEvent
}

func (m *mockInterceptor) LogStart(ev *StartEvent) {
	m.startEvents = append(m.startEvents, ev)
}

func (m *mockInterceptor) LogComplete(ev *CompleteEvent) {
	m.completeEvents = append(m.completeEvents, ev)
}

func (m *mockInterceptor) checkCommon(t *testing.T) {
	firstComplete := len(m.startEvents) - 1

	require.True(t, len(m.startEvents) > 0)
	require.Equal(t, len(m.startEvents), len(m.completeEvents))

	// The event class is set correctly in both start and complete
	require.Equal(t, testEventClass, m.startEvents[0].EventClass)
	require.Equal(t, testEventClass, m.completeEvents[firstComplete].EventClass)

	// start and complete must have the same ID
	require.NotEmpty(t, m.startEvents[0].ID)
	require.Equal(t, m.startEvents[0].ID, m.completeEvents[firstComplete].ID)

	// The first start event has no cause
	require.Empty(t, m.startEvents[0].Cause)

	// The timestamps were set
	require.WithinDuration(t, time.Now(), m.startEvents[0].Timestamp, time.Minute)
	require.WithinDuration(t, time.Now(), m.completeEvents[firstComplete].Timestamp, time.Minute)
}

var testEventClass = EventClass{
	Name:        "TestEvent",
	Description: "Testing",
}

func setupInterceptor() *mockInterceptor {
	testInterceptor := &mockInterceptor{}
	interceptors = []EventInterceptor{testInterceptor}
	return testInterceptor
}
func TestNoParent(t *testing.T) {

	t.Run("successful event", func(t *testing.T) {
		testInterceptor := setupInterceptor()
		WithContext(context.Background()).Record(testEventClass, func(context.Context) error { return nil })
		// We should have 1 start and finish event
		require.Len(t, testInterceptor.startEvents, 1)
		require.Len(t, testInterceptor.completeEvents, 1)

		testInterceptor.checkCommon(t)

		// The complete event should have no error
		require.Nil(t, testInterceptor.completeEvents[0].Err)
	})

	t.Run("failed event", func(t *testing.T) {
		testInterceptor := setupInterceptor()
		err := errors.New("Foo")
		WithContext(context.Background()).Record(testEventClass, func(context.Context) error { return err })
		testInterceptor.checkCommon(t)

		// The complete event should have no error
		require.Equal(t, err, testInterceptor.completeEvents[0].Err)
	})
}

func TestWithParent(t *testing.T) {

	t.Run("successful event", func(t *testing.T) {
		testInterceptor := setupInterceptor()
		WithContext(context.Background()).Record(testEventClass, func(ctx context.Context) error {
			return Record(ctx, testEventClass, func(context.Context) error { return nil })
		})
		// We should have 1 start and finish event
		require.Len(t, testInterceptor.startEvents, 2)
		require.Len(t, testInterceptor.completeEvents, 2)

		testInterceptor.checkCommon(t)

		// The first start event has no cause
		require.Equal(t, testInterceptor.startEvents[0].ID, testInterceptor.startEvents[1].Cause)
	})

}

func TestWithTags(t *testing.T) {
	t.Run("successful event", func(t *testing.T) {
		testInterceptor := setupInterceptor()
		testTag := Tag{Key: "key", Value: "value"}
		WithContext(context.Background()).
			WithTags(testTag).
			Record(testEventClass, func(context.Context) error { return nil })

		testInterceptor.checkCommon(t)

		// The complete event should have no error
		require.Nil(t, testInterceptor.completeEvents[0].Err)
		assert.Equal(t, testTag, testInterceptor.startEvents[0].Tags[0])
	})

}
