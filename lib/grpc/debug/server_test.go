package debug

import (
	"context"
	"io"
	"runtime/pprof"
	"testing"
	"time"

	"github.com/golang/protobuf/ptypes"
	"google.golang.org/grpc/metadata"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	api "github.com/chef/automate/lib/grpc/debug/debug_api"
)

type mockChunkWriter struct {
	b []byte
}

func (*mockChunkWriter) Context() context.Context {
	return context.Background()
}

func (m *mockChunkWriter) Send(c *api.Chunk) error {
	m.b = append(m.b, c.Chunk...)
	return nil
}

func (m *mockChunkWriter) DataReceived() []byte {
	if m.b == nil {
		return []byte{}
	}
	return m.b
}

func (*mockChunkWriter) SetHeader(metadata.MD) error {
	return nil
}
func (*mockChunkWriter) SendHeader(metadata.MD) error {
	return nil
}
func (*mockChunkWriter) SetTrailer(metadata.MD) {
}
func (*mockChunkWriter) SendMsg(m interface{}) error {
	return nil
}
func (*mockChunkWriter) RecvMsg(m interface{}) error {
	return nil
}

type mockSleeper struct {
	d time.Duration
}

func (m *mockSleeper) Sleep(d time.Duration) {
	m.d += d
}

func (m *mockSleeper) TimeSlept() time.Duration {
	return m.d
}

func TestTrace(t *testing.T) {
	// This test expects that tracing runs for a default of 30
	// seconds and returns some data
	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Trace(&api.TraceRequest{}, w)
	require.NoError(t, err)
	assert.NotEqual(t, []byte{}, w.DataReceived())
	assert.Equal(t, 30*time.Second, sleeper.TimeSlept())
}

func TestTraceWithValidSleep(t *testing.T) {
	// This test expects tracing to run for an overridden value of
	// 1 second and then return the tracing data
	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Trace(&api.TraceRequest{
		Duration: ptypes.DurationProto(time.Second),
	}, w)
	require.NoError(t, err)
	assert.NotEqual(t, []byte{}, w.DataReceived())
	assert.Equal(t, time.Second, sleeper.TimeSlept())
}

func TestTraceWithInvalidSleep(t *testing.T) {
	// This test expects that the server prevents you from
	// tracing for too long of a time
	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Trace(&api.TraceRequest{
		// We won't give people a footgun
		Duration: ptypes.DurationProto(time.Hour),
	}, w)
	require.Error(t, err)
	assert.Equal(t, []byte{}, w.DataReceived())
	assert.Equal(t, time.Duration(0), sleeper.TimeSlept())
}

func TestProfileUnknown(t *testing.T) {
	// This test expects the server returns an error if you try
	// to get a profile snapshot of a profile that does not exist
	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Profile(&api.ProfileRequest{
		ProfileName: "TestProfileUnknown",
		Duration:    ptypes.DurationProto(time.Second),
	}, w)
	require.Error(t, err)
	assert.Equal(t, []byte{}, w.DataReceived())
	assert.Equal(t, time.Duration(0), sleeper.TimeSlept())
}

func TestProfileCustomSnapshot(t *testing.T) {
	// This test asserts that we can register and use a custom
	// profile
	stopCalled := false
	RegisterProfileMetadata(ProfileMetadata{
		Name: "TestProfileCustomSnapshot",
		StartSnapshot: func(w io.Writer) error {
			_, err := w.Write([]byte("testdata"))
			return err
		},
		StopSnapshot: func() error {
			stopCalled = true
			return nil
		},
	})

	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Profile(&api.ProfileRequest{
		ProfileName: "TestProfileCustomSnapshot",
	}, w)
	require.NoError(t, err)
	assert.Equal(t, "testdata", string(w.DataReceived()))
	assert.Equal(t, time.Duration(0), sleeper.TimeSlept())
	assert.True(t, stopCalled, "The stop function was not called")
}

func TestProfileCustomSnapshotWithSleep(t *testing.T) {
	// This test asserts that a custom profile can have a default
	// sleep that is obeyed
	RegisterProfileMetadata(ProfileMetadata{
		Name: "TestProfileCustomSnapshotWithSleep",
		StartSnapshot: func(w io.Writer) error {
			_, err := w.Write([]byte("testdata"))
			return err
		},
		DefaultSleepDuration: time.Second,
	})

	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Profile(&api.ProfileRequest{
		ProfileName: "TestProfileCustomSnapshotWithSleep",
	}, w)
	require.NoError(t, err)
	assert.Equal(t, "testdata", string(w.DataReceived()))
	assert.Equal(t, time.Second, sleeper.TimeSlept())
}

func TestProfileCustomSnapshotWithoutStop(t *testing.T) {
	// This test asserts that a custom profile still works without
	// implementing the stop function
	RegisterProfileMetadata(ProfileMetadata{
		Name: "TestProfileCustomSnapshotWithoutStop",
		StartSnapshot: func(w io.Writer) error {
			_, err := w.Write([]byte("testdata"))
			return err
		},
	})

	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Profile(&api.ProfileRequest{
		ProfileName: "TestProfileCustomSnapshotWithoutStop",
	}, w)
	require.NoError(t, err)
	assert.Equal(t, "testdata", string(w.DataReceived()))
	assert.Equal(t, time.Duration(0), sleeper.TimeSlept())
}

func TestProfilePProfCPU(t *testing.T) {
	// This test asserts we can take a cpu profile snapshot. By default,
	// it is a 30 second snapshot
	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Profile(&api.ProfileRequest{
		ProfileName: "cpu",
	}, w)
	require.NoError(t, err)
	assert.NotEqual(t, []byte{}, w.DataReceived())
	assert.Equal(t, 30*time.Second, sleeper.TimeSlept())
}

func TestProfilePProfCPUWithInvalidSleep(t *testing.T) {
	// This test asserts the server does not allow absurdly long snapshot
	// times
	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Profile(&api.ProfileRequest{
		ProfileName: "cpu",
		Duration:    ptypes.DurationProto(time.Hour),
	}, w)
	require.Error(t, err)
	assert.Equal(t, []byte{}, w.DataReceived())
	assert.Equal(t, time.Duration(0), sleeper.TimeSlept())
}

func TestProfilePProfHeap(t *testing.T) {
	// This test asserts we can take a heap profile snapshot
	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Profile(&api.ProfileRequest{
		ProfileName: "heap",
	}, w)
	require.NoError(t, err)
	assert.NotEqual(t, []byte{}, w.DataReceived())
	assert.Equal(t, time.Duration(0), sleeper.TimeSlept())
}

func TestProfilePProfBlock(t *testing.T) {
	// This test asserts we can take a block profile snapshot
	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Profile(&api.ProfileRequest{
		ProfileName: "block",
	}, w)
	require.NoError(t, err)
	assert.NotEqual(t, []byte{}, w.DataReceived())
	assert.Equal(t, time.Duration(0), sleeper.TimeSlept())
}

func TestProfilePProfMutex(t *testing.T) {
	// This test asserts we can take a mutex profile snapshot
	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Profile(&api.ProfileRequest{
		ProfileName: "mutex",
	}, w)
	require.NoError(t, err)
	assert.NotEqual(t, []byte{}, w.DataReceived())
	assert.Equal(t, time.Duration(0), sleeper.TimeSlept())
}

func TestProfilePProfCustom(t *testing.T) {
	// This test asserts we can take a custom pprof profile snapshot
	profile := pprof.NewProfile("custom.TestProfilePProfCustom")
	profile.Add("stuff", 1)

	server := newDebugServer()
	sleeper := &mockSleeper{}
	w := &mockChunkWriter{}
	server.sleepFunc = sleeper.Sleep
	err := server.Profile(&api.ProfileRequest{
		ProfileName: "custom.TestProfilePProfCustom",
	}, w)
	require.NoError(t, err)
	assert.NotEqual(t, []byte{}, w.DataReceived())
	assert.Equal(t, time.Duration(0), sleeper.TimeSlept())
}
