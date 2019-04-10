package manifest

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type mockManifestProviderResp struct {
	m   *A2
	err error
}
type mockManifestProvider struct {
	t    *testing.T
	i    int
	resp []mockManifestProviderResp
}

func newMockManifestProvider(t *testing.T) *mockManifestProvider {
	return &mockManifestProvider{
		t:    t,
		resp: []mockManifestProviderResp{},
	}
}

func (mp *mockManifestProvider) AddResponse(m *A2, err error) {
	mp.resp = append(mp.resp, mockManifestProviderResp{
		m:   m,
		err: err,
	})
}

type nowProvider struct {
	t    *testing.T
	i    int
	nows []time.Time
}

func newNowProvider(t *testing.T) *nowProvider {
	return &nowProvider{
		t:    t,
		nows: []time.Time{},
	}
}

func (np *nowProvider) addNow(now time.Time) {
	np.nows = append(np.nows, now)
}

func (np *nowProvider) tick() {
	np.i++
}

func (np *nowProvider) now() time.Time {
	require.True(np.t, np.i < len(np.nows))
	return np.nows[np.i]
}

func (mp *mockManifestProvider) GetCurrentManifest(ctx context.Context, channel string) (*A2, error) {
	i := mp.i
	require.True(mp.t, i < len(mp.resp), "more calls to GetCurrentManifest than expected")
	mp.i++
	return mp.resp[i].m, mp.resp[i].err
}

// TODO(ssd) 2018-09-13: This is a bit wrong since now this counter covers both functions
func (mp *mockManifestProvider) GetManifest(ctx context.Context, release string) (*A2, error) {
	i := mp.i
	require.True(mp.t, i < len(mp.resp), "more calls to GetManifest than expected")
	mp.i++
	return mp.resp[i].m, mp.resp[i].err
}

func TestCachingReleaseManifestProvider(t *testing.T) {

	t.Run("Fetches a new manifest when none is cached", func(t *testing.T) {
		mockProvider := newMockManifestProvider(t)
		mockProvider.AddResponse(&A2{Build: "foobuild"}, nil)
		nowProvider := newNowProvider(t)
		startTime := time.Now()
		nowProvider.addNow(startTime)
		caching := newCachingReleaseManifestProvider(mockProvider, time.Minute)
		caching.nowProvider = nowProvider.now

		entryFoo, err := caching.GetCurrentManifest(context.Background(), "foo")
		require.NoError(t, err)
		assert.Equal(t, "foobuild", entryFoo.Build)
	})

	t.Run("Fetches a cached manifest when one exists and is not expired", func(t *testing.T) {
		mockProvider := newMockManifestProvider(t)
		mockProvider.AddResponse(&A2{Build: "foobuild"}, nil)
		mockProvider.AddResponse(&A2{Build: "barbuild"}, nil)

		nowProvider := newNowProvider(t)
		startTime := time.Now()
		nowProvider.addNow(startTime)
		nowProvider.addNow(startTime.Add(time.Minute))
		caching := newCachingReleaseManifestProvider(mockProvider, time.Minute)
		caching.nowProvider = nowProvider.now

		entryFooA, err := caching.GetCurrentManifest(context.Background(), "foo")
		nowProvider.tick()
		require.NoError(t, err)
		entryFooB, err := caching.GetCurrentManifest(context.Background(), "foo")
		require.NoError(t, err)
		assert.Equal(t, "foobuild", entryFooA.Build)
		assert.Equal(t, "foobuild", entryFooB.Build)
	})

	t.Run("Fetches a new manifest when one exists but is expired", func(t *testing.T) {
		mockProvider := newMockManifestProvider(t)
		mockProvider.AddResponse(&A2{Build: "foobuild"}, nil)
		mockProvider.AddResponse(&A2{Build: "barbuild"}, nil)

		nowProvider := newNowProvider(t)
		startTime := time.Now()
		nowProvider.addNow(startTime)
		nowProvider.addNow(startTime.Add(2 * time.Minute))
		caching := newCachingReleaseManifestProvider(mockProvider, time.Minute)
		caching.nowProvider = nowProvider.now

		entryFooA, err := caching.GetCurrentManifest(context.Background(), "foo")
		nowProvider.tick()
		require.NoError(t, err)
		entryFooB, err := caching.GetCurrentManifest(context.Background(), "foo")
		require.NoError(t, err)
		assert.Equal(t, "foobuild", entryFooA.Build)
		assert.Equal(t, "barbuild", entryFooB.Build)
	})
}
