package manifest

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"os"
	"path"
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
		assert.Equal(t, "foobuild", entryFoo.Version())
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
		assert.Equal(t, "foobuild", entryFooA.Version())
		assert.Equal(t, "foobuild", entryFooB.Version())
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
		assert.Equal(t, "foobuild", entryFooA.Version())
		assert.Equal(t, "barbuild", entryFooB.Version())
	})
}

func TestGetCompatibleManifestVersion(t *testing.T) {
	ts := compatibleVerServer()
	defer ts.Close()

	tests := []struct {
		input             string
		version           string
		isMinorAvailable  bool
		isMajorAvailable  bool
		compatibleVersion string
		isError           bool
		errorString       string
	}{
		{
			input:             "set1",
			version:           "20211201164433",
			isMinorAvailable:  true,
			isMajorAvailable:  false,
			compatibleVersion: "20220113154113",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set1",
			version:           "20220113145751",
			isMinorAvailable:  true,
			isMajorAvailable:  false,
			compatibleVersion: "20220113154113",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set1",
			version:           "20220113154113",
			isMinorAvailable:  false,
			isMajorAvailable:  true,
			compatibleVersion: "3.2.1",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set1",
			version:           "3.0.0",
			isMinorAvailable:  true,
			isMajorAvailable:  false,
			compatibleVersion: "3.2.1",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set1",
			version:           "3.2.1",
			isMinorAvailable:  false,
			isMajorAvailable:  true,
			compatibleVersion: "4.1.2",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set2",
			version:           "20220113154113",
			isMinorAvailable:  false,
			isMajorAvailable:  true,
			compatibleVersion: "3.0.0",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set2",
			version:           "3.0.0",
			isMinorAvailable:  false,
			isMajorAvailable:  false,
			compatibleVersion: "3.0.0",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set3",
			version:           "3.0.0",
			isMinorAvailable:  false,
			isMajorAvailable:  true,
			compatibleVersion: "4.0.0",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set4",
			version:           "20220120081508",
			isMinorAvailable:  true,
			isMajorAvailable:  false,
			compatibleVersion: "20220120081530",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set5",
			version:           "3.0.0",
			isMinorAvailable:  true,
			isMajorAvailable:  false,
			compatibleVersion: "3.2.5",
			isError:           false,
			errorString:       "",
		},
	}

	for _, tc := range tests {
		t.Run(tc.version, func(t *testing.T) {
			url := fmt.Sprintf("%s?set=%s", ts.URL, tc.input)
			isMinorAvailable, isMajorAvailable, compatibleVersion, err := getCompatibleManifestVersion(context.TODO(), tc.version, "dev", "", url)
			if !tc.isError {
				assert.NoError(t, err)
				assert.Equal(t, tc.isMinorAvailable, isMinorAvailable)
				assert.Equal(t, tc.isMajorAvailable, isMajorAvailable)
				assert.Equal(t, tc.compatibleVersion, compatibleVersion)
			} else {
				assert.Error(t, err)
				assert.Contains(t, err.Error(), tc.errorString)
			}
		})
	}
}

func TestGetMinCurrentVersion(t *testing.T) {
	ts := compatibleVerServer()
	defer ts.Close()

	tests := []struct {
		input             string
		version           string
		compatibleVersion string
		isError           bool
		errorString       string
	}{
		{
			input:             "set1",
			version:           "4.1.2",
			compatibleVersion: "3.2.1",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set1",
			version:           "20220113145751",
			compatibleVersion: "20211201164433",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set1",
			version:           "5.4.5",
			compatibleVersion: "4.1.2",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set1",
			version:           "3.0.0",
			compatibleVersion: "20220113154113",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set1",
			version:           "3.2.1",
			compatibleVersion: "20220113154113",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set2",
			version:           "20220113154113",
			compatibleVersion: "20220113154113",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set2",
			version:           "3.0.0",
			compatibleVersion: "20220113154113",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set3",
			version:           "4.0.0",
			compatibleVersion: "3.0.0",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set4",
			version:           "20220120081530",
			compatibleVersion: "20220112171518",
			isError:           false,
			errorString:       "",
		},
		{
			input:             "set5",
			version:           "3.2.5",
			compatibleVersion: "3.0.0",
			isError:           false,
			errorString:       "",
		},
	}

	for _, tc := range tests {
		t.Run(tc.version, func(t *testing.T) {
			url := fmt.Sprintf("%s?set=%s", ts.URL, tc.input)
			compatibleVersion, err := GetMinimumCurrentManifestVersion(context.TODO(), tc.version, "dev", "", url)
			if !tc.isError {
				assert.NoError(t, err)
				assert.Equal(t, tc.compatibleVersion, compatibleVersion)
			} else {
				assert.Error(t, err)
				assert.Contains(t, err.Error(), tc.errorString)
			}
		})
	}
}

func TestGetAllVersions(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		pValues := r.URL.Query()
		param := pValues["set"]
		var resp []string
		if param[0] == "set1" {
			resp = []string{
				"20220113154113",
				"20211201164433",
				"20211220104140",
				"20220113145751",
			}
		} else if param[0] == "set2" {
			resp = []string{
				"20220113154113",
				"3.0.0",
				"3.2.6",
				"4.2.4",
				"3.2.16",
			}
		}

		bytes, _ := json.Marshal(resp)
		w.Write(bytes)
	}))
	defer ts.Close()

	tests := []struct {
		input  string
		output []string
	}{
		{
			input:  "set1",
			output: []string{"20211201164433", "20211220104140", "20220113145751", "20220113154113"},
		},
		{
			input:  "set2",
			output: []string{"20220113154113", "3.0.0", "3.2.6", "3.2.16", "4.2.4"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.input, func(t *testing.T) {
			url := fmt.Sprintf("%s?set=%s", ts.URL, tc.input)
			result, err := getAllVersions(context.TODO(), url)
			assert.NoError(t, err)
			assert.Equal(t, tc.output, result)
		})
	}
}

var versions = `
[
  "20180319150121",
  "20220209045542",
  "3.5.6",
  "3.0.0", 
  "20180322085330",
  "20220121191356"
]
`

func TestGetAllVersionsFromPath(t *testing.T) {
	dir, err := ioutil.TempDir("", "VersionsFromFileTest")
	assert.NoError(t, err, "creating temporary dir")
	defer os.RemoveAll(dir)
	filename := path.Join(dir, "versions.json")
	err = ioutil.WriteFile(filename, []byte(versions), 0700)
	assert.NoError(t, err, "writing test data")

	resp, err := GetAllVersions(context.Background(), "", filename)
	assert.NoError(t, err, "getting all versions")
	assert.Equal(t, []string{"20180319150121", "20180322085330", "20220121191356", "20220209045542", "3.0.0", "3.5.6"}, resp)

}

func compatibleVerServer() *httptest.Server {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		pValues := r.URL.Query()
		param := pValues["set"]
		var resp []string
		if param[0] == "set1" {
			resp = []string{
				"20211201164433",
				"20211220104140",
				"20220113145751",
				"20220113154113",
				"3.0.0",
				"3.1.9",
				"3.2.1",
				"4.0.0",
				"4.1.2",
				"5.4.5",
			}
		} else if param[0] == "set2" {
			resp = []string{
				"20220113154113",
				"3.0.0",
			}
		} else if param[0] == "set3" {
			resp = []string{
				"20220113154113",
				"3.0.0",
				"4.0.0",
			}
		} else if param[0] == "set4" {
			resp = []string{
				"20220112171518",
				"20220112175624",
				"20220113145751",
				"20220113154113",
				"20220120081508",
				"20220120081530",
			}
		} else if param[0] == "set5" {
			resp = []string{
				"3.0.0",
				"3.2.5",
			}
		}
		bytes, _ := json.Marshal(resp)
		w.Write(bytes)
	}))
	return ts
}
