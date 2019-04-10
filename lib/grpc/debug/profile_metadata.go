package debug

import (
	"io"
	"runtime"
	"runtime/pprof"
	"sync"
	"time"
)

// SetSampleRateFunc sets the sample rate (in Hz). It must
// return the original value that will be passed to SetSampleRateFunc
// to reset it.
type SetSampleRateFunc func(rate int) int

// StartSnapshotFunc starts the perf snapshot. The data must
// be written to the provided writer. If this function is not
// provided, the pprof profile with the matching name will be
// looked up and used to write the profile snapshot.
type StartSnapshotFunc func(io.Writer) error

// StopSnapshotFunc is called to stop the perf snapshot if provided
type StopSnapshotFunc func() error

// ProfileMetadata allows setting metadata for how a snapshot of a
// profile is taken. ProfileMetadata need only be provided for a
// registered profile if sleeping is required, or a sample rate must
// be set before doing the snapshot. For non-pprof Profiles, it must
// be provided (see "cpu").
type ProfileMetadata struct {
	Name                 string
	DefaultSampleRate    int
	DefaultSleepDuration time.Duration
	SetSampleRate        SetSampleRateFunc
	StartSnapshot        StartSnapshotFunc
	StopSnapshot         StopSnapshotFunc
}

var defaultProfileMetadatas = []ProfileMetadata{
	{
		Name:                 "cpu",
		DefaultSleepDuration: defaultDuration,
		StartSnapshot:        pprof.StartCPUProfile,
		StopSnapshot: func() error {
			pprof.StopCPUProfile()
			return nil
		},
	},
	{
		Name:              "block",
		DefaultSampleRate: 1,
		SetSampleRate: func(rate int) int {
			runtime.SetBlockProfileRate(rate)
			return -1
		},
	},
	{
		Name:              "mutex",
		DefaultSampleRate: 1,
		SetSampleRate:     runtime.SetMutexProfileFraction,
	},
}

var profileMetadata map[string]ProfileMetadata
var profileMetadataLock sync.Mutex

// RegisterProfileMetadata registers the metadata used to describe
// how a profile snapshot is to be taken for a given profile name.
func RegisterProfileMetadata(m ProfileMetadata) {
	profileMetadataLock.Lock()
	defer profileMetadataLock.Unlock()
	if profileMetadata == nil {
		profileMetadata = make(map[string]ProfileMetadata)
	}
	profileMetadata[m.Name] = m
}

func getProfileMetadata(name string) (ProfileMetadata, bool) {
	if name == "" {
		name = "cpu"
	}
	profileMetadataLock.Lock()
	defer profileMetadataLock.Unlock()
	m, found := profileMetadata[name]
	return m, found
}

func init() {
	for _, p := range defaultProfileMetadatas {
		RegisterProfileMetadata(p)
	}
}
