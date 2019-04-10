package debug

import (
	"context"
	"io"
	"runtime"
	"runtime/pprof"
	"runtime/trace"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	duration "github.com/golang/protobuf/ptypes/duration"

	api "github.com/chef/automate/lib/grpc/debug/debug_api"
	"github.com/chef/automate/lib/io/chunks"
	"github.com/chef/automate/lib/version"
)

const defaultMaxChunkSize = (1 << 20) // 1MB
const defaultMaxDuration = 5 * time.Minute
const defaultDuration = 30 * time.Second

type debugServer struct {
	maxDuration time.Duration
	sleepFunc   func(time.Duration)
	versionInfo versionInfo
}

type ServerOpt func(*debugServer)

type versionInfo struct {
	gitRef  string
	version string
}

// WithVersionInfo sets the gitRef and release that will be returned
// by the debug version endpoint.
func WithVersionInfo(version, gitRef string) ServerOpt {
	return func(d *debugServer) {
		d.versionInfo = versionInfo{
			gitRef:  gitRef,
			version: version,
		}
	}
}

// NewDebugServer creates a grpc server that implements the Debug service
// interface
func NewDebugServer(opts ...ServerOpt) api.DebugServer {
	return newDebugServer(opts...)
}

func newDebugServer(opts ...ServerOpt) *debugServer {
	s := &debugServer{
		maxDuration: defaultMaxDuration,
		sleepFunc:   time.Sleep,
		versionInfo: versionInfo{
			gitRef:  version.GitSHA,
			version: version.Version,
		},
	}

	for _, o := range opts {
		o(s)
	}

	return s
}

type durationReq interface {
	GetDuration() *duration.Duration
}

func (s *debugServer) getDuration(req durationReq, defaultValue time.Duration) (time.Duration, error) {
	duration := defaultValue
	if req.GetDuration() != nil {
		var err error
		duration, err = ptypes.Duration(req.GetDuration())
		if err != nil {
			logrus.WithError(err).Error("Failed to parse duration")
			return 0, status.Error(codes.InvalidArgument, err.Error())
		}
	}

	if duration > s.maxDuration {
		logrus.WithFields(
			logrus.Fields{
				"duration":    duration.String(),
				"maxDuration": s.maxDuration.String(),
			}).Error("duration exceeds max")
		return 0, status.Errorf(codes.InvalidArgument, "duration exceeds max (%s > %s)",
			duration.String(), s.maxDuration.String())
	}
	return duration, nil
}

func (s *debugServer) Trace(req *api.TraceRequest, resp api.Debug_TraceServer) error {
	duration, err := s.getDuration(req, defaultDuration)
	if err != nil {
		return err
	}

	w := newChunkWriter(resp)

	logrus.Info("Starting trace")
	if err := trace.Start(w); err != nil {
		logrus.WithError(err).Error("Could not start execution tracing")
		return status.Error(codes.Unknown, "Could not start execution tracing")
	}

	logrus.WithField("duration", duration.String()).Info("tracing")
	s.sleepFunc(duration)

	logrus.Info("Stopping trace")
	trace.Stop()

	return nil
}

func (s *debugServer) Profile(req *api.ProfileRequest, resp api.Debug_ProfileServer) error {
	metadata, _ := getProfileMetadata(req.ProfileName)

	duration, err := s.getDuration(req, metadata.DefaultSleepDuration)
	if err != nil {
		return err
	}

	if metadata.SetSampleRate != nil {
		sampleRate := int(req.SampleRate)
		if sampleRate == 0 {
			sampleRate = metadata.DefaultSampleRate
		}
		logrus.WithField("rate", sampleRate).Info("Setting sample rate")
		nextSampleRate := metadata.SetSampleRate(sampleRate)
		defer func() {
			logrus.WithField("rate", nextSampleRate).Info("Resetting sample rate")
			metadata.SetSampleRate(nextSampleRate)
		}()
	}

	w := newChunkWriter(resp)
	if metadata.StartSnapshot == nil {
		// If a way to start a snapshot is not provided, we will lookup
		// up the profile from pprof and use that
		profile := pprof.Lookup(req.ProfileName)
		if profile == nil {
			logrus.WithField("profile", req.ProfileName).Error("Profile not found")
			return status.Errorf(codes.NotFound, "Profile '%s' not found", req.ProfileName)
		}
		logrus.Info("Writing profile snapshot")
		if err := profile.WriteTo(w, 0); err != nil {
			logrus.WithError(err).Error("Failed to start profile snapshot")
			return status.Error(codes.Unknown, "Failed to start profile snapshot")
		}
	} else {
		logrus.Info("Starting profile")
		if err := metadata.StartSnapshot(w); err != nil {
			logrus.WithError(err).Error("Failed to start cpu profile snapshot")
			return status.Error(codes.Unknown, "Failed to start cpu profile snapshot")
		}
	}
	s.sleepFunc(duration)

	if metadata.StopSnapshot != nil {
		logrus.Info("Stopping profile")
		if err := metadata.StopSnapshot(); err != nil {
			logrus.WithError(err).Error("Failed to stop snapshot")
			return status.Error(codes.Unknown, "Failed to stop snapshot")
		}
	}
	return nil
}

func (*debugServer) SetLogLevel(ctx context.Context, req *api.SetLogLevelRequest) (*api.SetLogLevelResponse, error) {
	switch req.GetLevel() {
	case api.SetLogLevelRequest_DEBUG:
		logrus.SetLevel(logrus.DebugLevel)
	case api.SetLogLevelRequest_INFO:
		logrus.SetLevel(logrus.InfoLevel)
	case api.SetLogLevelRequest_WARN:
		logrus.SetLevel(logrus.WarnLevel)
	case api.SetLogLevelRequest_FATAL:
		logrus.SetLevel(logrus.FatalLevel)
	default:
		return nil, status.Error(codes.InvalidArgument, "invalid log level")
	}

	return &api.SetLogLevelResponse{}, nil
}

func (s *debugServer) GetVersion(ctx context.Context, _ *api.VersionRequest) (*api.VersionResponse, error) {
	return &api.VersionResponse{
		GitRef:    s.versionInfo.gitRef,
		Version:   s.versionInfo.version,
		GoVersion: runtime.Version(),
	}, nil
}

type chunkSender interface {
	Send(*api.Chunk) error
}

func newChunkWriter(sender chunkSender) io.Writer {
	return chunks.NewWriter(defaultMaxChunkSize, func(p []byte) error {
		logrus.Infof("sending chunk of size %d", len(p))
		return sender.Send(&api.Chunk{
			Chunk: p,
		})
	})
}
