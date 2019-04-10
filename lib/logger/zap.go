package logger

import (
	"strings"

	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"
	"google.golang.org/grpc/grpclog"
)

type zapWrap struct {
	*zap.SugaredLogger
}

func WrapZapGRPC(l *zap.Logger) grpclog.LoggerV2 {
	return &zapWrap{SugaredLogger: l.Sugar()}
}

func (z *zapWrap) Infoln(args ...interface{}) {
	z.Info(args...)
}
func (z *zapWrap) Errorln(args ...interface{}) {
	z.Error(args...)
}

func (z *zapWrap) Warning(args ...interface{}) {
	z.Warn(args...)
}

func (z *zapWrap) Warningf(f string, args ...interface{}) {
	z.Warnf(f, args...)
}

func (z *zapWrap) Warningln(args ...interface{}) {
	z.Warn(args...)
}

func (z *zapWrap) Fatalln(args ...interface{}) {
	z.Fatal(args...)
}

func (z *zapWrap) V(i int) bool {
	return z.Desugar().Core().Enabled(zapcore.DebugLevel) || i <= 1
}

// ParseZapEncoding takes a string and returns a valid Zap encoding,
// defaulting to "console".
func ParseZapEncoding(enc string) string {
	switch strings.ToLower(enc) {
	case "json":
		return "json"
	default:
		return "console"
	}
}

// ParseZapLevel takes a string and returns a zap.Level. It the
// level is unrecognized, it returns the default level.
func ParseZapLevel(level string) zapcore.Level {
	switch strings.ToLower(level) {
	case "debug":
		return zap.DebugLevel
	case "info":
		return zap.InfoLevel
	case "warn":
		return zap.WarnLevel
	case "error":
		return zap.ErrorLevel
	case "dpanic":
		return zap.DPanicLevel
	case "panic":
		return zap.PanicLevel
	case "fatal":
		return zap.FatalLevel
	default:
		return zap.InfoLevel
	}
}
