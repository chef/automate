package logger_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"

	"github.com/chef/automate/lib/logger"
)

// TestGRPCLogCompatZap asserts that grpclog's verbosity check returns the
// proper results given the configured log level with Zap.
func TestGRPCLogCompatZap(t *testing.T) {
	levelsWeCareAbout := []string{"debug", "info", "warn", "error"}
	outputInLoggerLevel := [...]map[string]bool{
		// 0 is always output
		map[string]bool{"debug": true, "info": true, "warn": true, "error": true},
		// 1 is always output
		map[string]bool{"debug": true, "info": true, "warn": true, "error": true},
		// 2 and 3 are only output for debug
		map[string]bool{"debug": true},
		map[string]bool{"debug": true},
	}
	// grpc verbosity levels
	for lvl := 0; lvl <= 3; lvl++ {
		t.Run(fmt.Sprintf("V(%d)", lvl), func(t *testing.T) {
			for _, loggerLvl := range levelsWeCareAbout {
				t.Run(fmt.Sprintf("level %s", loggerLvl), func(t *testing.T) {
					expected := outputInLoggerLevel[lvl][loggerLvl]
					l, err := newLogger(loggerLvl)
					require.NoError(t, err)
					lw := logger.WrapZapGRPC(l)
					assert.Equalf(t, expected, lw.V(lvl), "level %q: expected V(%d) to be %v", loggerLvl, lvl, expected)
				})
			}
		})
	}
}

func newLogger(level string) (*zap.Logger, error) {
	cfg := zap.NewProductionConfig()
	cfg.EncoderConfig.EncodeTime = zapcore.ISO8601TimeEncoder

	switch strings.ToLower(level) {
	case "debug":
		cfg.Level.SetLevel(zap.DebugLevel)
	case "", "info":
		cfg.Level.SetLevel(zap.InfoLevel)
	case "error":
		cfg.Level.SetLevel(zap.ErrorLevel)
	case "warn":
		cfg.Level.SetLevel(zap.WarnLevel)
	default:
		return nil, fmt.Errorf("bad log level: %q", level)
	}

	return cfg.Build()
}
