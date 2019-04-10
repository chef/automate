package logger_test

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/logger"
)

func TestFieldLogging(t *testing.T) {
	var buf bytes.Buffer
	l, err := logger.NewLoggerWithOut("text", "info", &buf)
	require.NoError(t, err, "NewLogger returns a logger")
	l.WithFields(logger.KV{
		"a": 1,
		"b": "two",
	}).Info("hello")

	got := buf.String()
	assert.Contains(t, got, "a=1")
	assert.Contains(t, got, "b=two")
	assert.Contains(t, got, "hello")
}

// TestGRPCLogCompat asserts that grpclog's verbosity check returns the
// proper results given the configured log level
func TestGRPCLogCompat(t *testing.T) {
	levelsWeCareAbout := []string{"debug", "info", "warn", "error"}
	outputInLoggerLevel := []map[string]bool{
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
					l, err := logger.NewLoggerWithOut("text", loggerLvl, ioutil.Discard)
					require.NoError(t, err)
					assert.Equalf(t, expected, l.V(lvl), "level %q: expected V(%d) to be %v", loggerLvl, lvl, expected)
				})
			}
		})
	}
}
