package tracing

import (
	log "github.com/sirupsen/logrus"
)

// https://github.com/hellofresh/janus/blob/73684a7fb3f16de52755c653b7c3c258cbca9450/pkg/opentracing/jaeger_log.go
type jaegerLoggerAdapter struct {
	log *log.Logger
}

func (l jaegerLoggerAdapter) Error(msg string) {
	l.log.Error(msg)
}

// Infof adapts infof messages to logrus
func (l jaegerLoggerAdapter) Infof(msg string, args ...interface{}) {
	l.log.Infof(msg, args...)
}
