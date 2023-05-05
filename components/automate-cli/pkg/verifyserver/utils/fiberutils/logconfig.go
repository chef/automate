package fiberutils

import (
	"os"
	"strings"
	"time"

	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/middleware"
	"github.com/sirupsen/logrus"
)

const (
	VERIFY_SERVER_TIMEZONE = "VERIFY_SERVER_TIMEZONE"
	DEFAULT_TIMEZONE       = "UTC"
)

func GetLogConfig(log logger.Logger) (lc middleware.LoggerConfig) {
	timeZone := DEFAULT_TIMEZONE
	envTimeZone := os.Getenv(VERIFY_SERVER_TIMEZONE)
	if envTimeZone != "" {
		if _, err := time.LoadLocation(envTimeZone); err == nil {
			timeZone = envTimeZone
		}
	}
	log.Info("Using TimeZone: " + timeZone)
	lc = middleware.LoggerConfig{
		TimeFormat: time.RFC3339,
		TimeZone:   timeZone,
	}
	if log.NewEntry().Logger.GetLevel() <= logrus.DebugLevel {
		lc.Format = generateLogFormat(
			"${magenta}"+strings.ToUpper(log.NewEntry().Logger.GetLevel().String())+"${reset}",
			"time", "pid", "status", "method", "path", "latency", "error", "bytesReceived", "bytesSent", "body",
		)
	} else {
		lc.Format = generateLogFormat(
			"${green}"+strings.ToUpper(log.NewEntry().Logger.GetLevel().String())+"${reset}",
			"time", "pid", "status", "method", "path", "latency", "error", "bytesReceived", "bytesSent",
		)
	}
	return lc
}

func generateLogFormat(prefix string, fields ...string) string {
	color := []string{
		"red", "green", "yellow", "blue", "magenta", "cyan", "white",
	}
	logFormat := "| " + prefix + " | "
	for i, v := range fields {
		c := color[i%7]
		logFormat += "${" + c + "}${" + v + "}${reset}  |  "
	}
	logFormat += "\n"
	return logFormat
}
