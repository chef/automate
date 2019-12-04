package shared

import (
	"fmt"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/lib/stringutils"
)

// PreparedSystemConfig is a struct that can be rendered and used as a services
// system configuration.
type PreparedSystemConfig = interface{}

// PlatformServiceConfigurable is an interface that must be implemented by ConfigRequests
// that want to be part of the platform. It is used to setup the configuration for services
type PlatformServiceConfigurable interface {
	SetGlobalConfig(*GlobalConfig)
	PrepareSystemConfig(*TLSCredentials) (PreparedSystemConfig, error)
	Validate() error
}

type ProductConfig struct {
	Products []string
}

type PlatformServiceConfigurableV2 interface {
	PlatformServiceConfigurable
	ConfigureProduct(*ProductConfig)
}

// InvalidConfigError is an aggregate error containing missing keys in service
// ConfigRequest or AutomateConfig's.
type InvalidConfigError struct {
	// Key -> Reason
	invalidValues map[string]string
	deprecations  map[string]string
	missingKeys   []string
	unknownErrors []error
}

type Error interface {
	error
	AddMissingKey(string)
	MissingKeys() []string
	AddInvalidValue(string, string)
	InvalidValues() map[string]string
	AddUnknownError(error)
	UnknownErrors() []error
	AddDeprecation(string, string)
	Deprecations() map[string]string
	IsEmpty() bool
}

// NewInvalidConfigError returns a new instance of InvalidConfigError with zero values.
func NewInvalidConfigError() *InvalidConfigError {
	return &InvalidConfigError{
		invalidValues: map[string]string{},
		deprecations:  map[string]string{},
		missingKeys:   []string{},
		unknownErrors: []error{},
	}
}

// AddMissingKey adds a missing key to the error.
func (e *InvalidConfigError) AddMissingKey(k string) {
	e.missingKeys = append(e.missingKeys, k)
}

func (e *InvalidConfigError) MissingKeys() []string {
	return e.missingKeys
}

// AddInvalidValue takes a key and an invalid message.
func (e *InvalidConfigError) AddInvalidValue(k, m string) {
	e.invalidValues[k] = m
}

func (e *InvalidConfigError) InvalidValues() map[string]string {
	return e.invalidValues
}

// AddUnknownError adds and error to the unknown errors
func (e *InvalidConfigError) AddUnknownError(err error) {
	e.unknownErrors = append(e.unknownErrors, err)
}

func (e *InvalidConfigError) UnknownErrors() []error {
	return e.unknownErrors
}

// AddDeprecation takes an invalid key and a correction message
func (e *InvalidConfigError) AddDeprecation(k, m string) {
	e.deprecations[k] = m
}

func (e *InvalidConfigError) Deprecations() map[string]string {
	return e.deprecations
}

func (e *InvalidConfigError) IsEmpty() bool {
	return len(e.MissingKeys()) == 0 && len(e.InvalidValues()) == 0 && len(e.UnknownErrors()) == 0 && len(e.Deprecations()) == 0
}

// Error returns the error message as a string.
func (e *InvalidConfigError) Error() string {
	msg := strings.Builder{}

	if len(e.MissingKeys()) > 0 {
		// nolint errcheck
		msg.WriteString("\nConfig missing required keys:\n\t" + strings.Join(e.MissingKeys(), "\n\t"))
	}

	for key, reason := range e.InvalidValues() {
		// nolint errcheck
		msg.WriteString(fmt.Sprintf("\nConfiguration key '%s' has invalid value: %s\n", key, reason))
	}

	for key, remedy := range e.Deprecations() {
		// nolint errcheck
		msg.WriteString(fmt.Sprintf("\nConfiguration key '%s' has been deprecated and is no longer allowed. %s\n", key, remedy))
	}

	for _, err := range e.UnknownErrors() {
		msg.WriteString(fmt.Sprintf("Validation error: %s\n", err.Error()))
	}

	return msg.String()
}

// Validate calls validator functions and aggregates the errors into a single
// single InvalidConfigError.
func Validate(vs ...error) error {
	cfgErr := NewInvalidConfigError()

	for _, e := range vs {
		if e == nil {
			continue
		}

		err, ok := e.(Error)
		if !ok {
			cfgErr.AddUnknownError(e)
			continue
		}

		for k, v := range err.InvalidValues() {
			cfgErr.AddInvalidValue(k, v)
		}

		for _, v := range err.MissingKeys() {
			cfgErr.AddMissingKey(v)
		}

		for k, v := range err.Deprecations() {
			cfgErr.AddDeprecation(k, v)
		}
	}

	if cfgErr.IsEmpty() {
		return nil
	}

	return cfgErr
}

// NginxLogLevels contains the log levels accepted by nginx according
// to:
//    http://nginx.org/en/docs/ngx_core_module.html#error_log
var NginxLogLevels = []string{
	"debug",
	"info",
	"notice",
	"warn",
	"error",
	"crit",
	"alert",
	"emerg",
}

// LogrusLogLevels constains the log levels accepted by the Golang
// logrus library. Defined here:
//
// https://github.com/chef/automate/blob/master/vendor/github.com/sirupsen/logrus/logrus.go#L16-L33
var LogrusLogLevels = []string{
	"trace",
	"debug",
	"info",
	"warning",
	"error",
	"fatal",
	"panic",
}

// ZapLogLevels contains the log levels accepted by the Golang Zap
// library. Defined here:
//
// https://github.com/chef/automate/blob/master/vendor/go.uber.org/zap/level.go#L28-L47
var ZapLogLevels = []string{
	"debug",
	"info",
	"warn",
	"error",
	"dpanic",
	"panic",
	"fatal",
}

func ValidateNginxLogLevel(level string) error {
	if !stringutils.SliceContains(NginxLogLevels, level) {
		return errors.Errorf("invalid log level: %s (expected one of: %s)", level,
			strings.Join(NginxLogLevels, ", "))
	}

	return nil
}

func ValidateLogrusLogLevel(level string) error {
	if !stringutils.SliceContains(LogrusLogLevels, level) {
		return errors.Errorf("invalid log level: %s (expected one of: %s)", level,
			strings.Join(LogrusLogLevels, ", "))
	}

	return nil
}

func ValidateZapLogLevel(level string) error {
	if !stringutils.SliceContains(ZapLogLevels, level) {
		return errors.Errorf("invalid log level: %s (expected one of: %s)", level,
			strings.Join(ZapLogLevels, ", "))
	}

	return nil
}

// Convert the accepted GlobalLogLevels to a log level accepted by
// nginx's error_log setting.
func GlobalLogLevelToNginxLevel(level string) string {
	switch level {
	case "debug", "info", "error":
		return level
	case "warning":
		return "warn"
	case "panic", "fatal":
		return "crit"
	default:
		return "error"
	}
}

// Convert the accepted GlobalLogLevels to a log level accepted by
// Golang's ZAP logger
func GlobalLogLevelToZapLevel(level string) string {
	switch level {
	case "debug", "info", "error", "fatal", "panic":
		return level
	case "warning":
		return "warn"
	default:
		return "info"
	}
}
