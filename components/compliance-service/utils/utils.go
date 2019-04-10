package utils

import (
	"fmt"
	"net"
	"regexp"
	"strings"
	"time"

	"database/sql"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/lib/pq"
	"github.com/olivere/elastic"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// ProcessSQLNotFound checks for err of type sql.ErrNoRows and returns NotFoundError if true,
// otherwise wrap original error and return
func ProcessSQLNotFound(err error, id string, wrap string) error {
	if err == sql.ErrNoRows {
		return &NotFoundError{Id: id}
	}
	// unknown other type of error
	return errors.Wrap(err, wrap)
}

// NotFoundError is the error returned when nothing was found
type NotFoundError struct {
	underlying error
	Id         string
}

func (e *NotFoundError) Error() string {
	if e.underlying != nil {
		return fmt.Sprintf("Not found for id: %s - %s", e.Id, e.underlying.Error())
	}
	return fmt.Sprintf("Not found for id: %s", e.Id)
}

// ProcessNotFound returns NotFoundError
func ProcessNotFound(err error, id string) error {
	return &NotFoundError{Id: id}
}

// ProcessUnauthenticated returns UnauthenticatedError
func ProcessUnauthenticated(err error, msg string) error {
	return &UnauthenticatedError{underlying: err, Msg: msg}
}

// UnauthenticatedError is the error returned when user is not authenticated with an external service
// such as aws or azure
type UnauthenticatedError struct {
	underlying error
	Msg        string
}

func (e *UnauthenticatedError) Error() string {
	if e.underlying != nil {
		return e.Msg + ": " + e.underlying.Error()
	}
	return e.Msg
}

// ProcessInvalid returns InvalidError
func ProcessInvalid(err error, msg string) error {
	return &InvalidError{underlying: err, Msg: msg}
}

// InvalidError is the error returned when user has provided invalid arguments
type InvalidError struct {
	underlying error
	Msg        string
}

func (e *InvalidError) Error() string {
	if e.underlying != nil {
		return e.Msg + ": " + e.underlying.Error()
	}
	return e.Msg
}

// Error codes can be found here: https://github.com/grpc-ecosystem/grpc-gateway/blob/master/runtime/errors.go#L15
// FormatErrorMsg takes a message and a string id to return a unified error msg across requests
func FormatErrorMsg(err error, id string) error {
	if err == nil {
		return nil
	}
	logrus.Error(fmt.Sprintf("%s:%s", time.Now().UTC(), err.Error()))
	return checkErrorMsg(err, id)
}

func checkErrorMsg(err error, id string) error {
	err = errors.Cause(err)
	if _, ok := err.(*NotFoundError); ok {
		return status.Error(codes.NotFound, err.Error())
	}

	if _, ok := err.(*UnauthenticatedError); ok {
		// unauthenticated is used when the credentials provided for a cloud provider or other
		// external service are insufficient or invalid
		return status.Error(codes.PermissionDenied, err.Error())
	}

	if _, ok := err.(*InvalidError); ok {
		return status.Error(codes.InvalidArgument, err.Error())
	}

	if _, ok := err.(*net.OpError); ok {
		return status.Error(codes.Unavailable, "postgres not available")
	}

	if err == sql.ErrNoRows {
		return status.Error(codes.NotFound, "Not found for id: "+id)
	}

	if e, ok := err.(*pq.Error); ok {
		// defined in https://github.com/lib/pq/blob/88edab0803230a3898347e77b474f8c1820a1f20/error.go#L78
		switch e.Code.Name() {
		case "undefined_table":
			return status.Error(codes.Unavailable, "Postgres table not found.")
		}
	}

	if elastic.IsNotFound(err) {
		return status.Error(codes.NotFound, "Not found for id: "+id)
	}
	return status.Error(codes.Unknown, err.Error())
}

//DiffArray - A util func that takes two string arrays (a and b), and returns an array containing elements that are in a but not b
func DiffArray(a, b []string) (inANotB []string) {
	mapOfB := map[string]bool{}
	for _, elementB := range b {
		mapOfB[elementB] = true
	}
	inANotB = []string{}
	for _, elementA := range a {
		if _, ok := mapOfB[elementA]; !ok {
			inANotB = append(inANotB, elementA)
		}
	}
	return
}

// Ensuring it only has valid UUID characters
// TODO: unit tests
func IsSafeUUID(uuid string) bool {
	r := regexp.MustCompile("^[a-fA-F0-9-]{36}$")
	return r.MatchString(uuid)
}

// Ensuring str only has the characters we support at the moment
// TODO: unit tests
func IsSqlSafe(str string) bool {
	r := regexp.MustCompile("^[a-zA-Z0-9\\._-]*$")
	return r.MatchString(str)
}

func KvMatches(matchKey string, matchVal string, kv *common.Kv) bool {
	if kv.Key != matchKey {
		return false
	}
	/*
		- foo* -> HasPrefix
		- *foo -> HasSuffix
		- *foo* -> Contains
		- foo -> Exact match
	*/
	if strings.HasPrefix(matchVal, "*") && strings.HasSuffix(matchVal, "*") {
		matchVal = strings.TrimPrefix(matchVal, "*")
		matchVal = strings.TrimSuffix(matchVal, "*")
		return strings.Contains(kv.Value, matchVal)
	}
	if strings.HasSuffix(matchVal, "*") {
		matchVal = strings.TrimSuffix(matchVal, "*")
		return strings.HasPrefix(kv.Value, matchVal)
	}
	if strings.HasPrefix(matchVal, "*") {
		matchVal = strings.TrimPrefix(matchVal, "*")
		return strings.HasSuffix(kv.Value, matchVal)
	}
	return kv.Value == matchVal
}

func EscapeLiteralForPG(value string) string {
	value = strings.Replace(value, "'", "''", -1)
	value = strings.Replace(value, `\`, `\\`, -1)
	return value
}

func EscapeLiteralForPGPatternMatch(value string) string {
	value = EscapeLiteralForPG(value)
	value = strings.Replace(value, `_`, `\_`, -1)
	value = strings.Replace(value, `%`, `\%`, -1)
	return value
}

func UniqueStringSlice(stringSlice []string) []string {
	keys := make(map[string]bool)
	list := []string{}
	for _, entry := range stringSlice {
		if _, value := keys[entry]; !value {
			keys[entry] = true
			list = append(list, entry)
		}
	}
	return list
}
