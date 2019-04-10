// policy_v2 is a copy of the v1 policy code from
// components/automate-gateway/api/authz/policy/policy.go
package policy_v2

import (
	"regexp"
	"strings"

	"github.com/chef/automate/components/automate-gateway/api/authz/pairs"
)

// ExpansionFunc dictates how a parameter is filled in to a resource placeholder.
type ExpansionFunc func(pat string, req interface{}) string

func ExpandParameterizedResource(s string, mapping func(string) string) string {
	var expanded []string
	for _, t := range strings.Split(s, ":") {
		if isParameter(t) {
			expanded = append(expanded, mapping(extractParameter(t)))
		} else {
			expanded = append(expanded, t)
		}
	}
	return strings.Join(expanded, ":")
}

func isParameter(s string) bool {
	return paramRegexp.MatchString(s)
}

func extractParameter(s string) string {
	return strings.TrimSuffix(strings.TrimPrefix(s, "{"), "}")
}

// InfoForMethod retrieves metadata for an API method.
// The second parameter is used to fill in a placeholder in the resource
// (though not all resources will have placeholders).
func InfoForMethod(methodName string, in interface{}) *pairs.Info {
	info, ok := methodToInfo[strings.TrimSpace(methodName)]
	if !ok {
		return nil
	}
	return &pairs.Info{
		Resource:     info.expandedResource(in),
		Action:       info.action,
		HTTPMethod:   info.httpMethod,
		HTTPEndpoint: info.httpEndpoint,
	}
}

// MapMethodTo records details about API methods at startup in a central repository
// to allow convenient access via InfoForMethod.
func MapMethodTo(methodName, resource, action, httpMethod, httpEndpoint string, expandFunc ExpansionFunc) {
	methodToInfo[strings.TrimSpace(methodName)] = info{
		unexpandedResource: strings.TrimSpace(resource),
		action:             strings.TrimSpace(action),
		httpMethod:         strings.TrimSpace(httpMethod),
		httpEndpoint:       strings.TrimSpace(httpEndpoint),
		expandWith:         expandFunc,
	}
}

// GetInfoMap returns a map of API method names to method metadata.
func GetInfoMap() map[string]pairs.Info {
	methodMap := make(map[string]pairs.Info)
	for key, value := range methodToInfo {
		methodMap[key] = toInfo(value)
	}
	return methodMap
}

// Reset initializes the state of Policy.
// This may not be needed other than for unit tests, but is vital
// for that purpose to be able to start with a clean slate for each test.
func Reset() {
	methodToInfo = make(map[string]info)
}

func (i *info) expandedResource(in interface{}) string {
	if nil == i.expandWith {
		return strings.TrimSpace(i.unexpandedResource)
	}
	return i.expandWith(strings.TrimSpace(i.unexpandedResource), in)
}

type info struct {
	unexpandedResource string
	action             string
	httpMethod         string
	httpEndpoint       string
	expandWith         ExpansionFunc
}

func toInfo(policy info) pairs.Info {
	return pairs.Info{
		Resource:     policy.unexpandedResource,
		Action:       policy.action,
		HTTPMethod:   policy.httpMethod,
		HTTPEndpoint: policy.httpEndpoint,
	}
}

var methodToInfo = make(map[string]info)
var paramRegexp = regexp.MustCompile(`^{[a-zA-Z]\w*}$`)
