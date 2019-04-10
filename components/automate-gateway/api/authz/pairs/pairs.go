package pairs

import (
	"fmt"
	"regexp"
	"strings"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	gwAuthzRes "github.com/chef/automate/components/automate-gateway/api/authz/response"
)

// FindStringSubmatch returns an array.
// The first element is the whole match of the regexp.
// Subsequent elements are the capture groups, so starting at index 1.
// Similarly, FindAllStringSubmatch returns a similar array for EACH match.
const (
	regexpCaptureIndex = 1
	returnAllMatches   = -1
)

// With this regexp, then, the first element is the whole match, braces and content.
// The second element is the capture group. Here, that's between the braces.
var paramRegexp = regexp.MustCompile(`\{([a-zA-Z]\w*)\}`)

// Pair is a resource and an action
type Pair struct {
	Resource string `json:"resource,omitempty"`
	Action   string `json:"action,omitempty"`
}

// Info is a container for the metadata for an API method
type Info struct {
	Resource     string
	Action       string
	HTTPMethod   string
	HTTPEndpoint string
}

// InvertMapNonParameterized reverses the method map with a compound key of resource & action.
// That allows us to separate the http metadata from the resource/action pair,
// send the latter off to authz-service for processing, then upon return
// re-associate them to the one (or more) tuple of http data.
func InvertMapNonParameterized(inputMap map[string]Info) map[Pair][]string {
	invertMap := make(map[Pair][]string)
	for method, methodInfo := range inputMap {
		// Check the resource rather than the endpoint path, because the parameter may not appear
		// in the path for an HTTP POST method; it could be in the body.
		if !paramRegexp.MatchString(methodInfo.Resource) {
			invertKey := Pair{
				Resource: methodInfo.Resource,
				Action:   methodInfo.Action,
			}
			invertMap[invertKey] = append(invertMap[invertKey], method)
		}
	}
	return invertMap
}

// InvertMapParameterized reverses the method map with a compound key of resource & action.
func InvertMapParameterized(
	inputMap map[string]Info, path string, paramList []string) (map[Pair][]string, error) {

	invertMap := make(map[Pair][]string)
	for method, methodInfo := range inputMap {

		// Convert abstract path in policy to be able to extract param values from concrete path.
		// e.g. "/users/auth/{email}" => "/users/auth/([^/]*)"
		// (includes non-parameterized endpoints, too, i.e. those with no transform needed)
		reString := toPathComponentRegex(methodInfo.HTTPEndpoint)

		// Compile should never fail here because placeholders all come from
		// path specified in "option (google.api.http)" lines in *.proto files...
		// but check just in case
		re, err := regexp.Compile(reString)
		if err != nil {
			return nil, status.Errorf(codes.InvalidArgument, "invalid meta-char in path: %q", reString)
		}
		matches := re.FindStringSubmatch(path) // matches entire string

		if len(matches) > 0 {
			// Mirror the params into the abstract resource to make it concrete,
			// so it can then be passed to the OPA engine.
			// e.g. "users:{email}" => "users:foo@bar.com"

			paramMap := toMap(paramList)
			if len(paramMap) == 0 {
				paramMap = getParamMap(methodInfo.HTTPEndpoint, matches)
			}
			concreteResource, err := fillParametersResource(methodInfo.Resource, paramMap)
			if err != nil {
				return nil, err
			}

			invertKey := Pair{
				Resource: concreteResource,
				Action:   methodInfo.Action,
			}
			invertMap[invertKey] = append(invertMap[invertKey], method)
		}
	}
	return invertMap, nil
}

// GetKeys returns a list of keys of the map.
func GetKeys(mapByResourcePlusAction map[Pair][]string) []*Pair {

	pairs := make([]*Pair, len(mapByResourcePlusAction))
	i := 0
	for resourceAndAction := range mapByResourcePlusAction {
		pairs[i] = &Pair{
			Resource: resourceAndAction.Resource,
			Action:   resourceAndAction.Action,
		}
		i++
	}
	return pairs
}

// GetEndpointMapFromResponse returns a map whose keys are HTTP endpoints.
// The corresponding value is a sub-map of HTTP methods with boolean values indicating
// whether the user has permission to access that HTTP method on the HTTP endpoint.
func GetEndpointMapFromResponse(
	respPairs []*Pair,
	methodsInfo map[string]Info,
	mapByResourcePlusAction map[Pair][]string,
	fullyHydrate bool) (map[string]*gwAuthzRes.MethodsAllowed, error) {

	endpointMap := make(map[string]*gwAuthzRes.MethodsAllowed)

	// respPairs has just the allowed entries.
	// mapByResourcePlusAction contains all the requested entries.
	for _, p := range respPairs {
		for _, apiMethod := range mapByResourcePlusAction[*p] {
			metadata := methodsInfo[apiMethod]

			// Skip methods not exposed via HTTP
			if metadata.HTTPEndpoint == "" {
				continue
			}

			// Add one HTTP method to the results for a given HTTP endpoint.
			// Note that the response pairs are unordered so each one needs
			// to find its way to the right slot independently.
			// This initializes if not yet done, then accumulates one method at a time.
			if _, ok := endpointMap[metadata.HTTPEndpoint]; !ok {
				endpointMap[metadata.HTTPEndpoint] = &gwAuthzRes.MethodsAllowed{}
			}
			methodsAllowed, err := setAllowedMethod(
				endpointMap[metadata.HTTPEndpoint], metadata)
			if err != nil {
				return nil, err
			}
			endpointMap[metadata.HTTPEndpoint] = methodsAllowed
		}
	}

	// Now backfill any unsatisfied requests so deny shows up with all methods false
	// instead of not showing up at all.
	if fullyHydrate {
		for _, requestedPair := range mapByResourcePlusAction {
			for _, apiMethod := range requestedPair {
				metadata := methodsInfo[apiMethod]
				if metadata.HTTPEndpoint != "" {
					if _, ok := endpointMap[metadata.HTTPEndpoint]; !ok {
						endpointMap[metadata.HTTPEndpoint] = &gwAuthzRes.MethodsAllowed{}
					}
				}
			}
		}
	}

	return endpointMap, nil
}

func setAllowedMethod(allowed *gwAuthzRes.MethodsAllowed,
	method Info) (*gwAuthzRes.MethodsAllowed, error) {

	switch method.HTTPMethod {
	case "GET":
		allowed.Get = true
	case "PUT":
		allowed.Put = true
	case "POST":
		allowed.Post = true
	case "DELETE":
		allowed.Delete = true
	case "PATCH":
		allowed.Patch = true
	default:
		return nil, fmt.Errorf("unexpected method while parsing introspection: %+v", method)
	}
	return allowed, nil
}

func toPathComponentRegex(endpoint string) string {
	// The virgule (/) is the HTTP path separator; a parameter in the HTTP endpoint
	// must be wholly contained within a single path segment.
	s := paramRegexp.ReplaceAllString(endpoint, "([^/]*)")
	// Anchor so that we are matching the entire input.
	return "^" + s + "$"
}

func toMap(data []string) map[string]string {
	paramMap := make(map[string]string, len(data))
	for _, item := range data {
		splits := strings.Split(item, "=")
		if len(splits) == 2 && len(splits[0]) > 0 {
			paramMap[splits[0]] = splits[1]
		}
	}
	return paramMap
}

// Generate a map of parameter names (e.g. {email}) to parameter values (e.g. abc@foo.com)
// based on the supplied path.
func getParamMap(endpoint string, matches []string) map[string]string {
	// extract from the user-supplied path:
	paramValueMatches := matches[regexpCaptureIndex:] // just the capture groups
	// extract from the policy metadata:
	paramNameMatches := paramRegexp.FindAllStringSubmatch(endpoint, returnAllMatches)
	paramMap := make(map[string]string, len(paramNameMatches))

	// len(paramNameMatches) should always equal len(paramValueMatches)
	// or we would not have reached here
	for i := range paramNameMatches {
		paramMap[paramNameMatches[i][regexpCaptureIndex]] = paramValueMatches[i]
	}
	return paramMap
}

func fillParametersResource(resource string, paramMap map[string]string) (string, error) {
	for placeholder, value := range paramMap {
		// MustCompile should never fail here because placeholders all come from
		// path specified in "option (google.api.http)" lines in *.proto files
		re, err := regexp.Compile(`\{` + placeholder + `\}`)
		if err != nil {
			return "", status.Errorf(codes.InvalidArgument, "invalid input parameter: %q", placeholder)
		}
		if !re.MatchString(resource) {
			continue // ignore this parameter
		}
		if value == "" {
			return "", status.Errorf(codes.InvalidArgument, "parameter %q has empty value", placeholder)
		}
		resource = re.ReplaceAllString(resource, value)
	}
	if msg := missingParams(resource); msg != "" {
		return "", status.Errorf(codes.InvalidArgument, "missing parameter(s): %s", msg)
	}
	return resource, nil
}

func missingParams(resource string) string {
	missedParamMatches := paramRegexp.FindAllStringSubmatch(resource, -1)
	paramList := make([]string, len(missedParamMatches))
	for i, param := range missedParamMatches {
		paramList[i] = param[1]
	}
	return strings.Join(paramList, ", ")
}
