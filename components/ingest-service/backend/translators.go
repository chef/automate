package backend

import log "github.com/sirupsen/logrus"

// EmptyStringIfNil asserts an interface as a string, and if that fails it returns empty string
func EmptyStringIfNil(attribute interface{}) string {
	if v, ok := attribute.(string); ok {
		return v
	} // captures the nil case, too
	return ""
}

// InterfaceArrayToStringArray Converts an array of interfaces into an array of strings
func interfaceArrayToStringArray(fromInterface []interface{}) []string {
	aString := make([]string, len(fromInterface))
	for i, v := range fromInterface {
		// Make sure we can make the type assertion
		if vStr, ok := v.(string); ok {
			aString[i] = vStr
		} else {
			log.WithFields(log.Fields{
				"value":   v,
				"error":   "Unable to transform 'interface{}' into 'string'.",
				"default": "",
			}).Debug("setting default")
		}
	}
	return aString
}

// extractMapOrEmpty Tries to extract the provided key from the attributes map array
// as a Map of Interfaces, if it can't cast it, it will return an empty object
func extractMapOrEmpty(attrs map[string]interface{}, key string) map[string]interface{} {
	found, ok := attrs[key].(map[string]interface{})
	if !ok {
		log.WithFields(log.Fields{
			"attribute": key,
			"default":   "[]",
		}).Debug("Attribute not found; setting default")
		return map[string]interface{}{}
	}
	return found
}

// extractArrayOrEmpty Tries to extract the provided key from the attributes map array
// as an Array of Interfaces, if it can't cast it, it will return an empty object
func extractArrayOrEmpty(attrs map[string]interface{}, key string) []interface{} {
	found, ok := attrs[key].([]interface{})
	if !ok {
		log.WithFields(log.Fields{
			"attribute": key,
			"default":   "[]",
		}).Debug("Attribute not found; setting default")
		return []interface{}{}
	}
	return found
}
