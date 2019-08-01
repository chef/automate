package utils

import (
	"regexp"
	"strings"

	"github.com/chef/automate/components/compliance-service/api/common"
)

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

//remove duplicate entries from a string array
func deDupSlice(stringSlice []string) []string {
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

//Take out the duplicate entries if they exist.. not need to return the map.. maps are passed by ref.
// getting the duplicates out of the filters avoids the filtersQuery from becoming less efficient by having
// duplicate clauses.
// Duplicates in filter entries also make deep filtering impossible because deep filtering
// at the moment requires that we have exactly one profile and no controls or one profile and one control. If we have
// duplicates, it's easy to see why deep filtering would therefore, break.
func DeDupFilters(filters map[string][]string) {
	for key, value := range filters {
		if len(value) > 1 {
			filters[key] = deDupSlice(value)
		}
	}
}
