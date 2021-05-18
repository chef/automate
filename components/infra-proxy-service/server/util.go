package server

import (
	"fmt"
	"reflect"
	"strconv"
	"strings"

	"github.com/chef/automate/api/interservice/infra_proxy/response"
	chef "github.com/go-chef/chef"
)

// SafeStringFromMap returns the value referenced by `key` in `values`. as a string.
// If not found, it returns an empty string.
func SafeStringFromMap(values map[string]interface{}, key string) string {
	if values[key] == nil {
		return ""
	}
	return values[key].(string)
}

// SafeStringFromMapFloat returns the value referenced by `key` in `values`. as a string (after first formatting as a base 64 float).
// If not found, it returns an empty string.
func SafeStringFromMapFloat(values map[string]interface{}, key string) string {
	if values[key] == nil {
		return ""
	}
	return strconv.FormatFloat(values[key].(float64), 'E', -1, 64)
}

// SafeStringFromMap returns the value referenced by `key` in `values`. as a boolean.
// If not found, it returns a boolean false.
func SafeBooleanFromMap(values map[string]interface{}, key string) bool {
	if values[key] == nil {
		return false
	}
	return values[key].(bool)
}

// SafeSliceFromMap returns the value referenced by `key` in `values`. as a slice.
// If not found, it returns an empty slice.
func SafeSliceFromMap(values map[string]interface{}, key string) []string {
	value := reflect.ValueOf(values[key])
	switch value.Kind() {
	case reflect.Slice:
		t := make([]string, value.Len())
		for i := 0; i < value.Len(); i++ {
			t[i] = fmt.Sprint(value.Index(i))
		}
		return t
	}

	return []string{}
}

// SubtractSlice returns the slice a with all elements of b removed.
func SubtractSlice(a []string, b []string) []string {
	for _, element := range b {
		a = RemoveElement(a, element)
	}

	return a
}

// RemoveElement removes the element from slice.
func RemoveElement(a []string, str string) []string {
	for i := range a {
		if a[i] == str {
			return append(a[:i], a[i+1:]...)
		}
	}

	return a
}

// Unique returns a slice with duplicate values removed.
func Unique(a []string) []string {
	seen := make(map[string]struct{})

	b := a[:0]
	for _, v := range a {
		if _, ok := seen[v]; !ok {
			seen[v] = struct{}{}
			b = append(b, v)
		}
	}

	return b
}

func ToResponseExpandedRunList(client *ChefClient, runlist []string, cookbooks chef.EnvironmentCookbookResult, runlistCache RunListCache) ([]*response.RunList, error) {
	resRunList := make([]*response.RunList, len(runlist))
	var pos int32
	for i, item := range runlist {
		newItem, err := chef.NewRunListItem(item)
		if err != nil {
			return nil, err
		}
		newRunList := response.RunList{
			Type: newItem.Type,
			Name: newItem.Name,
		}

		if newItem.IsRecipe() {
			newRunList.Version = newItem.Version
			if newRunList.Version == "" {
				cookbookVersion := cookbooks[strings.Split(newItem.Name, "::")[0]]
				if len(cookbookVersion.Versions) > 0 {
					newRunList.Version = cookbookVersion.Versions[0].Version
				}
			}

			if runlistCache[newItem.Type] != nil {
				if runlistCache[newItem.Type][newItem.Name] {
					newRunList.Skipped = true
					newRunList.Position = -1
				}
				runlistCache[newItem.Type][newItem.Name] = true
			} else {
				runlistCache[newItem.Type] = map[string]bool{newItem.Name: true}
			}

			if !newRunList.Skipped {
				newRunList.Position = pos
				pos++
			}
		}

		if newItem.IsRole() {
			newRunList.Position = -1 // Ignore the position for a role.
			currentRole, err1 := client.client.Roles.Get(newItem.Name)
			chefError, _ := chef.ChefError(err1)
			if chefError != nil {
				newRunList.Error = chefError.StatusMsg()
			} else {
				if runlistCache[newItem.Type] != nil {
					if !runlistCache[newItem.Type][newItem.Name] {
						runlistCache[newItem.Type][newItem.Name] = true
						children, err := ToResponseExpandedRunList(client, currentRole.RunList, cookbooks, runlistCache)
						newRunList.Children = children
						if err != nil {
							newRunList.Error = err.Error()
						}
					} else {
						newRunList.Skipped = true
					}
				} else {
					runlistCache[newItem.Type] = map[string]bool{newItem.Name: true}
					children, err := ToResponseExpandedRunList(client, currentRole.RunList, cookbooks, runlistCache)
					newRunList.Children = children
					if err != nil {
						newRunList.Error = err.Error()
					}
				}
			}
		}

		resRunList[i] = &newRunList
	}

	return resRunList, nil
}
