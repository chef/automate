//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package backend

import (
	"reflect"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// Exposing the following set of variables that consumers
// might use to map their parameters to the matching param
// our backend
var (
	NodeType                  = reflect.TypeOf(Node{})
	Id                 string = NodeFieldToJson("EntityUuid")
	Name               string = NodeFieldToJson("NodeName")
	Organization       string = NodeFieldToJson("OrganizationName")
	Cookbook           string = "cookbooks"
	Recipe             string = "recipes"
	ResourceName       string = "resource_names"
	Attribute          string = "attributes"
	Role               string = "roles"
	Environment        string = "environment"
	PolicyRevision     string = NodeFieldToJson("PolicyRevision")
	PolicyName         string = NodeFieldToJson("PolicyName")
	PolicyGroup        string = NodeFieldToJson("PolicyGroup")
	ActionSourceFQDN   string = "service_hostname"
	ActionOrganization string = "organization_name"
	CheckIn            string = NodeFieldToJson("Checkin")
	LastCCRReceived    string = NodeFieldToJson("LastCCRReceived")
	UptimeSeconds      string = NodeFieldToJson("UptimeSeconds")
	Platform           string = NodeFieldToJson("Platform")
	ChefVersion        string = "chef_version"
	ChefTags           string = "chef_tags"
)

// NodeFieldToJson returns the value associated with key in the tag
// string of the `Node` struct. We are using this method to be able
// to convert parameters that consumers provides into backend format
func NodeFieldToJson(field string) (json string) {
	param, found := NodeType.FieldByName(field)
	if found {
		json = param.Tag.Get("json")
	}
	return
}

// SuggestionFieldArray returns true if the provided field/term is a suggestion type Array.
// otherwise it returns false for string type.
//
// It matters if the field could be an array or just a single string value, don't
// forget to add fields that are not string.
func SuggestionFieldArray(field string) bool {
	switch field {
	case Cookbook, Recipe, ResourceName, Attribute, Role, ChefTags:
		return true
	default:
		return false
	}
}

// GetSortableFieldValue Get the value of a node object from its field name.
func GetSortableFieldValue(fieldName string, node Node) (interface{}, error) {
	switch fieldName {
	case CheckIn:
		return node.Checkin, nil
	case LastCCRReceived:
		return node.LastCCRReceived, nil
	case Name:
		return node.NodeName, nil
	case Organization:
		return node.OrganizationName, nil
	case UptimeSeconds:
		return node.UptimeSeconds, nil
	case Platform:
		return node.Platform, nil
	case Environment:
		return node.Environment, nil
	default:
		return nil, status.Errorf(codes.InvalidArgument, "Can not sort by field %s", fieldName)
	}
}
