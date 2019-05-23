//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package backend

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"

	log "github.com/sirupsen/logrus"
)

// ChefClientRun is the struct representing a chef client converge message
type ChefClientRun struct {
	ChefServerFqdn       string               `json:"chef_server_fqdn"`
	EntityUUID           string               `json:"entity_uuid"`
	ExpandedRunList      ExpandedRunList      `json:"expanded_run_list"`
	ID                   string               `json:"id"`
	MessageVersion       string               `json:"message_version"`
	MessageType          string               `json:"message_type"`
	NodeName             string               `json:"node_name"`
	OrganizationName     string               `json:"organization_name"`
	Resources            []ChefClientResource `json:"resources"`
	RunID                string               `json:"run_id"`
	RunList              []string             `json:"run_list"`
	StartTime            time.Time            `json:"start_time"`
	EndTime              time.Time            `json:"end_time"`
	Source               string               `json:"source"`
	Status               string               `json:"status"`
	TotalResourceCount   int                  `json:"total_resource_count"`
	UpdatedResourceCount int                  `json:"updated_resource_count"`
	Error                ChefError            `json:"error,omitempty"`
	NodePayload          NodePayload          `json:"node"`
	PolicyName           string               `json:"policy_name"`
	PolicyGroup          string               `json:"policy_group"`
	Deprecations         []Deprecation        `json:"deprecations,omitempty"`
	Tags                 []string             `json:"tags,omitempty"`
}

// ChefClientResource represents a resource as reported from chef client
type ChefClientResource struct {
	Type            string              `json:"type"`
	Name            string              `json:"name"`
	ID              string              `json:"id"`
	Duration        string              `json:"duration"`
	Delta           string              `json:"delta"`
	Result          string              `json:"result"`
	Status          string              `json:"status"`
	IgnoreFailure   ignoreFailureOption `json:"ignore_failure,omitempty"`
	CookbookName    string              `json:"cookbook_name,omitempty"`
	CookbookVersion string              `json:"cookbook_version,omitempty"`
	CookbookType    string              `json:"cookbook_type,omitempty"`
	RecipeName      string              `json:"recipe_name,omitempty"`
	Conditional     string              `json:"conditional,omitempty"`
}

// NodePayload is the ohai information reported in a chef client converge message
type NodePayload struct {
	Name            string                 `json:"name"`
	ChefEnvironment string                 `json:"chef_environment"`
	PolicyName      string                 `json:"policy_name"`
	PolicyGroup     string                 `json:"policy_group"`
	RunList         []string               `json:"run_list"`
	Normal          map[string]interface{} `json:"normal"`
	Default         map[string]interface{} `json:"default"`
	Override        map[string]interface{} `json:"override"`
	Automatic       map[string]interface{} `json:"automatic"`
}

// ToNodeRun returns a run after transforming a ChefClientRun into a Run object
func (ccr *ChefClientRun) ToNodeRun() (run Run, err error) {
	if ccr.MessageType == "run_converge" { // Not currently handling run_start messages
		nodeInfo, nodeInfoErr := ccr.initializeNodeInfo()

		if nodeInfoErr != nil {
			err = nodeInfoErr
			return
		}

		run = Run{
			NodeInfo:             nodeInfo,
			RunID:                ccr.RunID,
			StartTime:            ccr.StartTime,
			EndTime:              ccr.EndTime,
			UpdatedResourceCount: ccr.UpdatedResourceCount,
			Resources:            ccr.initializeResources(),
		}
	}

	if ccr.Status == "failure" {
		run.Error = ccr.Error
	}

	return
}

// ToNode Returns a Node after transforming a ChefClientRun into a Node object
func (ccr *ChefClientRun) ToNode() (nodeState Node, err error) {
	if ccr.MessageType == "run_converge" { // Not currently handling run_start messages
		nodeInfo, nodeInfoErr := ccr.initializeNodeInfo()

		if nodeInfoErr != nil {
			err = nodeInfoErr
			return
		}
		hasDeprecations := false
		if len(ccr.Deprecations) > 0 {
			hasDeprecations = true
		}
		nodeState = Node{
			NodeInfo:          nodeInfo,
			Exists:            true,
			Checkin:           ccr.EndTime,
			LatestRunID:       ccr.RunID,
			LastCCRReceived:   ccr.EndTime,
			Attributes:        getSquishedAttributes(&ccr.NodePayload),
			HasDeprecations:   hasDeprecations,
			DeprecationsCount: len(ccr.Deprecations),
		}

		if ccr.NodePayload.Automatic["ec2"] != nil {
			chefRunEc2 := ccr.NodePayload.Automatic["ec2"].(map[string]interface{})
			nodeState.Ec2.InstanceId = EmptyStringIfNil(chefRunEc2["instance_id"])
			//In the future we can use cloud ID for azure or whatever IDs
			nodeState.CloudID = nodeState.Ec2.InstanceId
			nodeState.Ec2.InstanceType = EmptyStringIfNil(chefRunEc2["instance_type"])

			if ccr.NodePayload.Automatic["cloud"] != nil {
				cloud := ccr.NodePayload.Automatic["cloud"].(map[string]interface{})
				// Getting the public_ipv4 from the `cloud.public_ipv4` attribute
				// Avoid the convertion to a `string` if it is `nil` - We want nil!
				nodeState.Ec2.PublicIpv4 = cloud["public_ipv4"]
			}
			nodeState.Ec2.PlacementAvailabilityZone =
				EmptyStringIfNil(chefRunEc2["placement_availability_zone"])
		}

		if ccr.Status == "failure" {
			nodeState.ErrorMessage = ccr.Error.Message
		}
	}
	return
}

// ToNodeAttribute Returns the node attribute from a ChefClientRun
func (ccr *ChefClientRun) ToNodeAttribute() (NodeAttribute, error) {

	if ccr.MessageType == "run_converge" { // Not currently handling run_start messages
		return CreateNodeAttribute(ccr.NodePayload, ccr.EntityUUID)
	}

	return NodeAttribute{}, nil
}

// CreateNodeAttribute crate the NodeAttribute from the NodePayload
func CreateNodeAttribute(nodePayload NodePayload, entityUUID string) (attribute NodeAttribute, err error) {
	attributeNormal, errMarshal := json.Marshal(nodePayload.Normal)
	if errMarshal != nil {
		err = errMarshal
	}

	attributeDefault, errMarshal := json.Marshal(nodePayload.Default)
	if errMarshal != nil {
		if err != nil {
			err = fmt.Errorf("%v %v", err, errMarshal)
		} else {
			err = errMarshal
		}
	}

	attributeOverride, errMarshal := json.Marshal(nodePayload.Override)
	if errMarshal != nil {
		if err != nil {
			err = fmt.Errorf("%v %v", err, errMarshal)
		} else {
			err = errMarshal
		}
	}

	attributeAutomatic, errMarshal := json.Marshal(nodePayload.Automatic)
	if errMarshal != nil {
		if err != nil {
			err = fmt.Errorf("%v %v", err, errMarshal)
		} else {
			err = errMarshal
		}
	}

	attributeNormalValueCount := CountNumberOfValuesMap(nodePayload.Normal)
	attributeDefaultValueCount := CountNumberOfValuesMap(nodePayload.Default)
	attributeOverrideValueCount := CountNumberOfValuesMap(nodePayload.Override)
	attributeAutomaticValueCount := CountNumberOfValuesMap(nodePayload.Automatic)
	attributeAllValueCount := attributeNormalValueCount + attributeDefaultValueCount + attributeOverrideValueCount + attributeAutomaticValueCount

	attribute = NodeAttribute{
		EntityUUID:          entityUUID,
		Name:                nodePayload.Name,
		ChefEnvironment:     nodePayload.ChefEnvironment,
		RunList:             nodePayload.RunList,
		Normal:              string(attributeNormal),
		NormalValueCount:    attributeNormalValueCount,
		Default:             string(attributeDefault),
		DefaultValueCount:   attributeDefaultValueCount,
		Override:            string(attributeOverride),
		OverrideValueCount:  attributeOverrideValueCount,
		Automatic:           string(attributeAutomatic),
		AutomaticValueCount: attributeAutomaticValueCount,
		AllValueCount:       attributeAllValueCount,
		LastUpdate:          time.Now().UTC(),
	}

	return
}

func (ccr *ChefClientRun) initializeNodeInfo() (sharedNodeInfo NodeInfo, err error) {
	uptimeSeconds, ok := ccr.NodePayload.Automatic["uptime_seconds"].(float64)
	if !ok {
		log.WithFields(log.Fields{
			"attribute": "uptime_seconds",
			"default":   0,
		}).Debug("Automatic attribute not found; setting default")
		uptimeSeconds = 0
	}

	sharedNodeInfo = NodeInfo{
		EntityUuid:       ccr.EntityUUID,
		EventAction:      "Finished",
		NodeName:         ccr.NodeName,
		Fqdn:             EmptyStringIfNil(ccr.NodePayload.Automatic["fqdn"]),
		Ipaddress:        ccr.NodePayload.Automatic["ipaddress"], // Avoid returning an empty string
		PolicyRevision:   EmptyStringIfNil(ccr.NodePayload.Automatic["policy_revision"]),
		UptimeSeconds:    int64(uptimeSeconds),
		OrganizationName: ccr.OrganizationName,
		Environment:      ccr.NodePayload.ChefEnvironment,
		Platform:         getPlatformWithVersion(ccr.NodePayload),
		PlatformFamily:   EmptyStringIfNil(ccr.NodePayload.Automatic["platform_family"]),
		PlatformVersion:  EmptyStringIfNil(ccr.NodePayload.Automatic["platform_version"]),
		// Tags                 ccr. TODO: Is this compliance tags?
		Source:             ccr.Source,
		Status:             ccr.Status,
		TotalResourceCount: ccr.TotalResourceCount,
		SourceFqdn:         ccr.ChefServerFqdn,
		ExpandedRunList:    ccr.ExpandedRunList,
		Deprecations:       ccr.Deprecations,
		Recipes:            ccr.RecipeNames(),
		ChefTags:           ccr.TagNames(),
		Roles:              ccr.RoleNames(),
		ResourceNames:      ccr.ResourceNames(),
		RunList:            ccr.RunList,
		Tags:               ccr.Tags,
		Timestamp:          time.Now().UTC(),
	}
	// # Chef 12 only sends [node][policy_group] whereas
	// # Chef 13 sends [node][policy_group] and [policy_group].
	// # If policy_group already exists as base key in Chef 13 case, don't
	// # append [node][policy_group] as it will convert to ["policy_group_value", "policy_group_value"].
	if ccr.PolicyName != "" {
		sharedNodeInfo.PolicyName = ccr.PolicyName
		sharedNodeInfo.PolicyGroup = ccr.PolicyGroup
	} else {
		sharedNodeInfo.PolicyName = ccr.NodePayload.PolicyName
		sharedNodeInfo.PolicyGroup = ccr.NodePayload.PolicyGroup
	}
	sharedNodeInfo.Cookbooks = ccr.Cookbooks()
	copy(sharedNodeInfo.RunList, ccr.RunList)
	sharedNodeInfo.ChefVersion = ccr.ChefVersion()
	sharedNodeInfo.VersionedCookbooks = VersionedCookbooks(ccr.NodePayload)
	return
}

// ChefVersion Returns a chef version string retrieved from automatic attributes or an empty string if it is not present
func (ccr *ChefClientRun) ChefVersion() string {
	chefPackages := extractMapOrEmpty(ccr.NodePayload.Automatic, "chef_packages")
	if len(chefPackages) > 0 {
		chefJson := extractMapOrEmpty(chefPackages, "chef")
		if len(chefJson) > 0 {
			return EmptyStringIfNil(chefJson["version"])
		}
	}
	return ""
}

// Cookbooks returns an array of strings containing the names of cookbooks
func (ccr *ChefClientRun) Cookbooks() []string {
	cookbooksJson := extractMapOrEmpty(ccr.NodePayload.Automatic, "cookbooks")
	cookbookArray := make([]string, 0, len(cookbooksJson))
	for k := range cookbooksJson {
		cookbookArray = append(cookbookArray, k)
	}
	return cookbookArray
}

// VersionedCookbooks returns an array of VersionedCookbooks
func VersionedCookbooks(nodePayload NodePayload) []VersionedCookbook {
	cookbooksJson := extractMapOrEmpty(nodePayload.Automatic, "cookbooks")
	cookbookArray := make([]VersionedCookbook, 0, len(cookbooksJson))
	for k, v := range cookbooksJson {
		var version interface{}
		if v != nil {
			vMap := v.(map[string]interface{})
			version = vMap["version"]
		}
		vc := VersionedCookbook{
			Name:    k,
			Version: EmptyStringIfNil(version),
		}
		cookbookArray = append(cookbookArray, vc)
	}
	return cookbookArray
}

// RoleNames Returns an array of strings containing only the tag names of the CCR
func (ccr *ChefClientRun) RoleNames() []string {
	rolesJson := extractArrayOrEmpty(ccr.NodePayload.Automatic, "roles")
	return interfaceArrayToStringArray(rolesJson)
}

// TagNames Returns an array of strings containing only the tag names of the CCR
func (ccr *ChefClientRun) TagNames() []string {
	tagsJson := extractArrayOrEmpty(ccr.NodePayload.Normal, "tags")
	return interfaceArrayToStringArray(tagsJson)
}

// RecipeNames Returns an array of strings containing only the recipe names of the CCR
func (ccr *ChefClientRun) RecipeNames() []string {
	recipesJson := extractArrayOrEmpty(ccr.NodePayload.Automatic, "recipes")
	return interfaceArrayToStringArray(recipesJson)
}

// ResourceNames Returns an array of strings containing only the resource names of the CCR
func (ccr *ChefClientRun) ResourceNames() []string {
	names := make([]string, len(ccr.Resources))
	for i, resource := range ccr.Resources {
		names[i] = resource.Name
	}
	return names
}

// This copies all fields from ChefClientResource except before and after fields to a Resource array
func (ccr *ChefClientRun) initializeResources() []Resource {
	resources := make([]Resource, len(ccr.Resources))
	for i, resource := range ccr.Resources {
		resources[i] = Resource{
			Type:            resource.Type,
			Name:            resource.Name,
			ID:              resource.ID,
			Duration:        resource.Duration,
			IgnoreFailure:   bool(resource.IgnoreFailure),
			Result:          resource.Result,
			Status:          resource.Status,
			CookbookName:    resource.CookbookName,
			CookbookVersion: resource.CookbookVersion,
			CookbookType:    resource.CookbookType,
			RecipeName:      resource.RecipeName,
			Conditional:     resource.Conditional,
			Delta:           resource.Delta,
		}
	}
	return resources
}

// Returns normal, default and override keys as a flattened dotted array
func getSquishedAttributes(nodePayload *NodePayload) []string {
	defaultAttr := squishKeys(nodePayload.Default)
	normalAttr := squishKeys(nodePayload.Normal)
	overrideAttr := squishKeys(nodePayload.Override)

	var attributes = [][]string{
		defaultAttr,
		normalAttr,
		overrideAttr,
	}
	return concatCopyPreAllocate(attributes)
}

func squishKeys(m map[string]interface{}) []string {
	flattenedAttrs := Flatten(m)
	attrArray := make([]string, 0, len(flattenedAttrs))
	for k := range flattenedAttrs {
		attrArray = append(attrArray, k)
	}

	return attrArray
}

func Flatten(m map[string]interface{}) map[string]interface{} {
	o := make(map[string]interface{})
	s := make(stack, 0)
	s = pushMapItemOnStack("", s, m)

	for !s.IsEmpty() {
		var i item
		s, i = s.Pop()
		switch child := i.value.(type) {
		case map[string]interface{}:
			s = pushMapItemOnStack(i.key, s, child)
		default:
			o[i.key] = i.value
		}
	}
	return o
}

type item struct {
	key   string
	value interface{}
}

type stack []item

func (s stack) Push(v item) stack {
	return append(s, v)
}

func (s stack) Pop() (stack, item) {
	l := len(s)
	return s[:l-1], s[l-1]
}

func (s stack) IsEmpty() bool {
	return len(s) == 0
}

func pushMapItemOnStack(tag string, s stack, m map[string]interface{}) stack {
	for k, v := range m {
		newKey := k
		if tag != "" {
			newKey = tag + "." + k
		}
		s = s.Push(item{key: newKey, value: v})
	}

	return s
}

// merge arrays into one array
// Credit to:
// https://stackoverflow.com/questions/37884361/concat-multiple-slices-in-golang
func concatCopyPreAllocate(slices [][]string) []string {
	var totalLen int
	for _, s := range slices {
		totalLen += len(s)
	}
	tmp := make([]string, totalLen)
	var i int
	for _, s := range slices {
		i += copy(tmp[i:], s)
	}
	return tmp
}

// CountNumberOfValuesMap - counts the number of
func CountNumberOfValuesMap(jsonMap map[string]interface{}) int {
	count := 0
	for _, value := range jsonMap {
		count += countNumberOfValueObject(value)
	}
	return count
}

func countNumberOfValueObject(object interface{}) int {
	count := 0
	switch v := object.(type) {
	case map[string]interface{}:
		count += CountNumberOfValuesMap(v)
	case []interface{}:
		for _, item := range v {
			count += countNumberOfValueObject(item)
		}
	default:
		count++
	}

	return count
}

func getPlatformWithVersion(nodePayload NodePayload) string {
	platform := EmptyStringIfNil(nodePayload.Automatic["platform"])
	platformVersion := EmptyStringIfNil(nodePayload.Automatic["platform_version"])

	platformAndVersion := ""

	if platform != "" && platformVersion != "" {
		platformAndVersion = platform + " " + platformVersion
	} else if platformVersion == "" {
		platformAndVersion = platform
	}

	return platformAndVersion
}

type ignoreFailureOption bool

func (bit *ignoreFailureOption) UnmarshalJSON(data []byte) error {
	*bit = false
	// strip quotes
	asString := strings.Replace(string(data), "\"", "", -1)

	// quiet is the same as true for automate
	if asString == "true" || asString == "quiet" {
		*bit = ignoreFailureOption(true)
	}

	return nil
}
