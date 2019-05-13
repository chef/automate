package azure

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/Azure/azure-sdk-for-go/services/compute/mgmt/2017-12-01/compute"
	"github.com/Azure/azure-sdk-for-go/services/network/mgmt/2017-11-01/network"
	"github.com/Azure/azure-sdk-for-go/services/resources/mgmt/2016-06-01/subscriptions"
	"github.com/Azure/azure-sdk-for-go/services/resources/mgmt/2017-05-10/resources"
	"github.com/Azure/go-autorest/autorest"
	"github.com/Azure/go-autorest/autorest/adal"
	"github.com/Azure/go-autorest/autorest/azure"
	"github.com/Azure/go-autorest/autorest/azure/auth"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/components/compliance-service/utils/pool"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
	"github.com/chef/automate/lib/stringutils"
)

type Creds struct {
	Token    *adal.ServicePrincipalToken
	TenantID string
}

// New returns a Creds struct of ServicePrincipalToken and TenantID given azure creds
func New(clientID string, clientSecret string, tenantID string) (Creds, error) {
	if len(clientID) == 0 && len(clientSecret) == 0 && len(tenantID) == 0 {
		return Creds{}, nil
	}
	oAuthConfig, err := adal.NewOAuthConfig(azure.PublicCloud.ActiveDirectoryEndpoint, tenantID)
	if err != nil {
		return Creds{}, errors.Wrap(err, "azure - New unable to get oauth config")
	}
	token, err := adal.NewServicePrincipalToken(
		*oAuthConfig,
		clientID,
		clientSecret,
		azure.PublicCloud.ResourceManagerEndpoint)
	if err != nil {
		return Creds{}, errors.Wrap(err, "azure - New unable to get token")
	}
	token.SetAutoRefresh(true)
	return Creds{Token: token, TenantID: tenantID}, nil
}

func getAuthorizer(token *adal.ServicePrincipalToken) autorest.Authorizer {
	if token != nil {
		return autorest.NewBearerAuthorizer(token)
	}
	authorizer, err := auth.NewAuthorizerFromEnvironment()
	if err != nil {
		logrus.Errorf("getAuthorizer unable to create new authorizer from environment, %v", err)
		return nil
	}
	return authorizer
}

func (creds *Creds) GetTenantIds(ctx context.Context) ([]string, error) {
	tenants := make([]string, 0)
	client := subscriptions.NewTenantsClient()
	client.Authorizer = getAuthorizer(creds.Token)
	// "List gets all tenants for the account."
	for res, err := client.List(ctx); res.NotDone(); err = res.Next() {
		if err != nil {
			return tenants, errors.Wrap(err, "ListSubscriptions unable to list subscriptions")
		}
		for _, val := range res.Values() {
			tenants = append(tenants, *val.TenantID)
		}
	}
	return tenants, nil
}

// ListSubscriptions returns an array of all subscriptions for the account
func (creds *Creds) ListSubscriptions(ctx context.Context) ([]*manager.ManagerNode, error) {
	subs := make([]*manager.ManagerNode, 0)
	client := subscriptions.NewClient()
	client.Authorizer = getAuthorizer(creds.Token)
	// "List gets all subscriptions for a tenant."
	for res, err := client.List(ctx); res.NotDone(); err = res.Next() {
		if err != nil {
			return subs, errors.Wrap(err, "ListSubscriptions unable to list subscriptions")
		}
		for _, val := range res.Values() {
			subs = append(subs, &manager.ManagerNode{Id: *val.SubscriptionID, Name: *val.DisplayName})
		}
	}
	return subs, nil
}

// TestConnectivity tests if we can reach azure and list subscriptions
func (creds *Creds) TestConnectivity(ctx context.Context) error {
	// I haven't found any dry run operation style bits in the azure api yet
	// we should make this check more solid, but for now, to get us going,
	// we'll check if we can list subscriptions
	subs, err := creds.ListSubscriptions(ctx)
	if err != nil {
		return errors.Wrap(err, "TestConnectivity unable to connect")
	}
	// With bad credentials, Azure returns an empty list.
	if len(subs) == 0 {
		return utils.ProcessUnauthenticated(nil, "TestConnectivity unable to list subscriptions")
	}
	return nil
}

// ListLocations returns an array of all locations for the account
func (creds *Creds) ListLocations(ctx context.Context, subs []*manager.ManagerNode) ([]string, error) {
	locations := make([]string, 0)
	client := subscriptions.NewClient()
	client.Authorizer = getAuthorizer(creds.Token)
	for _, sub := range subs {
		res, err := client.ListLocations(ctx, sub.Id)
		if err != nil {
			return locations, errors.Wrap(err, "ListLocations unable to list locations")
		}
		for _, val := range *res.Value {
			locations = append(locations, *val.Name)
		}
	}
	return locations, nil
}

// ListTags returns an array of all tags for the account
func (creds *Creds) ListTags(ctx context.Context, subs []*manager.ManagerNode) (map[string][]string, error) {
	tags := make(map[string][]string, 0)
	for _, sub := range subs {
		client := resources.NewTagsClient(sub.Id)
		client.Authorizer = getAuthorizer(creds.Token)
		res, err := client.List(ctx)
		if err != nil {
			return tags, errors.Wrap(err, "ListTags unable to list tags")
		}

		for _, val := range res.Values() {
			tagVals := make([]string, len(*val.Values))
			for _, vals := range *val.Values {
				tagVals = append(tagVals, *vals.TagValue)
			}
			tags[*val.TagName] = tagVals
		}
	}
	return tags, nil
}

type vmTaskResult struct {
	Nodes        []*manager.ManagerNode
	Subscription string
}

func buildEqualityFilterString(key string, values []string, expression string) []string {
	filterStrings := make([]string, 0, len(values))
	for _, val := range values {
		filterStrings = append(filterStrings, key+expression+"'"+val+"'")
	}
	return filterStrings
}

func buildFilterStrings(filters []*common.Filter) (map[string][]string, []string) {
	// the azure api is based on oData syntax; the resources api docs don't actually list
	// what we can use here, but it's similar to the filter syntax described here: https://docs.microsoft.com/en-us/rest/api/apimanagement/tagresource/listbyservice
	// available filter types are location, name, tagname, tagvalue, and resourceType
	filterStrings := make(map[string][]string)
	var excludedTagsFilterStrings []string
	for _, filter := range filters {
		switch filter.Key {
		case "region":
			// location eq '' or location ne ''
			// location does not support substringof filters:
			// Message="Invalid $filter 'substringof(location, 'eastus')' specified in the query string.
			// Details: 'Unsupported filter function found:'substringof'. Property name:'location'. Supported functions: '''"
			if filter.Exclude {
				filterStrings[filter.Key] = append(filterStrings[filter.Key], buildEqualityFilterString("location", filter.Values, " ne ")...)
			} else {
				filterStrings[filter.Key] = append(filterStrings[filter.Key], buildEqualityFilterString("location", filter.Values, " eq ")...)
			}
		case "name":
			if filter.Exclude {
				// name ne ''
				// I was hoping to get the negated state working with the substringof syntax, but I have not yet
				// been successful. It is not currently supported by azure
				filterStrings[filter.Key] = append(filterStrings[filter.Key], buildEqualityFilterString("name", filter.Values, " ne ")...)
			} else {
				for _, val := range filter.Values {
					filterStrings[filter.Key] = append(filterStrings[filter.Key], "substringof(name, '"+val+"')")
				}
			}

		case "subscription_id":
			// do nothing, we filter by subscription id before we get here
			continue

		default:
			if filter.Exclude {
				// the api does not support search by exclusion for tags
				excludedTagsFilterStrings = append(excludedTagsFilterStrings, buildTagEqualityFilterString(filter.Key, filter.Values, "ne ")...)
			} else {
				filterStrings[filter.Key] = append(filterStrings[filter.Key], buildTagEqualityFilterString(filter.Key, filter.Values, "eq ")...)
			}
		}
	}
	return filterStrings, excludedTagsFilterStrings
}

func buildTagEqualityFilterString(key string, vals []string, expression string) []string {
	// tagname eq '' and tagvalue eq ''
	// substringof is not supported
	filterStrings := make([]string, 0, len(vals))
	for _, val := range vals {
		filterStrings = append(filterStrings, "tagname eq '"+key+"' and tagvalue "+expression+"'"+val+"'")
	}
	return filterStrings
}

func (creds *Creds) handleVMFilters(ctx context.Context, sub *manager.ManagerNode, filters []*common.Filter) ([]string, error) {
	// to filter vms, we need to call the resources api, then call the virtual machines api
	// with the appropriate resource group name
	resourceClient := resources.NewClient(sub.Id)
	resourceClient.Authorizer = getAuthorizer(creds.Token)
	// filterStrings is map of string to array strings
	// each map entry represents an and filter
	// all filters within a map entry are regarded as or filters
	filterStringsByKey, excludedTagFilterStrings := buildFilterStrings(filters)
	resourcesList := make(map[string][]resources.ListResultPage)
	// for each entry in the map, we will range over the array of filter strings
	// and attach the results to resourcesList as a map entry
	for key, filterStrings := range filterStringsByKey {
		for _, filterString := range filterStrings {
			logrus.Debugf("filter string used to call list on azure resources: %s", filterString)
			resources, err := resourceClient.List(ctx, filterString, "", nil)
			if err != nil {
				logrus.Error(time.Now().UTC(), err)
				return []string{}, errors.Wrap(err, "unable to list resources with provided filters")
			}
			resourcesList[key] = append(resourcesList[key], resources)
		}
	}
	resourceListVals := make(map[string][]resources.GenericResource)
	if len(excludedTagFilterStrings) > 0 {
		for _, excludedTagFilterString := range excludedTagFilterStrings {
			logrus.Debugf("exclusion filter string used to call list on azure resources: %s", excludedTagFilterString)
			excludedResources, err := resourceClient.List(ctx, excludedTagFilterString, "", nil)
			if err != nil {
				logrus.Error(time.Now().UTC(), err)
				return []string{}, errors.Wrap(err, "unable to list resources with provided filters")
			}
			for key, resourcesL := range resourcesList {
				for _, resources := range resourcesL {
					resourceListVals[key] = handleExcludedResources(resources.Values(), excludedResources.Values())
				}
			}
		}
	} else {
		for key, resourcesL := range resourcesList {
			for _, resources := range resourcesL {
				resourceListVals[key] = append(resourceListVals[key], resources.Values()...)
			}
		}
	}
	return handleResources(resourceListVals, filters)
}

func handleExcludedResources(resourcesList []resources.GenericResource, excludedResourcesList []resources.GenericResource) []resources.GenericResource {
	filteredList := resourcesList
	for _, excludedResource := range excludedResourcesList {
		filteredList = removeMatchingResource(filteredList, excludedResource)
	}
	return filteredList
}

func removeMatchingResource(list []resources.GenericResource, matcher resources.GenericResource) []resources.GenericResource {
	newResourcesList := make([]resources.GenericResource, 0)
	for _, resource := range list {
		if resource.ID != matcher.ID {
			newResourcesList = append(newResourcesList, resource)
		}
	}
	return newResourcesList
}

func handleResources(vals map[string][]resources.GenericResource, filters []*common.Filter) ([]string, error) {
	resourceGroupNames := make(map[string][]string, 0)
	// we go through each entry in the map and get a map entry of resource group names
	var aKey string
	for key, resByKey := range vals {
		aKey = key
		for _, res := range resByKey {
			resource := strings.Split(*res.ID, "/")
			if len(resource) >= 4 {
				resourceGroup := resource[4]
				resourceGroupNames[key] = append(resourceGroupNames[key], resourceGroup)
			}
		}
	}
	if len(resourceGroupNames) == 0 {
		err := fmt.Sprintf("No results available for provided filters: %v", filters)
		logrus.Error(err)
		return []string{}, &utils.InvalidError{Msg: err}
	}
	// get the intersect of the resource group names for each map entry
	// in other words, if there are 3 map entries (filter by region useast + tag:Test-VJ + tag:more-another_tag)
	// we only return the results that exist in all three lists
	resourceGroupNamesList := resourceGroupNames[aKey]
	for _, resourceGroupNameList := range resourceGroupNames {
		resourceGroupNamesList = intersect(resourceGroupNamesList, resourceGroupNameList)
	}
	if len(resourceGroupNamesList) == 0 {
		err := fmt.Sprintf("No results available for provided filters: %v", filters)
		logrus.Error(err)
		return []string{}, &utils.InvalidError{Msg: err}
	}
	return resourceGroupNamesList, nil
}

func intersect(a, b []string) []string {
	var result []string
	for _, i := range a {
		for _, x := range b {
			if i == x {
				result = append(result, i)
			}
		}
	}
	return result
}

type vmTaskStateResult struct {
	VMStates []pgdb.InstanceState
}

func (creds *Creds) getVMStatePoolTasks(ctx context.Context, subs []*manager.ManagerNode) []*pool.Task {
	var tasks = make([]*pool.Task, 0)
	for _, sub := range subs {
		client := compute.NewVirtualMachinesClient(sub.Id)
		client.Authorizer = getAuthorizer(creds.Token)
		logrus.Debugf("Getting nodes for azure subscription id: %s:%s", sub.Name, sub.Id)

		f := func() (pool.TaskResult, error) {
			var vtr = vmTaskStateResult{}
			var vmStateList []pgdb.InstanceState

			for vms, err := client.ListAll(ctx); vms.NotDone(); err = vms.Next() {
				if err != nil {
					logrus.Error(time.Now().UTC(), err)
					return pool.TaskResult(vtr), errors.Wrap(err, "ListVMInfo unable to list vm info")
				}
				for _, vm := range vms.Values() {
					var vmID, vmName, vmLocation, state string
					vmID = *vm.VMID
					vmName = *vm.Name
					vmLocation = *vm.Location
					ref := strings.Split(*vm.ID, "/")
					if len(ref) >= 8 {
						resourceGroup := ref[4]
						vmName := ref[8]
						vmInfo, err := client.Get(ctx, resourceGroup, vmName, "instanceView")
						if err != nil {
							logrus.Error(time.Now().UTC(), err)
							return pool.TaskResult(vtr), errors.Wrap(err, "ListVMInfo unable to list vm info")
						}
						state = handleStateResponse(vmInfo.InstanceView.Statuses)
					}
					vmStateList = append(vmStateList, pgdb.InstanceState{
						ID:     vmID,
						Name:   vmName,
						Region: vmLocation,
						State:  state,
					})
				}
			}
			vtr.VMStates = vmStateList
			return pool.TaskResult(vtr), nil
		}
		tasks = append(tasks, pool.NewTask(f))
	}
	return tasks
}

func handleStateResponse(statuses *[]compute.InstanceViewStatus) string {
	for _, status := range *statuses {
		if strings.HasPrefix(*status.Code, "PowerState") {
			return strings.TrimPrefix(*status.DisplayStatus, "VM ")
		}
	}
	return ""
}

func (creds *Creds) getVMPoolTasks(ctx context.Context, subs []*manager.ManagerNode, filters []*common.Filter) []*pool.Task {
	var tasks = make([]*pool.Task, 0)
	for _, sub := range subs {
		client := compute.NewVirtualMachinesClient(sub.Id)
		client.Authorizer = getAuthorizer(creds.Token)
		logrus.Debugf("Getting nodes for azure subscription id: %s:%s", sub.Name, sub.Id)

		f := func() (pool.TaskResult, error) {
			var vtr = vmTaskResult{}
			var vmList []*manager.ManagerNode
			var err error
			resourceGroupNames := make([]string, 0)

			if len(filters) > 0 {
				resourceGroupNames, err = creds.handleVMFilters(ctx, sub, filters)
				if err != nil {
					return pool.TaskResult(vtr), errors.Wrap(err, "ListVMInfo unable to parse filters")
				}
			}
			if len(resourceGroupNames) > 0 {
				for _, name := range uniqueStringSliceCaseInsens(resourceGroupNames) {
					vms, err := client.List(ctx, name)
					if err != nil {
						logrus.Error(time.Now().UTC(), err)
						return pool.TaskResult(vtr), errors.Wrap(err, "ListVMInfo unable to list vm info")
					}
					vmList, err = creds.buildVMList(ctx, vms.Values(), sub, vmList, name)
					if err != nil {
						return pool.TaskResult(vtr), errors.Wrap(err, "ListVMInfo unable to parse vm info")
					}
				}
			} else {
				for vms, err := client.ListAll(ctx); vms.NotDone(); err = vms.Next() {
					if err != nil {
						logrus.Error(time.Now().UTC(), err)
						return pool.TaskResult(vtr), errors.Wrap(err, "ListVMInfo unable to list vm info")
					}
					vmList, err = creds.buildVMList(ctx, vms.Values(), sub, vmList, "")
					if err != nil {
						return pool.TaskResult(vtr), errors.Wrap(err, "ListVMInfo unable to parse vm info")
					}
				}
			}
			vtr.Nodes = vmList
			return pool.TaskResult(vtr), nil
		}
		tasks = append(tasks, pool.NewTask(f))
	}
	return tasks
}

func (creds *Creds) buildVMList(ctx context.Context, vms []compute.VirtualMachine, sub *manager.ManagerNode, vmList []*manager.ManagerNode, resourceGroupName string) ([]*manager.ManagerNode, error) {
	vmNodeList, err := creds.parseVMInfo(ctx, vms, sub, resourceGroupName)
	if err != nil {
		return vmList, err
	}
	vmList = append(vmList, vmNodeList...)
	return vmList, nil
}

func handleVMTags(tags map[string]*string) []*common.Kv {
	instanceTags := make([]*common.Kv, 0)
	for k, v := range tags {
		instanceTags = append(instanceTags, &common.Kv{Key: k, Value: *v})
	}
	return instanceTags
}

func (creds *Creds) parseVMInfo(ctx context.Context, values []compute.VirtualMachine, sub *manager.ManagerNode, resourceGroupName string) ([]*manager.ManagerNode, error) {
	vmList := make([]*manager.ManagerNode, 0)
	for _, val := range values {
		var platform, ip, online string
		os := val.StorageProfile.OsDisk.OsType
		if os == "Windows" {
			platform = "windows"
		} else {
			platform = "linux"
		}
		online = "Online:Azure"
		ref := strings.Split(*val.ID, "/")
		if len(ref) >= 4 {
			resourceGroupName = ref[4]
		}
		instanceTags := handleVMTags(val.Tags)
		for _, t := range *val.NetworkProfile.NetworkInterfaces {
			var group, name string
			ref := strings.Split(*t.ID, "/")
			if len(ref) >= 8 {
				group = ref[4]
				name = ref[8]
			}
			intClient := network.NewInterfacesClient(sub.Id)
			intClient.Authorizer = getAuthorizer(creds.Token)
			netInterface, err := intClient.Get(ctx, group, name, "")
			if err != nil {
				return vmList, errors.Wrap(err, "parseVMInfo unable to list interfaces")
			}
			var ipGroup, ipName string
			for _, ipconfig := range *netInterface.IPConfigurations {
				if ipconfig.PublicIPAddress != nil && ipconfig.PublicIPAddress.ID != nil {
					ipRef := strings.Split(*ipconfig.PublicIPAddress.ID, "/")
					if len(ipRef) >= 8 {
						ipGroup = ipRef[4]
						ipName = ipRef[8]
					}
				}
				if len(ipGroup) == 0 && len(ipName) == 0 {
					// if public ip not available, get the private ip
					if ipconfig.PrivateIPAddress != nil {
						logrus.Infof("public ip address not found for node %s; getting private ip address", *val.Name)
						ipRef := strings.Split(*ipconfig.PrivateIPAddress, "/")
						if len(ipRef) >= 8 {
							ipGroup = ipRef[4]
							ipName = ipRef[8]
						}
					}
				}
			}
			if ipGroup != "" && ipName != "" {
				ipClient := network.NewPublicIPAddressesClient(sub.Id)
				ipClient.Authorizer = getAuthorizer(creds.Token)
				ipAddress, err := ipClient.Get(ctx, ipGroup, ipName, "")
				if err != nil {
					return vmList, errors.Wrap(err, "parseVMInfo unable to list ip addresses")
				}
				if ipAddress.IPAddress != nil {
					ip = *ipAddress.IPAddress
				}
			}
		}
		logrus.Infof("Registering %s node %s from azure account with ip %s", platform, *val.Name, ip)
		vmList = append(vmList, &manager.ManagerNode{
			Id:                *val.VMID,
			PublicIp:          ip,
			Tags:              instanceTags,
			Region:            *val.Location,
			Name:              *val.Name,
			Platform:          platform,
			MachineIdentifier: resourceGroupName,
			Group:             sub.Id,
			Ssm:               online,
		})
	}
	return vmList, nil
}

func handleSubscriptionFilters(filters []*common.Filter) (subs []*manager.ManagerNode, excludedSubs []string) {
	for _, filter := range filters {
		if filter.Key == "subscription_id" {
			if !filter.Exclude {
				for _, val := range filter.Values {
					subs = append(subs, &manager.ManagerNode{Id: val})
				}
				return subs, nil
			}
			excludedSubs = filter.Values
		}
	}
	return subs, excludedSubs
}

func removeExcludedSubsFromList(list []*manager.ManagerNode, exclude []string) []*manager.ManagerNode {
	returnList := []*manager.ManagerNode{}

	excluded := func(id string) bool { return stringutils.SliceContains(exclude, id) }
	for _, sub := range list {
		if !excluded(sub.Id) {
			returnList = append(returnList, sub)
		}
	}
	return returnList
}

func (creds *Creds) GetSubscriptions(ctx context.Context, filters []*common.Filter) ([]*manager.ManagerNode, error) {
	subs, excludedSubs := handleSubscriptionFilters(filters)
	if len(subs) > 0 {
		return subs, nil
	}
	subsList, err := creds.ListSubscriptions(ctx)
	if err != nil {
		return subsList, errors.Wrap(err, "getSubscriptions unable to list subscriptions")
	}
	if len(excludedSubs) == 0 {
		return subsList, nil
	}
	return removeExcludedSubsFromList(subsList, excludedSubs), nil
}

// QueryVMs returns an array of ManagerNodes, one for each vm in the account, over all subscriptions
func (creds *Creds) QueryVMs(ctx context.Context, filters []*common.Filter) (map[string][]*manager.ManagerNode, error) {
	var err error
	vmList := make(map[string][]*manager.ManagerNode, 0)
	subs := make([]*manager.ManagerNode, 0)
	for _, filter := range filters {
		if filter.Key == "subscription_id" {
			subs = make([]*manager.ManagerNode, 0)
			for _, subID := range filter.Values {
				subs = append(subs, &manager.ManagerNode{Id: subID})
			}
		}
	}
	if len(subs) == 0 {
		subs, err = creds.GetSubscriptions(ctx, filters)
		if err != nil {
			return vmList, errors.Wrap(err, "QueryVMs unable to list subscriptions")
		}
	}
	startTime := time.Now()
	poolOfTasks := pool.NewPool(creds.getVMPoolTasks(ctx, subs, filters), len(subs))
	poolOfTasks.Run()
	logrus.Debugf("QueryVMs time to run in parallel: %s", time.Since(startTime))

	for _, task := range poolOfTasks.Tasks {
		taskVMs := task.Result.(vmTaskResult)
		if len(taskVMs.Nodes) > 0 {
			vmList[taskVMs.Subscription] = taskVMs.Nodes
		}
	}

	logrus.Debugf("Got %d vms back for all subscriptions.", len(vmList))

	if !poolOfTasks.HasErrors() {
		logrus.Debugf("We had NO errors while retrieving nodes from azure!!")
	} else {
		logrus.Errorf("We had errors retrieving nodes from azure: %+v", poolOfTasks.GetErrors())
	}
	return vmList, nil
}

// QueryVMState returns an array of ManagerNodes, one for each vm in the account, over all subscriptions
func (creds *Creds) QueryVMState(ctx context.Context) ([]pgdb.InstanceState, error) {
	var err error
	vmList := make([]pgdb.InstanceState, 0)
	subs, err := creds.GetSubscriptions(ctx, []*common.Filter{})
	if err != nil {
		return vmList, errors.Wrap(err, "QueryVMs unable to list subscriptions")
	}

	startTime := time.Now()
	poolOfTasks := pool.NewPool(creds.getVMStatePoolTasks(ctx, subs), len(subs))
	poolOfTasks.Run()
	logrus.Debugf("QueryVMs time to run in parallel: %s", time.Since(startTime))

	for _, task := range poolOfTasks.Tasks {
		taskVMs := task.Result.(vmTaskStateResult)
		if len(taskVMs.VMStates) > 0 {
			vmList = append(vmList, taskVMs.VMStates...)
		}
	}

	logrus.Debugf("Got %d vms back for all subscriptions.", len(vmList))

	if !poolOfTasks.HasErrors() {
		logrus.Debugf("We had NO errors while retrieving nodes from azure!!")
	} else {
		logrus.Errorf("We had errors retrieving nodes from azure: %+v", poolOfTasks.GetErrors())
	}
	return vmList, nil
}

// QueryField returns account tags, locations, subscriptions
func (creds *Creds) QueryField(ctx context.Context, filters []*common.Filter, field string) ([]string, error) {
	var err error
	resultArray := make([]string, 0)
	subs := make([]*manager.ManagerNode, 0)
	for _, filter := range filters {
		if filter.Key == "subscription_id" {
			subs = make([]*manager.ManagerNode, 0)
			for _, subID := range filter.Values {
				subs = append(subs, &manager.ManagerNode{Id: subID})
			}
		}
	}
	if len(subs) == 0 {
		subs, err = creds.ListSubscriptions(ctx)
		if err != nil {
			return nil, errors.Wrap(err, "QueryField unable to list subscriptions")
		}
	}
	switch field {
	case "regions":
		resultArray, err = creds.ListLocations(ctx, subs)
		if err != nil {
			return nil, errors.Wrap(err, "QueryField unable to list locations")
		}
	case "names", "tags:name":
		vmsResult, err := creds.QueryVMs(ctx, []*common.Filter{})
		if err != nil {
			return nil, errors.Wrap(err, "QueryField unable to query vms")
		}
		for _, vms := range vmsResult {
			for _, vm := range vms {
				resultArray = append(resultArray, vm.Name)
			}
		}
	case "tags":
		tags, err := creds.ListTags(ctx, subs)
		if err != nil {
			return nil, errors.Wrap(err, "QueryField unable to list tags")
		}
		for k := range tags {
			resultArray = append(resultArray, k)
		}
		resultArray = append(resultArray, "name")
	case "subscriptions":
		for _, sub := range subs {
			resultArray = append(resultArray, fmt.Sprintf("%s:%s", sub.Name, sub.Id))
		}
	default:
		if strings.HasPrefix(field, "tags:") {
			tags, err := creds.ListTags(ctx, subs)
			if err != nil {
				return nil, errors.Wrap(err, "QueryField unable to list tags")
			}
			key := strings.TrimPrefix(field, "tags:")
			for k, v := range tags {
				if k == key {
					resultArray = append(resultArray, v...)
				}
			}
		} else {
			return resultArray, utils.ProcessInvalid(nil, fmt.Sprintf("invalid filter field %s", field))
		}
	}

	return resultArray, nil
}

func uniqueStringSliceCaseInsens(stringSlice []string) []string {
	keys := make(map[string]bool)
	list := []string{}
	for _, entry := range stringSlice {
		if _, found := keys[strings.ToLower(entry)]; !found {
			keys[strings.ToLower(entry)] = true
			list = append(list, entry)
		}
	}
	return list
}

const (
	RunCommandLinuxShellScript = "RunShellScript"
	RunCommandPowerShellScript = "RunPowerShellScript"
)

func sendCommand(ctx context.Context, job *types.InspecJob, script string, scriptType string, client compute.VirtualMachinesClient) (compute.VirtualMachinesRunCommandFuture, error) {
	runCommandScript := []string{script}
	scriptTypeAZ := RunCommandLinuxShellScript
	if scriptType == inspec.PowershellScript {
		scriptTypeAZ = RunCommandPowerShellScript
	}

	params := compute.RunCommandInput{
		CommandID: &scriptTypeAZ,
		Script:    &runCommandScript,
	}
	logrus.Infof("calling runcommand %s with nodename %s resourcegroupname %s", scriptTypeAZ, job.NodeName, job.MachineIdentifier)
	return client.RunCommand(ctx, job.MachineIdentifier, job.NodeName, params)
}

func (creds *Creds) SendRunCommandJob(ctx context.Context, job *types.InspecJob, script string, scriptType string) error {
	*job.NodeStatus = types.StatusRunning
	client := compute.NewVirtualMachinesClient(job.TargetConfig.SubscriptionId)
	client.Authorizer = getAuthorizer(creds.Token)
	future, err := sendCommand(ctx, job, script, scriptType, client)
	if err != nil {
		*job.NodeStatus = types.StatusFailed
		return errors.Wrap(err, fmt.Sprintf("unable to run scan: %s %s", job.NodeName, job.MachineIdentifier))
	}
	err = future.WaitForCompletionRef(ctx, client.Client)
	if err != nil {
		*job.NodeStatus = types.StatusFailed
		return errors.Wrap(err, "unable to get command status")
	}
	logrus.Infof("azure run command job for node %s status %s", job.NodeName, future.Status())
	*job.NodeStatus = types.StatusCompleted
	return nil
}
