package awsec2

import (
	"context"
	"fmt"
	"os"

	"time"

	"sort"

	"strings"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/awserr"
	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/aws/aws-sdk-go/aws/credentials/stscreds"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/ec2"
	"github.com/aws/aws-sdk-go/service/iam"
	"github.com/aws/aws-sdk-go/service/ssm"
	"github.com/aws/aws-sdk-go/service/sts"
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

const DefaultRegion string = "us-east-1"

type nodesTaskResult struct {
	Nodes  []*manager.ManagerNode
	Region string
}

type AwsCreds struct {
	AccessKeyId     string
	SecretAccessKey string
	ArnRole         string
	SessionToken    string
	Region          string
}

// New instantiates a new AWS session and returns the API client
func New(AwsAccessKeyId string, AwsSecretAccessKey string, arnRole string, sessionToken string) (*AwsCreds, error) {
	logrus.Infof("Credential len: access key %d and secret %d", len(AwsAccessKeyId), len(AwsSecretAccessKey))
	// Create new AwsCreds client
	region := os.Getenv("AWS_REGION")
	if region == "" {
		region = DefaultRegion
	}
	return &AwsCreds{
		AccessKeyId:     AwsAccessKeyId,
		SecretAccessKey: AwsSecretAccessKey,
		ArnRole:         arnRole,
		SessionToken:    sessionToken,
		Region:          region,
	}, nil
}

// newClient instantiates a new AWS session and returns the API client
func (creds *AwsCreds) newClient(AwsRegion string) (*ec2.EC2, error) {
	if AwsRegion == "" {
		AwsRegion = creds.Region
	}
	sess := session.Must(session.NewSession())

	if len(creds.AccessKeyId) == 0 && len(creds.ArnRole) == 0 && len(creds.SecretAccessKey) == 0 {
		// users running automate in aws connect with no credentials!
		return ec2.New(sess, &aws.Config{Region: aws.String(AwsRegion)}), nil
	}
	cred := creds.getCredsForAwsConfig(sess)

	// Create new aws client session
	return ec2.New(sess, &aws.Config{Region: aws.String(AwsRegion), Credentials: cred}), nil
}

// newSTS instantiates a new AWS session and returns the STS API client
func (creds *AwsCreds) newSTS(AwsRegion string) (*sts.STS, error) {
	if AwsRegion == "" {
		AwsRegion = creds.Region
	}
	sess := session.Must(session.NewSession())

	if len(creds.AccessKeyId) == 0 && len(creds.ArnRole) == 0 && len(creds.SecretAccessKey) == 0 {
		// users running automate in aws connect with no credentials!
		return sts.New(sess, &aws.Config{Region: aws.String(AwsRegion)}), nil
	}
	cred := creds.getCredsForAwsConfig(sess)

	// Create new aws sts client session
	return sts.New(sess, &aws.Config{Region: aws.String(AwsRegion), Credentials: cred}), nil
}

func (creds *AwsCreds) getCredsForAwsConfig(sess *session.Session) *credentials.Credentials {
	if len(creds.ArnRole) > 0 {
		// if the user has provided a role, we will prioritize this method
		return stscreds.NewCredentials(sess, creds.ArnRole)
	}
	return credentials.NewStaticCredentials(creds.AccessKeyId, creds.SecretAccessKey, creds.SessionToken)
}

func (creds *AwsCreds) newIAM() (*iam.IAM, error) {
	sess := session.Must(session.NewSession())
	if len(creds.AccessKeyId) == 0 && len(creds.ArnRole) == 0 && len(creds.SecretAccessKey) == 0 {
		// users running automate in aws connect with no credentials!
		return iam.New(sess), nil
	}
	cred := creds.getCredsForAwsConfig(sess)

	return iam.New(sess, &aws.Config{Credentials: cred}), nil
}

func (creds *AwsCreds) GetAccountAlias(ctx context.Context) (string, error) {
	svc, err := creds.newIAM()
	if err != nil {
		return "", err
	}
	input := &iam.ListAccountAliasesInput{}

	result, err := svc.ListAccountAliasesWithContext(ctx, input)
	if err != nil {
		return "", err
	}

	if len(result.AccountAliases) > 0 {
		return *result.AccountAliases[0], nil
	}
	return "", nil
}

func (creds *AwsCreds) GetAccountID(ctx context.Context) (string, error) {
	client, err := creds.newSTS(creds.Region)
	if err != nil {
		return "", errors.Wrap(err, "GetAccountID unable to create a session connection to STS")
	}
	params := sts.GetCallerIdentityInput{}
	resp, err := client.GetCallerIdentityWithContext(ctx, &params)
	if err != nil {
		return "", errors.Wrap(&utils.InvalidError{Msg: err.Error()}, "GetAccountID unable to call GetCallerIdentity API")
	}
	return *resp.Account, nil
}

// getRegions returns all regions available for this AWS account
func (creds *AwsCreds) getRegions(ctx context.Context, filters []*string) ([]string, error) {
	client, err := creds.newClient(creds.Region)
	if err != nil {
		return nil, err
	}
	var result *ec2.DescribeRegionsOutput
	regions := make([]string, 0)
	if len(filters) > 0 {
		// this allows us to send filters like "get all regions for eu* and us-west*"
		filtersList := formatRegionFilters(filters)
		result, err = client.DescribeRegionsWithContext(ctx, &ec2.DescribeRegionsInput{
			Filters: filtersList,
		})
	} else {
		result, err = client.DescribeRegionsWithContext(ctx, &ec2.DescribeRegionsInput{})
	}
	if err != nil {
		return nil, err
	}
	for _, region := range result.Regions {
		regions = append(regions, *region.RegionName)
	}
	return regions, nil
}

func formatRegionFilters(filters []*string) []*ec2.Filter {
	return []*ec2.Filter{{Name: aws.String("endpoint"), Values: filters}}
}

// GetRegions returns a list of the names of AWS regions visible to this set of AWS credentials.
func (creds *AwsCreds) GetRegions(ctx context.Context) ([]string, error) {
	client, err := creds.newClient(creds.Region)
	if err != nil {
		return nil, err
	}

	result, err := client.DescribeRegionsWithContext(ctx, &ec2.DescribeRegionsInput{})
	if err != nil {
		return nil, err
	}

	regions := make([]string, 0)
	for _, region := range result.Regions {
		regions = append(regions, *region.RegionName)
	}
	return regions, nil
}

// QueryField returns node fields(tags, tags:environment, regions)
func (client *AwsCreds) QueryField(ctx context.Context, nodesFilters []*common.Filter, field string) ([]string, error) {
	var err error
	resultArray := make([]string, 0)
	if field == "regions" {
		regionsFilters := make([]*string, 0)
		// if the field is regions, we're requesting all the regions, hence the empty regionsFilters array
		resultArray, err = client.getRegions(ctx, regionsFilters)
		if err != nil {
			return nil, err
		}
		return resultArray, nil
	}
	nodes, err := client.QueryNodes(ctx, nodesFilters, false)
	if err != nil {
		return nil, err
	}
	uniqueMap, err := handleFieldFilterTags(nodes, field)
	if err != nil {
		return nil, err
	}
	for k := range uniqueMap {
		resultArray = append(resultArray, k)
	}
	sort.Strings(resultArray)
	return resultArray, nil
}

func handleFieldFilterTags(nodesMap map[string][]*manager.ManagerNode, field string) (map[string]interface{}, error) {
	uniqueMap := make(map[string]interface{}, 0)
	for _, nodes := range nodesMap {
		if field == "tags" {
			for _, node := range nodes {
				for _, tag := range node.Tags {
					uniqueMap[tag.Key] = nil
				}
			}
		} else if strings.HasPrefix(field, "tags:") {
			tagKey := field[5:]
			for _, node := range nodes {
				for _, tag := range node.Tags {
					if tag.Key == tagKey {
						uniqueMap[tag.Value] = nil
					}
				}
			}
		} else {
			return nil, fmt.Errorf("Invalid field filter")
		}
	}
	return uniqueMap, nil
}

func handleRegionFilters(regions []*string, excRegions []*string) ([]*string, []*string) {
	searchRegions := make([]*string, 0)
	excludedRegions := make([]*string, 0)
	// to do a search on a region, we search against the endpoint value,
	// so we need to prepend the search value with an asterisk to account for the text
	// before the region part of the full endpoint string, and append with an asterisk if the user
	// did not do so already
	var searchRegion *string

	for _, region := range regions {
		wildSearch := *region
		if !strings.HasPrefix(wildSearch, "*") {
			wildSearch = "*" + wildSearch
		}
		if strings.HasSuffix(*region, "*") {
			searchRegion = &wildSearch
		} else {
			valCopy := wildSearch + "*"
			searchRegion = &valCopy
		}
		searchRegions = append(searchRegions, searchRegion)
	}

	// we manually remove the excluded regions from the list of regions
	// to search against, so here we just need to remove any trailing * the user may have attached
	for _, region := range excRegions {
		excRegion := strings.TrimSuffix(*region, "*")
		excludedRegions = append(excludedRegions, &excRegion)
	}
	return searchRegions, excludedRegions
}

// QueryNodes returns nodes based on a map of filters
func (client *AwsCreds) QueryNodes(ctx context.Context, nodesFilters []*common.Filter, ssm bool) (map[string][]*manager.ManagerNode, error) {
	nodes := make(map[string][]*manager.ManagerNode, 0)
	excludedNodes := make([]*manager.ManagerNode, 0)
	ec2Filters, excludedEc2Filters, regions, excludedRegions := nodesFiltersToEc2Filters(nodesFilters)
	searchRegions, excludedRegions := handleRegionFilters(regions, excludedRegions)

	reg, err := client.getRegions(ctx, searchRegions)
	if err != nil {
		return nil, err
	}

	// remove exclude values from list of regions
	for _, region := range excludedRegions {
		reg = stringutils.SliceFilter(reg, func(s string) bool {
			return !strings.Contains(s, *region)
		})
	}
	regions = aws.StringSlice(reg)
	startTime := time.Now()
	poolOfTasks := pool.NewPool(client.getNodeInstancePoolTasks(ctx, regions, ec2Filters, ssm), len(regions))
	poolOfTasks.Run()
	logrus.Debugf("QueryNodes time to run in parallel: %s", time.Since(startTime))

	for _, task := range poolOfTasks.Tasks {
		//here we do a Type Assertion to assert that task.Result holds the type nodesTaskResult.. if so, taskNodes is assigned the type and value of the desired type.
		taskNodes := task.Result.(nodesTaskResult)
		if len(taskNodes.Nodes) > 0 {
			nodes[taskNodes.Region] = taskNodes.Nodes
		}
	}
	if len(excludedEc2Filters) > 1 {
		// b/c aws api does not support exclusion, we are doing it in the code here
		// if len(excludedEc2Filters) > 1 (accounting for the running-instance-state filter), then we know that the value of nodes is all nodes across the specified regions,
		// with no tag filters.  now we fetch the list of nodes based on the non-negated excludedEc2Filters, so we can
		// compare the arrays and remove from nodes the list of instances that comes back from the filtered search
		poolOfTasks := pool.NewPool(client.getNodeInstancePoolTasks(ctx, regions, excludedEc2Filters, ssm), len(regions))
		poolOfTasks.Run()
		logrus.Debugf("QueryNodes time to run in parallel for excluded filters search: %s", time.Since(startTime))
		for _, task := range poolOfTasks.Tasks {
			taskNodes := task.Result.(nodesTaskResult)
			if len(taskNodes.Nodes) > 0 {
				excludedNodes = append(excludedNodes, taskNodes.Nodes...)
			}
		}
		nodes = handleExcludedNodes(nodes, excludedNodes)
	}

	if !poolOfTasks.HasErrors() {
		logrus.Debugf("We had NO errors while retrieving nodes from aws!!")
	} else {
		logrus.Errorf("We had errors retrieving nodes from aws: %+v", poolOfTasks.GetErrors())
	}
	return nodes, nil
}
func handleExcludedNodes(nodes map[string][]*manager.ManagerNode, excludedNodes []*manager.ManagerNode) map[string][]*manager.ManagerNode {
	for _, excludedNode := range excludedNodes {
		nodes[excludedNode.Region] = removeMatchingNodeMngr(nodes[excludedNode.Region], excludedNode)
	}
	return nodes
}

func removeMatchingNodeMngr(arr []*manager.ManagerNode, matcher *manager.ManagerNode) []*manager.ManagerNode {
	newNodesArr := make([]*manager.ManagerNode, 0)
	for _, arrNode := range arr {
		if arrNode.Id != matcher.Id {
			newNodesArr = append(newNodesArr, arrNode)
		}
	}
	return newNodesArr
}

// nodesFiltersToEc2Filters returns nodes based on a map of filters
func nodesFiltersToEc2Filters(nodesFilters []*common.Filter) ([]*ec2.Filter, []*ec2.Filter, []*string, []*string) {
	ec2Filters := make([]*ec2.Filter, 0)
	excludedEc2Filters := make([]*ec2.Filter, 0)
	regions := make([]*string, 0)
	excludedRegions := make([]*string, 0)

	for _, filter := range nodesFilters {
		if filter == nil {
			logrus.Warn("nodesFiltersToEc2Filters: received a node filter == nil")
			continue
		}

		zaMap := kvArrayToMap(filter)
		for key, values := range zaMap {
			if key == "region" {
				if filter.Exclude {
					excludedRegions = values
				} else {
					regions = values
				}
			} else {
				if filter.Exclude {
					excludedEc2Filters = append(excludedEc2Filters, &ec2.Filter{
						Name:   aws.String(fmt.Sprintf("tag:%s", key)),
						Values: values,
					})
				} else {
					ec2Filters = append(ec2Filters, &ec2.Filter{
						Name:   aws.String(fmt.Sprintf("tag:%s", key)),
						Values: values,
					})
				}
			}
		}
	}

	// Hardcoding this filter to ensure that we only get running instances
	ec2Filters = append(ec2Filters, &ec2.Filter{
		Name:   aws.String("instance-state-name"),
		Values: []*string{aws.String("running")},
	})
	excludedEc2Filters = append(excludedEc2Filters, &ec2.Filter{
		Name:   aws.String("instance-state-name"),
		Values: []*string{aws.String("running")},
	})
	return ec2Filters, excludedEc2Filters, regions, excludedRegions
}

func getSSMStatusForInstances(ctx context.Context, region string) (map[string]string, error) {
	ssmInstances := make(map[string]string, 0)
	// call out to node with ssm api to see if it supports ssm
	sess := session.Must(session.NewSession(&aws.Config{Region: aws.String(region)}))
	svc := ssm.New(sess)
	// not specifying any instance ids will result in getting the instance information for all instances
	nextToken := aws.String("no_token_to_start_with")
	params := &ssm.DescribeInstanceInformationInput{}
	for nextToken != nil {
		instances, err := svc.DescribeInstanceInformationWithContext(ctx, params)
		nextToken = instances.NextToken
		if instances.NextToken != nil {
			logrus.Debugf("NextToken received, len(instances): %d", len(instances.InstanceInformationList))
			params.NextToken = nextToken
		}
		if err != nil {
			return ssmInstances, errors.Wrap(err, "getSSMStatusForInstances unable to call DescribeInstanceInformation")
		}
		for _, inst := range instances.InstanceInformationList {
			ssmInstances[*inst.InstanceId] = *inst.PingStatus
		}
	}
	return ssmInstances, nil
}

func (client *AwsCreds) getNodeInstancePoolTasks(ctx context.Context, regions []*string, filters []*ec2.Filter, ssmBool bool) []*pool.Task {
	var tasks = make([]*pool.Task, 0)
	for _, region := range regions {
		ec2Client, err := client.newClient(*region)
		if err != nil {
			logrus.Errorf("Could not connect to ec2 for region: %s", *region)
			continue
		}

		logrus.Debugf("Getting nodes for AWS region: %s", *ec2Client.Config.Region)
		f := func() (pool.TaskResult, error) {
			var ntr = nodesTaskResult{}
			nodes := make([]*manager.ManagerNode, 0)
			ssmInstances := make(map[string]string, 0)
			var ssmErr error

			if ssmBool {
				ssmInstances, ssmErr = getSSMStatusForInstances(ctx, *ec2Client.Config.Region)
				if ssmErr != nil {
					// only logging the error here, we will not always have access to ssm status for instances,
					// this is reserved for users running in aws ec2
					logrus.Warnf("getNodeInstancePoolTasks unable to get ssm status for instances, %s", ssmErr.Error())
				}
			}

			nextToken := aws.String("no_token_to_start_with")
			// Can't use MaxResults param for DescribeInstancesInput as Filters might contain tags and this is not supported by the AWS API
			params := &ec2.DescribeInstancesInput{Filters: filters}
			for nextToken != nil {
				instances, err := ec2Client.DescribeInstancesWithContext(ctx, params)
				nextToken = instances.NextToken
				if instances.NextToken != nil {
					logrus.Debugf("NextToken received, len(nodes): %d", len(nodes))
					params.NextToken = nextToken
				}
				if err != nil {
					logrus.Errorf("getNodeInstancePoolTasks unable to describe instances")
					ntr.Nodes = nodes
					ntr.Region = *region
					return pool.TaskResult(ntr), err
				}

				for idx := range instances.Reservations {
					for _, inst := range instances.Reservations[idx].Instances {
						nodes = append(nodes, parseInstanceInfo(inst, *ec2Client.Config.Region, ssmInstances))
					}
				}
			}
			ntr.Nodes = nodes
			ntr.Region = *ec2Client.Config.Region

			//Here we convert type (cast) from nodesTaskResult to pool.TaskResult
			return pool.TaskResult(ntr), nil
		}
		tasks = append(tasks, pool.NewTask(f))
	}
	return tasks
}

func parseInstanceInfo(inst *ec2.Instance, region string, ssmInstances map[string]string) *manager.ManagerNode {
	var platform, ip, name string
	if inst.PublicIpAddress != nil {
		ip = *inst.PublicIpAddress
	}
	if len(ip) == 0 && inst.PrivateIpAddress != nil {
		logrus.Infof("public ip address not found for node %s; getting private ip address", *inst.InstanceId)
		ip = *inst.PrivateIpAddress
	}
	instanceTags := make([]*common.Kv, 0)
	for _, tag := range inst.Tags {
		instanceTags = append(instanceTags, &common.Kv{Key: *tag.Key, Value: *tag.Value})
		if *tag.Key == "Name" {
			name = *tag.Value
		}
	}
	if inst.Platform != nil {
		platform = *inst.Platform
	}
	if len(name) == 0 {
		name = *inst.PublicDnsName
	}
	logrus.Infof("Registering %s node %s from aws account with ip %s", platform, name, ip)
	ssmConnectionStatus := ssmInstances[*inst.InstanceId]
	return &manager.ManagerNode{
		Name:     name,
		Id:       *inst.InstanceId,
		Host:     *inst.PublicDnsName,
		PublicIp: ip,
		Tags:     instanceTags,
		Platform: platform,
		Region:   region,
		Ssm:      ssmConnectionStatus,
	}
}

func kvArrayToMap(filter *common.Filter) map[string][]*string {
	zaMap := make(map[string][]*string, 0)
	for _, val := range filter.Values {
		// without this value copy, the map array will have pointers to the last added item. Think due to clever go memory management
		valCopy := val
		zaMap[filter.Key] = append(zaMap[filter.Key], &valCopy)
	}
	return zaMap
}

// TestConnectivity tests if we can reach AWS and can get regions and nodes
func (creds *AwsCreds) TestConnectivity(ctx context.Context) error {
	client, err := creds.newClient(creds.Region)
	if err != nil {
		return err
	}

	_, err = client.DescribeRegionsWithContext(ctx, &ec2.DescribeRegionsInput{DryRun: aws.Bool(true)})
	if err != nil {
		awsErr, ok := err.(awserr.Error)
		if !ok || awsErr.Code() != "DryRunOperation" {
			return utils.ProcessUnauthenticated(awsErr, "Unsuccessful DescribeRegions check")
		}
	}

	_, err = client.DescribeInstancesWithContext(ctx, &ec2.DescribeInstancesInput{DryRun: aws.Bool(true)})
	if err != nil {
		awsErr, ok := err.(awserr.Error)
		if !ok || awsErr.Code() != "DryRunOperation" {
			return utils.ProcessUnauthenticated(awsErr, "Unsuccessful DescribeInstances check")
		}
	}

	_, err = client.DescribeInstanceStatusWithContext(ctx, &ec2.DescribeInstanceStatusInput{DryRun: aws.Bool(true)})
	if err != nil {
		awsErr, ok := err.(awserr.Error)
		if !ok || awsErr.Code() != "DryRunOperation" {
			return utils.ProcessUnauthenticated(awsErr, "Unsuccessful DescribeInstanceStatus check")
		}
	}
	return nil
}

type statesTaskResult struct {
	States []pgdb.InstanceState
}

func (client *AwsCreds) getInstanceStatusPoolTasks(ctx context.Context, regions []string) []*pool.Task {
	var tasks = make([]*pool.Task, 0)
	for _, region := range regions {
		ec2Client, err := client.newClient(region)
		if err != nil {
			logrus.Errorf("Could not connect to ec2 for region: %s", region)
			continue
		}

		logrus.Debugf("Getting nodes for AWS region: %s", *ec2Client.Config.Region)
		regionVal := region
		f := func() (pool.TaskResult, error) {
			var str = statesTaskResult{}
			nextToken := aws.String("no_token_to_start_with")
			instanceStates := make([]pgdb.InstanceState, 0)
			t := true
			params := &ec2.DescribeInstanceStatusInput{IncludeAllInstances: &t}
			for nextToken != nil {
				instances, err := ec2Client.DescribeInstanceStatusWithContext(ctx, params)
				nextToken = instances.NextToken
				if instances.NextToken != nil {
					logrus.Debugf("NextToken received, len(instanceStates): %d", len(instanceStates))
					params.NextToken = nextToken
				}
				if err != nil {
					str.States = instanceStates
					return pool.TaskResult(str), err
				}
				for idx := range instances.InstanceStatuses {
					instanceStates = append(instanceStates, pgdb.InstanceState{
						ID:     *instances.InstanceStatuses[idx].InstanceId,
						State:  *instances.InstanceStatuses[idx].InstanceState.Name,
						Region: regionVal,
					})
				}
			}
			str.States = instanceStates
			return pool.TaskResult(str), nil
		}
		tasks = append(tasks, pool.NewTask(f))
	}
	return tasks
}

// QueryStatus returns an array of instanceState based on regions and instanceIds given to it
func (client *AwsCreds) QueryStatus(ctx context.Context, regions []string) ([]pgdb.InstanceState, error) {
	var err error
	instanceStates := make([]pgdb.InstanceState, 0)
	startTime := time.Now()
	if len(regions) == 0 {
		regions, err = client.getRegions(ctx, []*string{})
		if err != nil {
			logrus.Errorf("getInstanceStatusPoolTasks unable to get regions")
		}
	}
	poolOfTasks := pool.NewPool(client.getInstanceStatusPoolTasks(ctx, regions), len(regions))
	poolOfTasks.Run()
	logrus.Debugf("QueryStatus time to run in parallel: %s", time.Since(startTime))

	for _, task := range poolOfTasks.Tasks {
		//here we do a Type Assertion to assert that task.Result holds the type statesTaskResult.. if so, taskStates is assigned the type and value of the desired type.
		taskStates := task.Result.(statesTaskResult)
		if len(taskStates.States) > 0 {
			instanceStates = append(instanceStates, taskStates.States...)
		}
	}
	logrus.Debugf("Got %d instances back for all regions.", len(instanceStates))

	if !poolOfTasks.HasErrors() {
		logrus.Debugf("We had NO errors while retrieving nodes from aws!!")
	} else {
		// TODO (@vj): do we return the error (instead of only logging), if only one of the tasks
		// had an error? we were previously completely ignoring the error. I will revisit this
		// error handling in the next cleanup, and either log with comment why or return
		logrus.Errorf("We had errors retrieving nodes from aws: %s", poolOfTasks.GetErrors())
	}
	return instanceStates, nil
}

type SSM struct {
}

func NewSSM() *SSM {
	return new(SSM)
}

func (s *SSM) SendSSMJob(ctx context.Context, job *types.InspecJob, script string, scriptType string) error {
	*job.NodeStatus = types.StatusRunning
	output, err := s.Send(ctx, job, script, scriptType)
	if err != nil {
		if awsErr, ok := err.(awserr.Error); ok {
			if awsErr.Code() == ssm.ErrCodeInvalidInstanceId {
				// we don't want to return the error here,
				// we want to mark the node as failed, and return the awserr
				logrus.Errorf("unreachable node detected: %s", job.SourceID)
			} else {
				logrus.Errorf("unable to run scan: %s %s", job.SourceID, err.Error())
			}
			*job.NodeStatus = types.StatusFailed
			return awsErr
		}
	}
	if output != nil && output.Command != nil {
		commandID := *output.Command.CommandId
		status := *output.Command.Status
		statusDetails := *output.Command.StatusDetails
		// call ssm api to find out if job done
		// https://docs.aws.amazon.com/systems-manager/latest/APIReference/API_Command.html
		// Pending | InProgress | Success | Cancelled | Failed | TimedOut | Cancelling
		for status == "Pending" || status == "InProgress" || status == "" { // we include the empty string check to ensure we keep going on throttle errors
			// keep polling for status
			time.Sleep(3) // sleep a little to avoid excessive throttling errors
			status, statusDetails, err = s.GetCommandStatus(ctx, commandID, job.TargetConfig.Region)
			if err != nil {
				// return any error except throttling. we'll re-loop for those
				if awsErr, ok := err.(awserr.Error); ok {
					if awsErr.Code() != "ThrottlingException" { // not sure why i can't find this one defined somewhere..
						logrus.Errorf("SendSSMJob unable to get command status: %s", err.Error())
						*job.NodeStatus = types.StatusFailed
						return awsErr
					}
				}
			}
		}
		// the ssm command will return the exit code of the command script, which we are modifying to accommodate our needs
		// so success should be successful and failure should be an inspec execution failure (not control failures)
		if status == "Success" {
			*job.NodeStatus = types.StatusCompleted
		} else {
			*job.NodeStatus = types.StatusFailed
			return fmt.Errorf("aws ssm job id %s for node %s failed with status code %s - details %s", commandID, job.NodeName, status, statusDetails)
		}
	}
	return nil
}

const (
	SSMLinuxShellScript = "AWS-RunShellScript"
	SSMPowerShellScript = "AWS-RunPowerShellScript"
)

func (s *SSM) Send(ctx context.Context, job *types.InspecJob, script string, scriptType string) (*ssm.SendCommandOutput, error) {
	conf := &aws.Config{Region: aws.String(job.TargetConfig.Region)}
	sess := session.Must(session.NewSession(conf))
	svc := ssm.New(sess)

	input := new(ssm.SendCommandInput)
	input.SetComment("run script to execute inspec and report to automate")
	scriptTypeAWS := SSMLinuxShellScript
	if scriptType == inspec.PowershellScript {
		scriptTypeAWS = SSMPowerShellScript
	}
	input.SetDocumentName(scriptTypeAWS)

	// this is left here, commented out, intentionally
	// if ever we have trouble with this, we can uncomment this line to see the
	// output of all the jobs
	// input.SetOutputS3BucketName("vj-test-ssm")

	// set instance id of node
	input.SetInstanceIds([]*string{aws.String(job.SourceID)})
	params := make(map[string][]*string)

	// convert it to a string pointer for AWS
	ssmScript := []*string{}
	ssmScript = append(ssmScript, &script)

	params["commands"] = ssmScript
	input.SetParameters(params)

	err := input.Validate()
	if err != nil {
		return nil, errors.Wrap(err, "Send (ssm job) unable to validate input")
	}
	// and send the command
	return svc.SendCommandWithContext(ctx, input)
}

func (s *SSM) GetCommandStatus(ctx context.Context, commandID string, region string) (string, string, error) {
	var status string
	var statusDetails string
	conf := &aws.Config{Region: aws.String(region)}
	sess := session.Must(session.NewSession(conf))
	svc := ssm.New(sess)
	output, err := svc.ListCommandsWithContext(ctx, &ssm.ListCommandsInput{CommandId: aws.String(commandID)})
	if err != nil {
		return status, statusDetails, err
	}
	for _, cmd := range output.Commands {
		if *cmd.CommandId == commandID {
			return *cmd.Status, *cmd.StatusDetails, nil
		}
	}

	return status, statusDetails, nil
}
