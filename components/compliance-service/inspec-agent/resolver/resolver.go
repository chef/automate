package resolver

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/compliance-service/scanner"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/chef/automate/components/nodemanager-service/managers"
	"github.com/chef/automate/components/nodemanager-service/mgrtypes"
	"github.com/chef/automate/lib/errorutils"
	"github.com/chef/automate/lib/stringutils"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type Resolver struct {
	managerClient manager.NodeManagerServiceClient
	nodesClient   nodes.NodesServiceClient
	db            *pgdb.DB
	scannerServer *scanner.Scanner
	secretsClient secrets.SecretsServiceClient
}

func New(managerClient manager.NodeManagerServiceClient, nodesClient nodes.NodesServiceClient, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) *Resolver {
	scannerServer := scanner.New(managerClient, nodesClient, db)
	return &Resolver{managerClient, nodesClient, db, scannerServer, secretsClient}
}

type managerNodes struct {
	nodes   map[string][]*manager.ManagerNode
	manager *manager.NodeManager
}

type managerRegions struct {
	nodes   []string
	manager *manager.NodeManager
}

type managerSubs struct {
	nodes   []mgrtypes.Sub
	manager *manager.NodeManager
}

func (r *Resolver) ResolveJob(ctx context.Context, job *jobs.Job) ([]*types.InspecJob, error) {
	var nodeJobs []*types.InspecJob
	var err error
	var nodeAgentJobs []*types.InspecJob
	logrus.Debugf("Resolving job into inspec tasks: %+v", job)
	if job == nil {
		return nil, fmt.Errorf("Failed to resolve job, it is nil")
	}

	// note: the order here matters, b/c of the way we query manual node manager
	// we need to check for the static nodes list before we do the check for the node selectors,
	// or we'll end up duplicating the list of nodes to scan for the job
	if job.Nodes != nil && len(job.Nodes) > 0 {
		logrus.Debugf("resolveStaticJob %s: %s", job.Type, job.Id)
		nodeAgentJobs, err = r.resolveStaticJob(ctx, job)
		if err != nil {
			return nil, err
		}
		nodeJobs = append(nodeJobs, nodeAgentJobs...)
	}
	if job.NodeSelectors != nil && len(job.NodeSelectors) > 0 {
		logrus.Debugf("resolveDynamicJob %s: %s", job.Type, job.Id)
		nodeAgentJobs, err = r.resolveDynamicJob(ctx, job)
		if err != nil {
			return nil, err
		}
		nodeJobs = append(nodeJobs, nodeAgentJobs...)
	}

	if job.NodeSelectors == nil && job.Nodes == nil {
		logrus.Warnf("Nothing to do for job %s, empty list of nodes/selectors", job.Id)
		return []*types.InspecJob{}, fmt.Errorf("cannot process this job, nodes and selectors are both nil")
	}
	r.scannerServer.UpdateJobNodeCount(job.Id, len(nodeJobs))
	return nodeJobs, nil
}
func (r *Resolver) handleAzureApiNodes(ctx context.Context, m *manager.NodeManager, filters []*common.Filter, job *jobs.Job) ([]*types.InspecJob, error) {
	nodeCollections := make(map[string]managerSubs)

	more, err := r.querySubs(ctx, m.Id, filters)
	if err != nil {
		return nil, err
	}
	nodeCollections[m.Id] = managerSubs{
		nodes:   more,
		manager: m,
	}

	secret, err := r.secretsClient.Read(ctx, &secrets.Id{Id: m.CredentialId})
	if err != nil {
		return nil, fmt.Errorf("unable to fetch credential id:%s %s", m.CredentialId, err.Error())
	}
	clientID, clientSecret, tenantID := managers.GetAzureCreds(secret)

	jobArray := []*types.InspecJob{}
	for _, group := range nodeCollections {
		for _, node := range group.nodes {
			baseJob := types.InspecBaseJob{
				JobID:    job.Id,
				JobName:  job.Name,
				NodeEnv:  "azure-api",
				NodeName: node.Name,
				JobType:  job.Type,
				Status:   job.Status,
			}
			nodeUUID, err := r.scannerServer.GetNodeUUID(ctx, node.ID, "", m.AccountId)
			if err != nil {
				// don't need to fail here, just log and continue
				logrus.Debugf("handleManagerNodes unable to get node id: %s", err.Error())
			}
			if len(nodeUUID) == 0 {
				nodeUUID, err = r.scannerServer.AddNodeToDB(ctx, &inspec.OSInfo{}, &baseJob, scanner.SourceInfo{SourceAccountID: m.AccountId, SourceRegion: node.ID, SourceID: node.ID}, time.Time{})
				if err != nil {
					logrus.Errorf("resolver unable to add node: %s", err.Error())
				}
			}
			baseJob.NodeID = nodeUUID
			jobArray = append(jobArray, &types.InspecJob{
				InspecBaseJob: baseJob,
				Timeout:       job.Timeout,
				Retries:       job.Retries,
				TargetConfig: inspec.TargetConfig{
					TargetBaseConfig: inspec.TargetBaseConfig{
						Backend:        "azure",
						SubscriptionId: node.ID,
					},
					Secrets: inspec.Secrets{
						AzureClientID:     clientID,
						AzureClientSecret: clientSecret,
						AzureTenantID:     tenantID,
					},
				},
				Profiles:        job.Profiles,
				SourceID:        node.ID,
				SourceAccountID: m.AccountId,
			})
		}
	}
	return jobArray, nil
}

// handleGcpApiNodes locates the gcp-api nodes
func (r *Resolver) handleGcpApiNodes(ctx context.Context, m *manager.NodeManager, filters []*common.Filter, job *jobs.Job) ([]*types.InspecJob, error) {
	if len(job.NodeSelectors) == 0 {
		return nil, fmt.Errorf("handleGcpApiNodes: unable to locate manager node selector")
	}

	var nmFilter = []*common.Filter{
		{Key: "manager_id", Exclude: false, Values: []string{job.NodeSelectors[0].ManagerId}},
	}

	dbNodes, err := r.nodesClient.List(ctx, &nodes.Query{PerPage: 1000000, Filters: nmFilter})
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}

	secret, err := r.secretsClient.Read(ctx, &secrets.Id{Id: m.CredentialId})
	if err != nil {
		return nil, fmt.Errorf("handleGcpApiNodes: unable to fetch credential id:%s %s", m.CredentialId, err.Error())
	}

	gcpCred := ""
	for _, oneKV := range secret.Data {
		if oneKV.Key == "GOOGLE_CREDENTIALS_JSON" {
			gcpCred = oneKV.Value
		}
	}
	if gcpCred == "" {
		return nil, fmt.Errorf("handleGcpApiNodes: unable to fetch GOOGLE_CREDENTIALS_JSON for credential id: %s", m.CredentialId)
	}

	jobArray := []*types.InspecJob{}
	for _, node := range dbNodes.Nodes {
		jobArray = append(jobArray, &types.InspecJob{
			InspecBaseJob: types.InspecBaseJob{
				JobID:    job.Id,
				JobName:  job.Name,
				NodeEnv:  "gcp-api",
				NodeName: node.Name,
				JobType:  job.Type,
				Status:   job.Status,
				NodeID:   node.Id,
			},
			Timeout: job.Timeout,
			Retries: job.Retries,
			TargetConfig: inspec.TargetConfig{
				TargetBaseConfig: inspec.TargetBaseConfig{
					Backend:        "gcp",
					SubscriptionId: m.AccountId,
				},
				Secrets: inspec.Secrets{
					GcpCredsJson: gcpCred,
				},
			},
			Profiles:        job.Profiles,
			SourceID:        node.Name,
			SourceAccountID: m.AccountId,
		})
	}
	return jobArray, nil
}

func handleRegionFilters(filters []*common.Filter, regions []string) ([]string, error) {
	var includeFilters []string
	var excludeFilters []string
	for _, filter := range filters {
		if filter.Key != "region" {
			continue
		}

		for _, val := range filter.Values {
			val = strings.TrimSuffix(val, "*")
			if filter.Exclude {
				excludeFilters = append(excludeFilters, val)
			} else {
				includeFilters = append(includeFilters, val)
			}
		}
	}

	if len(includeFilters) > 0 && len(excludeFilters) > 0 {
		return nil, errors.New("using include and exclude filters in the same job is unsupported")
	}

	if len(includeFilters) > 0 {
		allRegions := regions
		regions = make([]string, 0)

		for _, filter := range includeFilters {
			matches := stringutils.SliceFilter(allRegions, func(region string) bool {
				return strings.HasPrefix(region, filter)
			})
			regions = append(regions, matches...)
		}
	}

	for _, filter := range excludeFilters {
		regions = stringutils.SliceFilter(regions, func(region string) bool {
			return !strings.HasPrefix(region, filter)
		})
	}

	return utils.UniqueStringSlice(regions), nil
}

func (r *Resolver) handleAwsApiNodes(ctx context.Context, m *manager.NodeManager, filters []*common.Filter, job *jobs.Job) ([]*types.InspecJob, error) {
	// we have upgraded our logic to be able to work with mult-region scanning, meaning any new integrations
	// created will only create one node per account (instead of one per region)
	// the following logic checks to see if more than one node is associated with the account in our db
	// if it is, we follow the "old path"
	// if it is not, we follow a "new path" where we just prepare the one node for scanning
	var nmFilter = []*common.Filter{
		{Key: "manager_id", Exclude: false, Values: []string{m.Id}},
	}

	dbNodes, err := r.nodesClient.List(ctx, &nodes.Query{PerPage: 1000000, Filters: nmFilter})
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	if len(dbNodes.Nodes) == 0 {
		return nil, fmt.Errorf("no nodes found in db for manager %s", m.Name)
	}
	if len(dbNodes.Nodes) == 1 {
		return r.handleAwsApiNodesSingleNode(ctx, m, job, dbNodes.Nodes[0])
	}
	return r.handleAwsApiNodesMultiNode(ctx, m, filters, job)
}

func (r *Resolver) handleAwsApiNodesSingleNode(ctx context.Context, m *manager.NodeManager, job *jobs.Job, node *nodes.Node) ([]*types.InspecJob, error) {
	var err error
	var secret *secrets.Secret
	var accessKeyID, secretKey, sessionToken string
	if len(m.CredentialId) > 0 {
		secret, err = r.secretsClient.Read(ctx, &secrets.Id{Id: m.CredentialId})
		if err != nil {
			return nil, fmt.Errorf("unable to fetch credential id:%s %s", m.CredentialId, err.Error())
		}
		accessKeyID, secretKey, _, sessionToken = managers.GetAWSCreds(secret)
	}

	return []*types.InspecJob{&types.InspecJob{
		InspecBaseJob: types.InspecBaseJob{
			JobID:    job.Id,
			JobName:  job.Name,
			NodeEnv:  "aws-api",
			NodeName: node.Name,
			JobType:  job.Type,
			Status:   job.Status,
			NodeID:   node.Id,
		},
		Timeout: job.Timeout,
		Retries: job.Retries,
		TargetConfig: inspec.TargetConfig{
			TargetBaseConfig: inspec.TargetBaseConfig{
				Backend: "aws",
				Region:  "us-east-1",
			},
			Secrets: inspec.Secrets{
				AwsUser:         accessKeyID,
				AwsPassword:     secretKey,
				AwsSessionToken: sessionToken,
			},
		},
		Profiles:        job.Profiles,
		SourceID:        m.AccountId,
		SourceAccountID: m.AccountId,
	}}, nil
}

func (r *Resolver) handleAwsApiNodesMultiNode(ctx context.Context, m *manager.NodeManager, filters []*common.Filter, job *jobs.Job) ([]*types.InspecJob, error) {
	nodeCollections := make(map[string]managerRegions)
	regions, acctAlias, err := r.queryRegions(ctx, m.Id, filters)
	if err != nil {
		return nil, err
	}
	// handle included regions manually
	regions, err = handleRegionFilters(filters, regions)
	if err != nil {
		return nil, err
	}

	nodeCollections[m.Id] = managerRegions{
		nodes:   regions,
		manager: m,
	}

	var secret *secrets.Secret
	var accessKeyID, secretKey, name, sessionToken string
	if len(m.CredentialId) > 0 {
		secret, err = r.secretsClient.Read(ctx, &secrets.Id{Id: m.CredentialId})
		if err != nil {
			return nil, fmt.Errorf("unable to fetch credential id:%s %s", m.CredentialId, err.Error())
		}
		accessKeyID, secretKey, _, sessionToken = managers.GetAWSCreds(secret)
	}

	jobArray := []*types.InspecJob{}
	for _, group := range nodeCollections {
		for _, node := range group.nodes {
			if len(acctAlias) > 0 {
				name = fmt.Sprintf("%s:%s", acctAlias, node)
			} else {
				name = node
			}
			baseJob := types.InspecBaseJob{
				JobID:    job.Id,
				JobName:  job.Name,
				NodeEnv:  "aws-api",
				NodeName: name,
				JobType:  job.Type,
				Status:   job.Status,
			}
			nodeUUID, err := r.scannerServer.GetNodeUUID(ctx, node, node, m.AccountId)
			if err != nil {
				// don't need to fail here, just log and continue
				logrus.Errorf("handleManagerNodes unable to get node id: %s", err.Error())
			}
			if len(nodeUUID) == 0 {
				nodeUUID, err = r.scannerServer.AddNodeToDB(ctx, &inspec.OSInfo{}, &baseJob, scanner.SourceInfo{SourceAccountID: m.AccountId, SourceRegion: node, SourceID: node}, time.Time{})
				if err != nil {
					logrus.Errorf("resolver unable to add node: %s", err.Error())
				}
			}
			baseJob.NodeID = nodeUUID

			jobArray = append(jobArray, &types.InspecJob{
				InspecBaseJob: baseJob,
				Timeout:       job.Timeout,
				Retries:       job.Retries,
				TargetConfig: inspec.TargetConfig{
					TargetBaseConfig: inspec.TargetBaseConfig{
						Backend: "aws",
						Region:  node,
					},
					Secrets: inspec.Secrets{
						AwsUser:         accessKeyID,
						AwsPassword:     secretKey,
						AwsSessionToken: sessionToken,
					},
				},
				Profiles:        job.Profiles,
				SourceID:        node,
				SourceAccountID: m.AccountId,
			})
		}
	}
	return jobArray, nil
}

func (r *Resolver) handleManuallyManagedNodes(ctx context.Context, m *manager.NodeManager, filters []*common.Filter, job *jobs.Job) ([]*types.InspecJob, error) {
	var perPage int32 = 100
	var total int64 = 100
	nodesList := make([]*nodes.Node, 0)
	nodeIds := make([]string, 0)
	for cnt := int32(1); int64(len(nodesList)) < total; cnt++ {
		logrus.Debugf("getting nodes with page %d for total %d, per_page %d", cnt, total, perPage)
		filters = append(filters, &common.Filter{Key: "manager_id", Values: []string{mgrtypes.AutomateManagerID}})
		pageNodes, err := r.nodesClient.List(ctx, &nodes.Query{Page: cnt, PerPage: perPage, Filters: filters})
		if err != nil {
			logrus.Errorf("handleManuallyManagedNodes unable to get nodes with query: %+v  aborting job: %s", filters, err.Error())
			return nil, err
		}
		total = int64(pageNodes.Total)
		nodesList = append(nodesList, pageNodes.Nodes...)
	}
	for _, node := range nodesList {
		nodeIds = append(nodeIds, node.Id)
	}
	job.Nodes = nodeIds
	return r.resolveStaticJob(ctx, job)
}

func (r *Resolver) handleAwsEc2Nodes(ctx context.Context, m *manager.NodeManager, filters []*common.Filter, job *jobs.Job) ([]*types.InspecJob, error) {
	nodeCollections := make(map[string]managerNodes)
	nodesPerManager := make(map[string][]*manager.ManagerNode)
	nodesResp, err := r.queryAWSNodes(ctx, m.Id, filters)
	if err != nil {
		return nil, err
	}
	nodesPerManager[m.Id] = nodesResp
	nodeCollections[m.Id] = managerNodes{
		nodes:   nodesPerManager,
		manager: m,
	}
	return r.handleManagerNodes(ctx, m, nodeCollections, job)
}

func (r *Resolver) handleAzureVmNodes(ctx context.Context, m *manager.NodeManager, filters []*common.Filter, job *jobs.Job) ([]*types.InspecJob, error) {
	nodeCollections := make(map[string]managerNodes)
	nodesPerManager := make(map[string][]*manager.ManagerNode)
	nodesResp, err := r.queryAzureNodes(ctx, m.Id, filters)
	if err != nil {
		return nil, err
	}
	nodesPerManager[m.Id] = nodesResp
	nodeCollections[m.Id] = managerNodes{
		nodes:   nodesPerManager,
		manager: m,
	}
	return r.handleManagerNodes(ctx, m, nodeCollections, job)
}

func (r *Resolver) handleManagerNodes(ctx context.Context, m *manager.NodeManager, nodeCollections map[string]managerNodes, job *jobs.Job) ([]*types.InspecJob, error) {
	jobArray := []*types.InspecJob{}
	var baseJob types.InspecBaseJob
	var creds *inspec.Secrets
	var clientID, clientSecret, tenantID string
	if len(m.CredentialId) == 0 && m.Type == "azure-vm" {
		logrus.Infof("GetAzureCreds attempting to use environment credentials")
	} else {
		mgrCreds, err := r.secretsClient.Read(ctx, &secrets.Id{Id: m.CredentialId})
		if err != nil {
			logrus.Errorf("Failed to get manager credentials for node manager %s: %s", m.Id, err.Error())
		}
		if m.Type == "azure-vm" {
			clientID, clientSecret, tenantID = managers.GetAzureCreds(mgrCreds)
		}
	}

	for _, group := range nodeCollections {
		for _, nodesArr := range group.nodes {
			for _, node := range nodesArr {
				var backend, nodeEnv, nodeName string
				backend = inspec.BackendSSH
				if node.Platform == "windows" {
					backend = inspec.BackendWinRm
				}
				logrus.Infof("inspec agent resolver handling node with backend: %s -- ssm ping status: %s", backend, node.Ssm)
				nodeName = node.Name
				var credsArr []*inspec.Secrets
				for _, credTagGroup := range group.manager.InstanceCredentials {
					for _, kv := range node.Tags {
						if kv.Key == "Name" {
							nodeName = kv.Value
						}
						if kv.Key == "Environment" {
							nodeEnv = kv.Value
						}
						isMatch := utils.KvMatches(credTagGroup.TagKey, credTagGroup.TagValue, kv)
						if isMatch {
							for _, cred := range credTagGroup.CredentialIds {
								secret, err := r.secretsClient.Read(ctx, &secrets.Id{Id: cred})
								if err != nil {
									logrus.Errorf("Failed to get node credentials for node manager %s node %s: %s", group.manager.Id, node.Id, err.Error())
									continue
								}
								creds, err = getNodeCredentials(secret)
								if err != nil {
									logrus.Errorf("Failed to get node credentials for node manager %s node %s: %s", group.manager.Id, node.Id, err.Error())
									continue
								}
								credsArr = append(credsArr, creds)
							}
						}
					}
				}
				if len(nodeName) == 0 {
					nodeName = node.Host
				}
				ssmJob := false
				// if the user has specified ssh/winrm secrets to be associated with the node
				// then let's prioritize that -- otherwise try ssm
				if len(credsArr) == 0 {
					var skip bool
					ssmJob, skip = handleSSMNodes(node, job, &backend)
					if skip {
						logrus.Warnf("action not supported: cannot run a detect job on ssm node %s", node.Name)
						continue
					}
				}
				baseJob = types.InspecBaseJob{
					JobID:    job.Id,
					JobName:  job.Name,
					NodeEnv:  nodeEnv,
					NodeName: nodeName,
					JobType:  job.Type,
					Status:   job.Status,
				}
				nodeUUID, err := r.scannerServer.GetNodeUUID(ctx, node.Id, node.Region, m.AccountId)
				if err != nil {
					// don't need to fail here, just log and continue
					logrus.Debugf("handleManagerNodes unable to get node id: %s", err.Error())
				}
				if len(nodeUUID) == 0 {
					nodeUUID, err = r.scannerServer.AddNodeToDB(ctx, &inspec.OSInfo{}, &baseJob, scanner.SourceInfo{SourceAccountID: m.AccountId, SourceRegion: node.Region, SourceID: node.Id}, time.Time{})
					if err != nil {
						logrus.Errorf("resolver unable to add node: %s", err.Error())
					}
				}
				baseJob.NodeID = nodeUUID
				fullJob := types.InspecJob{
					EndTime:       &time.Time{},
					InspecBaseJob: baseJob,
					SSM:           ssmJob,
					Timeout:       job.Timeout,
					Retries:       job.Retries,
					TargetConfig: inspec.TargetConfig{
						TargetBaseConfig: inspec.TargetBaseConfig{
							Hostname:       node.PublicIp,
							Backend:        backend,
							Region:         node.Region,
							SubscriptionId: node.Group,
						},
						SecretsArr: credsArr,
						Secrets: inspec.Secrets{
							AzureClientID:     clientID,
							AzureClientSecret: clientSecret,
							AzureTenantID:     tenantID,
						},
					},
					Profiles:          job.Profiles,
					SourceID:          node.Id,
					SourceAccountID:   m.AccountId,
					MachineIdentifier: node.MachineIdentifier,
					Tags:              node.Tags,
				}
				jobArray = append(jobArray, &fullJob)
			}
		}
	}
	return jobArray, nil
}

func handleSSMNodes(node *manager.ManagerNode, job *jobs.Job, backend *string) (ssmJob bool, skip bool) {
	skip = false
	switch node.Ssm {
	case "Online":
		// if the ping status is online, we want this to be a ssm job (aws)
		ssmJob = true
		*backend = inspec.BackendSSM
		if node.Platform == "windows" {
			*backend = inspec.BackendSSMWindows
		}
	case "Online:Azure":
		ssmJob = true
		*backend = inspec.BackendAZ
		if node.Platform == "windows" {
			*backend = inspec.BackendAZWindows
		}
	}
	if ssmJob == true && job.Type == "detect" {
		skip = true
	}
	return
}

type FiltersByManager struct {
	Manager *manager.NodeManager
	Filters []*common.Filter
}

func (r *Resolver) resolveDynamicJob(ctx context.Context, job *jobs.Job) ([]*types.InspecJob, error) {
	nodeManagers := make([]FiltersByManager, len(job.GetNodeSelectors()))
	for index, mgrFilter := range job.GetNodeSelectors() {
		mgr, err := r.managerClient.Read(ctx, &manager.Id{Id: mgrFilter.GetManagerId()})
		if err != nil {
			return nil, fmt.Errorf("Failed to retrieve node manager: %s", err)
		}
		mgrFilters := mgrFilter.GetFilters()
		nodeManagers[index] = FiltersByManager{
			Manager: mgr,
			Filters: mgrFilters,
		}
	}

	jobs := make([]*types.InspecJob, 0)
	var agentJobs []*types.InspecJob
	var err error
	for _, m := range nodeManagers {
		logrus.Debugf("handling job for manager %+v", m)
		managerType := m.Manager.GetType()
		switch managerType {
		case "aws-ec2":
			agentJobs, err = r.handleAwsEc2Nodes(ctx, m.Manager, m.Filters, job)
		case "azure-vm":
			agentJobs, err = r.handleAzureVmNodes(ctx, m.Manager, m.Filters, job)
		case "aws-api":
			agentJobs, err = r.handleAwsApiNodes(ctx, m.Manager, m.Filters, job)
		case "azure-api":
			agentJobs, err = r.handleAzureApiNodes(ctx, m.Manager, m.Filters, job)
		case "gcp-api":
			agentJobs, err = r.handleGcpApiNodes(ctx, m.Manager, m.Filters, job)
		case "automate":
			agentJobs, err = r.handleManuallyManagedNodes(ctx, m.Manager, m.Filters, job)
		default:
			strErr := "resolveDynamicJob unable to resolve node manager type: " + managerType
			logrus.Debugf(strErr)
			return nil, fmt.Errorf(strErr)
		}
		if err != nil {
			return nil, err
		}
		jobs = append(jobs, agentJobs...)
	}

	// Keeping the child - parent job reference alive in the resolved jobs array
	for _, j := range jobs {
		j.ParentJobID = job.Id
	}

	return jobs, nil
}

func (r *Resolver) resolveStaticJob(ctx context.Context, job *jobs.Job) ([]*types.InspecJob, error) {
	jobArray := make([]*types.InspecJob, 0)
	for _, id := range job.Nodes {
		node, err := r.managerClient.GetNodeWithSecrets(ctx, &manager.Id{Id: id})
		if err != nil {
			logrus.Errorf("Could not retrieve node %s, due to: %s", id, err.Error())
			continue
		}
		resolvedTC, err := convertNodeTcToInspecTc(node.TargetConfig)
		if err != nil {
			logrus.Errorf("Could not resolve target config for node %s, due to: %s", id, err.Error())
			continue
		}
		if resolvedTC.Backend == "ssm" && job.Type == "detect" {
			logrus.Warnf("action not supported: cannot run a detect job on ssm node %s", node.Name)
			continue
		}
		agentJob := r.resolveStaticJobInfo(job, node, resolvedTC, id)
		agentJob.ParentJobID = job.Id
		jobArray = append(jobArray, &agentJob)
	}
	return jobArray, nil
}

func convertNodeTcToInspecTc(tc *nodes.TargetConfig) (inspec.TargetConfig, error) {
	inspecTC := inspec.TargetConfig{}
	inspecTC.AwsPassword = tc.AwsPassword
	inspecTC.AwsUser = tc.AwsUser
	inspecTC.Backend = tc.Backend
	inspecTC.Hostname = tc.Host
	inspecTC.KeyFiles = tc.KeyFiles
	inspecTC.GcpCredsJson = tc.GcpCredsJson
	inspecTC.SubscriptionId = tc.SubscriptionId
	if tc.User != "" {
		inspecTC.Password = tc.Password
	}
	inspecTC.LoginPath = tc.Path
	inspecTC.Port = int(tc.Port)
	inspecTC.Region = tc.Region
	secretsArr := make([]*inspec.Secrets, 0)
	for _, secret := range tc.SecretsArr {
		inspecSecret := inspec.Secrets{
			User:              secret.User,
			KeyFiles:          secret.KeyFiles,
			SudoPassword:      secret.SudoPassword,
			SudoOptions:       secret.SudoOptions,
			AwsUser:           secret.AwsUser,
			AwsPassword:       secret.AwsPassword,
			AzureClientID:     secret.AzureClientId,
			AzureClientSecret: secret.AzureClientSecret,
			AzureTenantID:     secret.AzureTenantId,
		}
		if secret.User != "" {
			inspecSecret.Password = secret.Password
		}
		secretsArr = append(secretsArr, &inspecSecret)
	}
	inspecTC.SecretsArr = secretsArr
	inspecTC.SslSelfSigned = tc.SelfSigned
	inspecTC.Ssl = tc.Ssl
	inspecTC.SubscriptionId = tc.SubscriptionId
	inspecTC.Sudo = tc.Sudo
	inspecTC.SudoOptions = tc.SudoOptions
	inspecTC.SudoPassword = tc.SudoPassword
	inspecTC.User = tc.User
	return inspecTC, nil
}

func (r *Resolver) resolveStaticJobInfo(job *jobs.Job, node *nodes.Node, tc inspec.TargetConfig, id string) types.InspecJob {
	env := pgdb.FindKeyValue(node.Tags, "environment").Value

	agentJob := types.InspecJob{
		InspecBaseJob: types.InspecBaseJob{
			JobID:    job.Id,
			JobName:  job.Name,
			NodeEnv:  env,
			NodeName: node.Name,
			JobType:  job.Type,
			NodeID:   id,
			Status:   job.Status,
		},
		Timeout:      job.Timeout,
		Retries:      job.Retries,
		TargetConfig: tc,
		Profiles:     job.Profiles,
		Tags:         node.Tags,
	}
	return agentJob
}

func (r *Resolver) queryAWSNodes(ctx context.Context, nodemgrID string, filters []*common.Filter) ([]*manager.ManagerNode, error) {
	logrus.Debugf("Resolving node manager %s nodes: %#v", nodemgrID, filters)
	nodesResp, err := r.managerClient.SearchManagerNodes(ctx, &manager.NodeQuery{NodeManagerId: nodemgrID, Query: &manager.Query{FilterMap: filters}})
	if err != nil {
		return nil, fmt.Errorf("Failed to query all nodes for job manager %s: %s", nodemgrID, err)
	}
	return nodesResp.Nodes, nil
}

func (r *Resolver) queryAzureNodes(ctx context.Context, nodemgrID string, filters []*common.Filter) ([]*manager.ManagerNode, error) {
	logrus.Debugf("Resolving node manager %s nodes: %#v", nodemgrID, filters)
	nodesResp, err := r.managerClient.SearchManagerNodes(ctx, &manager.NodeQuery{NodeManagerId: nodemgrID, Query: &manager.Query{FilterMap: filters}})
	if err != nil {
		return nil, fmt.Errorf("Failed to query all nodes for job manager %s: %s", nodemgrID, err)
	}
	return nodesResp.Nodes, nil
}

func (r *Resolver) queryRegions(ctx context.Context, nodemgrID string, filters []*common.Filter) ([]string, string, error) {
	logrus.Debugf("Resolving node manager %s regions: %#v", nodemgrID, filters)

	fields, err := r.managerClient.SearchNodeFields(ctx, &manager.FieldQuery{NodeManagerId: nodemgrID, Field: "regions", Query: &manager.Query{FilterMap: filters}})
	if err != nil {
		return nil, "", fmt.Errorf("Failed to query all nodes for job manager %s: %s", nodemgrID, err)
	}
	manager, err := r.managerClient.Read(ctx, &manager.Id{Id: nodemgrID})
	if err != nil {
		return nil, "", fmt.Errorf("queryRegions; unable to get manager info: %s %s", nodemgrID, err.Error())
	}
	acctAlias := manager.AccountAlias
	if len(acctAlias) == 0 {
		// the user may have never set an alias for their account, and that's ok, but we should log it out
		logrus.Warnf("addNodeForEachApiRegion; no account aliases found for manager: %s", nodemgrID)
	}
	return fields.Fields, acctAlias, nil
}

func (r *Resolver) querySubs(ctx context.Context, nodemgrID string, filters []*common.Filter) ([]mgrtypes.Sub, error) {
	logrus.Debugf("Resolving node manager %s subscriptions", nodemgrID)
	nodes, err := r.managerClient.SearchManagerNodes(ctx, &manager.NodeQuery{NodeManagerId: nodemgrID, Query: &manager.Query{FilterMap: filters}})
	if err != nil {
		return nil, fmt.Errorf("Failed to query all nodes for job manager %s: %s", nodemgrID, err)
	}
	subs := make([]mgrtypes.Sub, len(nodes.Nodes))
	for i, sub := range nodes.Nodes {
		subs[i] = mgrtypes.Sub{
			ID:   sub.Id,
			Name: sub.Name,
		}
	}

	return subs, nil
}

func getNodeCredentials(secret *secrets.Secret) (*inspec.Secrets, error) {
	var sshKey, password, sudoOpts, sudoPass, user, path string
	var err error
	switch secret.Type {
	case "ssh":
		for _, item := range secret.Data {
			if item.Key == "key" {
				sshKey = item.Value
				path, err = nodes.PrepareSSHPrivateKey(sshKey)
				if err != nil {
					return nil, fmt.Errorf("getNodeCredentials Failed to prepare SSH key for remote: %s", err)
				}
			}
			if item.Key == "username" {
				user = item.Value
			}
			if item.Key == "password" {
				password = item.Value
			}
		}
	case "winrm":
		for _, item := range secret.Data {
			if item.Key == "username" {
				user = item.Value
			}
			if item.Key == "password" {
				password = item.Value
			}
		}
	case "sudo":
		for _, item := range secret.Data {
			if item.Key == "options" {
				sudoOpts = item.Value
			}
			if item.Key == "password" {
				sudoPass = item.Value
			}
		}
	}

	return &inspec.Secrets{
		User:         user,
		KeyFiles:     []string{path},
		Password:     password,
		SudoOptions:  sudoOpts,
		SudoPassword: sudoPass,
	}, nil
}
