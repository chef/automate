package elastic

import (
	"context"
	"fmt"
	"os"
	"testing"

	"github.com/chef/automate/api/external/cfgmgmt/request"
	"github.com/chef/automate/components/config-mgmt-service/config"
	grpc "github.com/chef/automate/components/config-mgmt-service/grpcserver"
	platform_config "github.com/chef/automate/lib/platform/config"

	"github.com/stretchr/testify/require"
)

var _curl_cmd = `
curl -Ss -k -X GET "http://localhost:10144/node-state/_search" -H 'Content-Type: application/json' -d'
{
    "size": 0,
    "aggs" : {
        "segment": {
            "composite" : {
							"size": 500,
                "sources" : [
                    { "policy_name": { "terms" : { "field": "policy_name"          } } },
                    { "policy_group": { "terms" : { "field": "policy_group"        } } },
                    { "source_fqdn": { "terms" : { "field": "source_fqdn"          } } },
                    { "org": { "terms" : { "field": "organization_name"            } } }
                ]
            },
						"aggregations": {
						  "policy_revisions": {
							  "terms": { "field": "policy_revision"  }
								}
							}
						}
        }
     }
}
' | jq .
`

const (
	pgDatabaseName       = "chef_config_mgmt_service"
	A2_SVC_NAME          = "A2_SVC_NAME"
	A2_SVC_PATH          = "A2_SVC_PATH"
	defaultA2ServiceName = "config-mgmt-service"
	defaultA2ServicePath = "/hab/svc/config-mgmt-service"
)

func TestRolloutAggregate(t *testing.T) {
	elasticsearchUrl := "http://localhost:10144"
	e := New(elasticsearchUrl)
	_ = os.Setenv(A2_SVC_NAME, defaultA2ServiceName)
	_ = os.Setenv(A2_SVC_PATH, defaultA2ServicePath)

	uri, err := platform_config.PGURIFromEnvironment(pgDatabaseName)

	require.NoError(t, err)

	cfg := config.Default()
	cfg.Postgres = config.Postgres{
		URI:        uri,
		Database:   pgDatabaseName,
		SchemaPath: "/src/components/config-mgmt-service/backend/postgres/schema/sql/",
	}

	cfg.SetBackend(e)
	srv := grpc.NewCfgMgmtServer(cfg)
	err = srv.ConnectPg()
	require.NoError(t, err)

	ctx := context.Background()
	res, err := srv.ListNodeSegmentsWithRolloutProgress(ctx, &request.ListNodeSegmentsWithRolloutProgress{})
	require.NoError(t, err)
	fmt.Println("=========== Rollouts ===========")
	fmt.Printf("NODE SEGMENTS: %d\n", len(res.NodeSegmentRolloutProgress))
	for _, ns := range res.NodeSegmentRolloutProgress {
		current := ns.CurrentRolloutProgress
		fmt.Printf("## %s::%s @ %s\n", ns.PolicyName, ns.PolicyNodeGroup, ns.PolicyDomainUrl)
		fmt.Printf("  Total nodes: %d, %d on latest revision\n", ns.TotalNodes, current.NodeCount)
		fmt.Printf("  ### Current rollout #%s started %s\n", current.Rollout.Id, current.Rollout.StartTime)
		fmt.Printf("  - nodes:           %d (successful: %d, failed: %d)\n", current.NodeCount, current.LatestRunSuccessfulCount, current.LatestRunFailedCount)
		fmt.Printf("  - policy revision: %s\n", current.Rollout.PolicyRevisionId)
		fmt.Printf("  - desc:            %q\n", current.Rollout.Description)
		fmt.Printf("  ### Past rollouts: %d\n", len(ns.PreviousRollouts))
		for _, r := range ns.PreviousRollouts {
			fmt.Printf("    ### rollout #%s started %s\n", r.Rollout.Id, r.Rollout.StartTime)
			fmt.Printf("    - policy revision: %s\n", r.Rollout.PolicyRevisionId)
			fmt.Printf("    - desc:            %q\n", r.Rollout.Description)
			fmt.Printf("    - nodes still on this revision: %d\n", r.LatestRunNodeCount)
		}
	}
	fmt.Println("........... RESPONSE ...........")
}

// TODO TEST CASES:
// CCRs with no rollouts
// Rollouts with no CCRs
// 1 rollout/segment up to 6 rollouts/segment
// validation and cleanup of Chef Server URLs especially, but also pname/group
// need some way to do a diagnostic on the matching-ness of the CCR vs. pg rollout data
// another note here that "missing" shouldn't impact anything in this api
