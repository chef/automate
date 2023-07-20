package hardwarecal

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestHardwareCal(t *testing.T) {
	tests := []struct {
		name   string
		req    *HardwareCalReq
		expRes *HardwareCalRes
		expErr error
	}{
		{
			name: "80K_2Comp_24client_24client_1rep_30days",
			req: &HardwareCalReq{
				NoOfNodes:                     80000,
				FreqCountComplianceScanPerDay: 2,
				FreqCountClientRunPerDay:      24,
				FreqCountEventFeedPerDay:      24,
				ComplianceReportSizeKB:        1024,
				ClientRunReportSizeKB:         300,
				EventFeedSizeKB:               100,
				DataRetentionDays:             30,
				NoOfReplicasInOpenSearch:      1,
			},
			expRes: &HardwareCalRes{
				AutomateNode: &Node{
					InstanceCount: 3,
					CpuCount:      4,
					RamGB:         16,
					StorageGB:     200,
					Type:          "m5.xlarge",
				},
				ChefServerNode: &Node{
					InstanceCount: 3,
					CpuCount:      4,
					RamGB:         16,
					StorageGB:     200,
					Type:          "m5.xlarge",
				},
				PostgresqlNode: &Node{
					InstanceCount: 3,
					CpuCount:      8,
					RamGB:         32,
					StorageGB:     1600,
					Type:          "m5.2xlarge",
				},
				OpenSearchNode: &Node{
					InstanceCount: 5,
					CpuCount:      16,
					RamGB:         64,
					StorageGB:     13300,
					Type:          "m5.4xlarge",
				},
			},
			expErr: nil,
		},
		{
			name: "100K_2Comp_24client_24client_1rep_30days",
			req: &HardwareCalReq{
				NoOfNodes:                     100000,
				FreqCountComplianceScanPerDay: 2,
				FreqCountClientRunPerDay:      24,
				FreqCountEventFeedPerDay:      24,
				ComplianceReportSizeKB:        1024,
				ClientRunReportSizeKB:         300,
				EventFeedSizeKB:               100,
				DataRetentionDays:             30,
				NoOfReplicasInOpenSearch:      1,
			},
			expRes: &HardwareCalRes{
				AutomateNode: &Node{
					InstanceCount: 2,
					CpuCount:      8,
					RamGB:         32,
					StorageGB:     200,
					Type:          "m5.2xlarge",
				},
				ChefServerNode: &Node{
					InstanceCount: 2,
					CpuCount:      8,
					RamGB:         32,
					StorageGB:     200,
					Type:          "m5.2xlarge",
				},
				PostgresqlNode: &Node{
					InstanceCount: 3,
					CpuCount:      16,
					RamGB:         64,
					StorageGB:     2000,
					Type:          "m5.4xlarge",
				},
				OpenSearchNode: &Node{
					InstanceCount: 7,
					CpuCount:      16,
					RamGB:         64,
					StorageGB:     11900,
					Type:          "m5.4xlarge",
				},
			},
			expErr: nil,
		},
		{
			name: "50K_2Comp_24client_24client_1rep_30days",
			req: &HardwareCalReq{
				NoOfNodes:                     50000,
				FreqCountComplianceScanPerDay: 2,
				FreqCountClientRunPerDay:      24,
				FreqCountEventFeedPerDay:      24,
				ComplianceReportSizeKB:        1024,
				ClientRunReportSizeKB:         300,
				EventFeedSizeKB:               100,
				DataRetentionDays:             30,
				NoOfReplicasInOpenSearch:      1,
			},
			expRes: &HardwareCalRes{
				AutomateNode: &Node{
					InstanceCount: 2,
					CpuCount:      4,
					RamGB:         16,
					StorageGB:     200,
					Type:          "m5.xlarge",
				},
				ChefServerNode: &Node{
					InstanceCount: 2,
					CpuCount:      4,
					RamGB:         16,
					StorageGB:     200,
					Type:          "m5.xlarge",
				},
				PostgresqlNode: &Node{
					InstanceCount: 3,
					CpuCount:      8,
					RamGB:         32,
					StorageGB:     1000,
					Type:          "m5.2xlarge",
				},
				OpenSearchNode: &Node{
					InstanceCount: 3,
					CpuCount:      16,
					RamGB:         64,
					StorageGB:     13900,
					Type:          "m5.4xlarge",
				},
			},
			expErr: nil,
		},
		{
			name: "50K_2Comp_24client_24client_0rep_30days",
			req: &HardwareCalReq{
				NoOfNodes:                     50000,
				FreqCountComplianceScanPerDay: 2,
				FreqCountClientRunPerDay:      24,
				FreqCountEventFeedPerDay:      24,
				ComplianceReportSizeKB:        1024,
				ClientRunReportSizeKB:         300,
				EventFeedSizeKB:               100,
				DataRetentionDays:             30,
				NoOfReplicasInOpenSearch:      0,
			},
			expRes: &HardwareCalRes{
				AutomateNode: &Node{
					InstanceCount: 2,
					CpuCount:      4,
					RamGB:         16,
					StorageGB:     200,
					Type:          "m5.xlarge",
				},
				ChefServerNode: &Node{
					InstanceCount: 2,
					CpuCount:      4,
					RamGB:         16,
					StorageGB:     200,
					Type:          "m5.xlarge",
				},
				PostgresqlNode: &Node{
					InstanceCount: 3,
					CpuCount:      8,
					RamGB:         32,
					StorageGB:     1000,
					Type:          "m5.2xlarge",
				},
				OpenSearchNode: &Node{
					InstanceCount: 3,
					CpuCount:      8,
					RamGB:         32,
					StorageGB:     6900,
					Type:          "m5.2xlarge",
				},
			},
			expErr: nil,
		},
		{
			name: "20K_2Comp_24client_24client_1rep_30days",
			req: &HardwareCalReq{
				NoOfNodes:                     20000,
				FreqCountComplianceScanPerDay: 2,
				FreqCountClientRunPerDay:      24,
				FreqCountEventFeedPerDay:      24,
				ComplianceReportSizeKB:        1024,
				ClientRunReportSizeKB:         300,
				EventFeedSizeKB:               100,
				DataRetentionDays:             30,
				NoOfReplicasInOpenSearch:      1,
			},
			expRes: &HardwareCalRes{
				AutomateNode: &Node{
					InstanceCount: 2,
					CpuCount:      2,
					RamGB:         8,
					StorageGB:     200,
					Type:          "m5.large",
				},
				ChefServerNode: &Node{
					InstanceCount: 2,
					CpuCount:      2,
					RamGB:         8,
					StorageGB:     200,
					Type:          "m5.large",
				},
				PostgresqlNode: &Node{
					InstanceCount: 3,
					CpuCount:      2,
					RamGB:         8,
					StorageGB:     400,
					Type:          "m5.large",
				},
				OpenSearchNode: &Node{
					InstanceCount: 5,
					CpuCount:      4,
					RamGB:         16,
					StorageGB:     3300,
					Type:          "m5.xlarge",
				},
			},
			expErr: nil,
		},
		{
			name: "200K_2Comp_24client_24client_1rep_30days",
			req: &HardwareCalReq{
				NoOfNodes:                     200000,
				FreqCountComplianceScanPerDay: 2,
				FreqCountClientRunPerDay:      24,
				FreqCountEventFeedPerDay:      24,
				ComplianceReportSizeKB:        1024,
				ClientRunReportSizeKB:         300,
				EventFeedSizeKB:               100,
				DataRetentionDays:             30,
				NoOfReplicasInOpenSearch:      1,
			},
			expRes: nil,
			expErr: errors.New("opensearch node count needed: 15, please see if you can reduce the input requirements so the OpenSearch Nodes needed comes down to max 9 for 64gb ram"),
		},
	}
	for _, tt := range tests {
		res, err := tt.req.NewCal()

		assert.Equal(t, tt.expErr, err)
		if res != nil {
			assert.Equal(t, tt.expRes.AutomateNode.CpuCount, res.AutomateNode.CpuCount, "Automate CPU Count")
			assert.Equal(t, tt.expRes.AutomateNode.InstanceCount, res.AutomateNode.InstanceCount, "Automate Instance Count")
			assert.Equal(t, tt.expRes.AutomateNode.StorageGB, res.AutomateNode.StorageGB, "Automate Storage")
			assert.Equal(t, tt.expRes.AutomateNode.RamGB, res.AutomateNode.RamGB, "Automate RAM")
			assert.Equal(t, tt.expRes.AutomateNode.Type, res.AutomateNode.Type, "Automate Type")
			assert.Equal(t, tt.expRes.ChefServerNode.CpuCount, res.ChefServerNode.CpuCount, "Chef Server CPU Count")
			assert.Equal(t, tt.expRes.ChefServerNode.InstanceCount, res.ChefServerNode.InstanceCount, "Chef Server Instance Count")
			assert.Equal(t, tt.expRes.ChefServerNode.StorageGB, res.ChefServerNode.StorageGB, "Chef Server Storage")
			assert.Equal(t, tt.expRes.ChefServerNode.RamGB, res.ChefServerNode.RamGB, "Chef Server RAM")
			assert.Equal(t, tt.expRes.ChefServerNode.Type, res.ChefServerNode.Type, "Chef Server Type")
			assert.Equal(t, tt.expRes.PostgresqlNode.CpuCount, res.PostgresqlNode.CpuCount, "PostgreSQL CPU Count")
			assert.Equal(t, tt.expRes.PostgresqlNode.InstanceCount, res.PostgresqlNode.InstanceCount, "PostgreSQL Instance Count")
			assert.Equal(t, tt.expRes.PostgresqlNode.StorageGB, res.PostgresqlNode.StorageGB, "PostgreSQL Storage")
			assert.Equal(t, tt.expRes.PostgresqlNode.RamGB, res.PostgresqlNode.RamGB, "PostgreSQL RAM")
			assert.Equal(t, tt.expRes.PostgresqlNode.Type, res.PostgresqlNode.Type, "PostgreSQL Type")
			assert.Equal(t, tt.expRes.OpenSearchNode.CpuCount, res.OpenSearchNode.CpuCount, "OpenSearch CPU Count")
			assert.Equal(t, tt.expRes.OpenSearchNode.InstanceCount, res.OpenSearchNode.InstanceCount, "OpenSearch Instance Count")
			assert.Equal(t, tt.expRes.OpenSearchNode.StorageGB, res.OpenSearchNode.StorageGB, "OpenSearch Storage")
			assert.Equal(t, tt.expRes.OpenSearchNode.RamGB, res.OpenSearchNode.RamGB, "OpenSearch RAM")
			assert.Equal(t, tt.expRes.OpenSearchNode.Type, res.OpenSearchNode.Type, "OpenSearch Type")
		}
	}
}
