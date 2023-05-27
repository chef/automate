package hardwarecal

import (
	"errors"
	"fmt"
	"math"
)

type HardwareCalReq struct {
	NoOfNodes                     int
	FreqCountComplianceScanPerDay int
	FreqCountClientRunPerDay      int
	FreqCountEventFeedPerDay      int

	DataRetentionDays int

	ComplianceReportSizeKB int
	ClientRunReportSizeKB  int
	EventFeedSizeKB        int

	NoOfReplicasInOpenSearch int
}

type HardwareCalRes struct {
	AutomateNode   *Node
	ChefServerNode *Node
	PostgresqlNode *Node
	OpenSearchNode *Node
}

type Node struct {
	InstanceCount int
	CpuCount      int
	RamGB         int
	StorageGB     int
	Type          string
}

func (h *HardwareCalReq) OpenSearchCal() (osNodeCount, opensearchRam, osNodeStorage int, err error) {
	compReportsTotalRawSizePerDay := h.NoOfNodes * (h.FreqCountComplianceScanPerDay * h.ComplianceReportSizeKB)
	clientReportsTotalRawSizePerDay := h.NoOfNodes * (h.FreqCountClientRunPerDay * h.ClientRunReportSizeKB)
	eventReportsTotalRawSizePerDay := h.NoOfNodes * (h.FreqCountEventFeedPerDay * h.EventFeedSizeKB)

	reportsTotalRawSizePerDay := compReportsTotalRawSizePerDay + clientReportsTotalRawSizePerDay + eventReportsTotalRawSizePerDay

	retainedData := reportsTotalRawSizePerDay * h.DataRetentionDays
	retainedDataGB := retainedData / 1024 / 1024

	totalDataWithReplicationGB := retainedDataGB * (1 + h.NoOfReplicasInOpenSearch)

	opensearchRam = 8
	nodeCountFactor := float64(h.NoOfNodes) / 100000.0
	if nodeCountFactor > 1 {
		nodeCountFactor = 1
	}
	if nodeCountFactor < 0.8 {
		nodeCountFactor = 0.8
	}
	MemoryDiskRatio := 160.0 / nodeCountFactor

	osNodeCount = int(math.Ceil(float64(totalDataWithReplicationGB) / float64(opensearchRam) / MemoryDiskRatio))
	if osNodeCount > 5 {
		opensearchRam = 16
		osNodeCount = int(math.Ceil(float64(totalDataWithReplicationGB) / float64(opensearchRam) / MemoryDiskRatio))
	}
	if osNodeCount > 5 {
		opensearchRam = 32
		osNodeCount = int(math.Ceil(float64(totalDataWithReplicationGB) / float64(opensearchRam) / MemoryDiskRatio))
	}
	if osNodeCount > 5 {
		opensearchRam = 64
		osNodeCount = int(math.Ceil(float64(totalDataWithReplicationGB) / float64(opensearchRam) / MemoryDiskRatio))
	}

	if osNodeCount%2 == 0 {
		osNodeCount++
	}
	if osNodeCount < 3 {
		osNodeCount = 3
	}

	IndexingOverhead := 0.1
	DesiredShardSize := 50.0
	ShardCount := int(float64(totalDataWithReplicationGB) * (1 + IndexingOverhead) / DesiredShardSize)

	MaxShardCount := int(osNodeCount) * (opensearchRam / 2) * 20
	if ShardCount > MaxShardCount {
		return 0, 0, 0, errors.New(fmt.Sprint("MaxShardCount:", MaxShardCount, " is less than Shard Count: ", ShardCount))
	}

	if osNodeCount > 9 {
		return 0, 0, 0, errors.New(fmt.Sprint("opensearch node count needed: ", osNodeCount, ", please see if you can reduce the input requirements so the OpenSearch Nodes needed comes down to max 9 for 64gb ram"))
	}

	diskWatermarkThreshold := 0.15
	marginOfError := 0.1
	totalStorageNeed := int(float64(totalDataWithReplicationGB) * (1 + diskWatermarkThreshold + marginOfError))
	osNodeStorage = int(math.Ceil(float64(totalStorageNeed) / float64(osNodeCount)))
	osNodeStorage = int(math.Round(float64(osNodeStorage)/100.0) * 100.0)

	return
}

func (h *HardwareCalReq) PostgresqlCal() (pgNodeRam, pgStorage int) {
	pgStorageNeedGB := (h.NoOfNodes * 20) / 1024
	pgNodeRam = int(float64(pgStorageNeedGB) * 0.02)
	if pgNodeRam < 8 {
		pgNodeRam = 8
	}
	pgStorage = 200
	if pgStorageNeedGB > pgStorage {
		pgStorage = int(math.Round(float64(pgStorageNeedGB)/100.0) * 100.0)
	}
	return
}

func (h *HardwareCalReq) FrontendCal() (frontendNodeCount, frontendRam int) {
	concurrentComp := h.NoOfNodes
	if h.FreqCountComplianceScanPerDay == 0 {
		concurrentComp = 0
	}
	concurrentClient := h.NoOfNodes
	if h.FreqCountClientRunPerDay == 0 {
		concurrentClient = 0
	}
	concurrentEvent := h.NoOfNodes
	if h.FreqCountEventFeedPerDay == 0 {
		concurrentEvent = 0
	}
	concurrencyNeeded := concurrentComp + concurrentClient + concurrentEvent

	frontendNodeCount = 2
	frontendRam = 8
	processPerMB := 5
	processesPerGB := processPerMB * 1024
	_ = h.checkFrontendNodeCount(&frontendNodeCount, frontendRam, processesPerGB, concurrencyNeeded)

	if frontendNodeCount > 3 {
		frontendNodeCount = 2
		frontendRam = 16
		_ = h.checkFrontendNodeCount(&frontendNodeCount, frontendRam, processesPerGB, concurrencyNeeded)
	}
	if frontendNodeCount > 3 {
		frontendNodeCount = 2
		frontendRam = 32
		_ = h.checkFrontendNodeCount(&frontendNodeCount, frontendRam, processesPerGB, concurrencyNeeded)
	}
	return
}

func (h *HardwareCalReq) checkFrontendNodeCount(frontendNodeCount *int, frontendRam, processesPerGB, concurrencyNeeded int) (maxConcurrency int) {
	maxConcurrency = *frontendNodeCount * (frontendRam * processesPerGB)
	for i := 0; i < 20; i++ {
		if maxConcurrency < concurrencyNeeded {
			*frontendNodeCount++
			maxConcurrency = *frontendNodeCount * (frontendRam * processesPerGB)
		}
	}
	return
}

func (h *HardwareCalReq) NewCal() (r *HardwareCalRes, err error) {
	r = &HardwareCalRes{}

	osNodeCount, opensearchRam, osNodeStorage, err := h.OpenSearchCal()
	if err != nil {
		return nil, err
	}
	osNode, err := GetAwsMtype(opensearchRam)
	if err != nil {
		return nil, err
	}
	osNode.InstanceCount = osNodeCount
	osNode.StorageGB = osNodeStorage
	r.OpenSearchNode = osNode

	pgRam, pgStorage := h.PostgresqlCal()
	r.PostgresqlNode, err = GetAwsMtype(pgRam)
	if err != nil {
		return nil, err
	}
	r.PostgresqlNode.InstanceCount = 3
	r.PostgresqlNode.StorageGB = pgStorage

	frontendNodeCount, frontendRam := h.FrontendCal()
	r.AutomateNode, err = GetAwsMtype(frontendRam)
	if err != nil {
		return nil, err
	}
	r.AutomateNode.InstanceCount = frontendNodeCount
	r.AutomateNode.StorageGB = 200

	r.ChefServerNode, err = GetAwsMtype(frontendRam)
	if err != nil {
		return nil, err
	}
	r.ChefServerNode.InstanceCount = frontendNodeCount
	r.ChefServerNode.StorageGB = 200

	return
}

var AwsMtypes = []*Node{
	&Node{
		CpuCount: 2,
		RamGB:    8,
		Type:     "m5.large",
	},
	&Node{
		CpuCount: 4,
		RamGB:    16,
		Type:     "m5.xlarge",
	},
	&Node{
		CpuCount: 8,
		RamGB:    32,
		Type:     "m5.2xlarge",
	},
	&Node{
		CpuCount: 16,
		RamGB:    64,
		Type:     "m5.4xlarge",
	},
}

func GetAwsMtype(ramGB int) (*Node, error) {
	for _, n := range AwsMtypes {
		if ramGB <= n.RamGB {
			newNode := &Node{
				CpuCount: n.CpuCount,
				RamGB:    n.RamGB,
				Type:     n.Type,
			}
			return newNode, nil
		}
	}
	return nil, errors.New("relevant aws machine type not found")
}
