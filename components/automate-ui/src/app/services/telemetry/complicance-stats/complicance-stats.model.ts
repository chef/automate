export interface UnfilteredStats {
    controls: number;
    environments: number;
    nodes: string;
    nodes_cnt: number;
    platforms: number;
    profiles: number;  
}

export interface ComplianceStatsSummary {
    report_summary: {
        unfiltered_stats: UnfilteredStats
    }
}
