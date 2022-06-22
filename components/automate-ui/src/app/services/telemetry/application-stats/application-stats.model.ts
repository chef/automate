export interface ApplicationUsageStats {
    total_services: string;
    days_since_last_post: string;
}

export interface ApplicationUsageAckStats {
    lastTelemetryReportedAt: string;
}
