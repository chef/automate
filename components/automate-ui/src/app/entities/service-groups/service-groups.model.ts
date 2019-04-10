import { RollupServiceStatus } from '../../types/types';

export interface ServiceGroupHealthSummary {
  total: number;
  ok: number;
  warning: number;
  critical: number;
  unknown: number;
}
export interface ServiceGroup {
  id: string;
  name: string;
  release: string;
  status: string;
  health_percentage: boolean;
  services_health_counts: ServiceGroupHealthSummary;
}

export interface ServiceGroupFilters {
  status?: RollupServiceStatus;
}

export interface ServiceGroupsPayload {
  service_groups: ServiceGroup[];
}

