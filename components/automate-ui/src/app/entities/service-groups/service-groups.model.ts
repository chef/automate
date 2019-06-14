import { RollupServiceStatus, SortDirection  } from '../../types/types';

export interface HealthSummary {
  total: number;
  ok: number;
  warning: number;
  critical: number;
  unknown: number;
}

export interface Service {
  supervisor_id: string;
  release: string;
  group: string;
  status: string;
  health_check: string;
  applications: string;
  environment: string;
  fqdn: string;
  site: string;
  channel: string;
}

export interface ServiceGroup {
  id: string;
  name: string;
  package: string;
  release: string;
  status: string;
  health_percentage: boolean;
  services_health_counts: HealthSummary;
  applications: string;
  environment: string;
}

export interface ServiceGroupFilters {
  page?: number;
  pageSize?: number;
  status?: RollupServiceStatus;
  sortField?: string;
  sortDirection?: SortDirection;
}

export interface ServicesFilters {
  service_group_id?: string;
  health: string;
  page: number;
  pageSize?: number;
}

export interface ServicesPayload {
  group: string;
  services: Service[];
  services_health_counts: HealthSummary;
}

export interface ServiceGroupsPayload {
  service_groups: ServiceGroup[];
}

export interface FieldDirection {
  name: SortDirection;
  percent_ok: SortDirection;
  environment: SortDirection;
  app_name: SortDirection;
}
