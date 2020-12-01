import { HttpErrorResponse } from '@angular/common/http';
import { EntityStatus } from 'app/entities/entities';
import {
  Chicklet,
  RollupServiceStatus,
  SortDirection
} from 'app/types/types';

export interface ServiceGroup {
  id: string;
  name: string;
  package: string;
  release: string;
  status: string;
  health_percentage: boolean;
  services_health_counts: ServiceGroupsHealthSummary;
  applications: string;
  environment: string;
}

export interface ServiceGroupsFilters {
  page?: number;
  pageSize?: number;
  status?: RollupServiceStatus;
  searchBar?: Array<Chicklet>;
  sortField?: string;
  sortDirection?: SortDirection;
}

export enum AllowedStatus {
  OK = 'ok',
  CRITICAL = 'critical',
  WARNING = 'warning',
  UNKNOWN = 'unknown',
  TOTAL = 'total',
  DISCONNECTED = 'disconnected',
  CONNECTED = 'connected'
}

export interface ServiceGroupsHealthSummary {
  total: number;
  ok: number;
  warning: number;
  critical: number;
  unknown: number;
  disconnected: number;
}

export interface ServiceGroupsSuggestions {
  values: string[];
  status: EntityStatus;
}

export interface ServiceGroupsPayload {
  service_groups: ServiceGroup[];
}

export interface SelectedServiceGroup {
  name: string;
  services: GroupServices;
}

export interface GroupServices {
  list: GroupService[];
  filters: GroupServicesFilters;
  status: EntityStatus;
  error: HttpErrorResponse;
  healthSummary: ServiceGroupsHealthSummary;
}

export interface GroupService {
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
  update_strategy: 'NONE' | 'AT-ONCE' | 'ROLLING';
  previous_health_check: string;
  current_health_since: string;
  health_updated_at: Date;
  health_check_result: HealthCheckResult;
  id?: number;
}

export interface HealthCheckResult {
  stdout: string;
  stderr: string;
  exit_status: number;
}

export interface GroupServicesFilters {
  service_group_id: string;
  health?: string;
  page?: number;
  pageSize?: number;
  searchBar: Array<Chicklet>;
}

export interface GroupServicesPayload {
  group: string;
  services: GroupService[];
  services_health_counts: ServiceGroupsHealthSummary;
}

export interface ServicesStats {
  totalServiceGroups: number;
  totalServices: number;
  totalSupervisors: number;
  totalDeployments: number;
}

// Not used within file
export interface FieldDirection {
  name: SortDirection;
  percent_ok: SortDirection;
  environment: SortDirection;
  app_name: SortDirection;
}
