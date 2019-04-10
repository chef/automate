import { Chicklet, RollupCompliance, RollupState, SortDirection } from '../../types/types';

export interface RespNode {
  id: string;
  name: string;
  status: string;
  checkin: Date;
  uptime_seconds: number;
  platform: string;
  environment: string;
  policy_group: string;
  latest_run_id: string;
  fqdn: string;
  organization: string;
  source_fqdn: string;
  has_runs_data: boolean;
  last_ccr_received: Date;
  deprecations_count: number;
  chef_version: string;
}

export interface NodeFilter {
  attribute?: string;
  compliance?: RollupCompliance;
  cookbook?: string;
  environment?: string;
  node_name?: string;
  organizations?: Array<string>;
  page: number;
  pageSize: number;
  recipe?: string;
  resource_name?: string;
  role?: string;
  searchBar?: Array<Chicklet>;
  servers?: Array<string>;
  sortDirection?: SortDirection;
  sortField?: string;
  status?: RollupState;
  policy_group?: string;
  policy_name?: string;
  policy_revision?: string;
}

export interface Node {
  id: string;
  name: string;
  status: string;
  checkin: Date;
  uptime_seconds: number;
  platform: string;
  environment: string;
  policy_group: string;
  latestRunId: string;
  hasRuns: boolean;
  fqdn?: string;
  organization?: string;
  source_fqdn?: string;
  lastCcrReceived: Date;
  deprecationsCount: number;
  chefVersion: string;
}
export interface RespService {
  name: string;
  origin: string;
  version: string;
  release: string;
}
export interface RespServiceVersions {
  services: RespService[];
}

export interface FieldDirection {
  name: SortDirection;
  checkin: SortDirection;
  uptime_seconds: SortDirection;
  platform: SortDirection;
  environment: SortDirection;
  policy_group: SortDirection;
  chef_version: SortDirection;
  deprecations_count: SortDirection;
}

export interface ColumnsPreference {
  check_in: boolean;
  uptime: boolean;
  platform: boolean;
  environment: boolean;
  policy_group: boolean;
  chef_version: boolean;
  deprecations_count: boolean;
}
