import { NodeRun } from 'app/types/types';

export enum Terms {
  DesktopName = 'name',
  Platform = 'platform',
  Domain = 'domain',
  Status = 'status',
  CheckInTime = 'checkin',
  ErrorMessage = 'error_message',
  ErrorType = 'error_type'
}

export enum SortOrder {
  Ascending = 'ASC',
  Descending = 'DESC'
}

export interface Selected {
  desktop: Desktop;
  daysAgo: number;
  nodeRun: NodeRun;
}

export interface DailyCheckInCountCollection {
  buckets: DailyCheckInCount[];
  updated: Date;
}

export interface DailyCheckInCount {
  start: Date;
  end: Date;
  checkInCount: number;
  total: number;
}

export interface DailyNodeRuns {
  durations: NodeRunsDailyStatusCollection;
  daysAgo: number;
  nodeId: string;
  status: string;
}

export interface DailyNodeRunsStatus {
  start: Date;
  end: Date;
  status: string;
  run_id: string;
  label?: string;
}

export interface NodeRunsDailyStatusCollection {
  buckets: DailyNodeRunsStatus[];
  updated: Date;
}

export interface DayPercentage {
  daysAgo: number;
  percentage: number;
  total: number;
}

export interface TopErrorsCollection {
  items: TopErrorsItem[];
  updated: Date;
}

export interface TopErrorsItem {
  count: number;
  type: string;
  message: string;
}

export interface CountedDurationCollection {
  items: CountedDurationItem[];
  updated: Date;
}

export interface CountedDurationItem {
  duration: string;
  count: number;
}

export interface Desktop {
  id: string;
  name: string;
  status: string;
  checkin: Date;
  uptimeSeconds: number;
  platform: string;
  platformFamily: string;
  platformVersion: string;
  chefVersion: string;
  domain: string;
  latestRunId: string;
  environment: string;
  tag: string;
  ipaddress: string;
  ip6address: string;
  macaddress: string;
  memoryTotal: string;
  virtualizationSystem: string;
  virtualizationRole: string;
  kernelRelease: string;
  kernelVersion: string;
  hostname: string;
  timezone: string;
  dmiSystemManufacturer: string;
  dmiSystemSerialNumber: string;
  cloudProvider: string;
}

export interface Filter {
  currentPage: number;
  pageSize: number;
  sortingField: string;
  sortingOrder: string;
  terms: TermFilter[];
  start?: Date;
  end?: Date;
}

export interface TermFilter {
  type: string;
  value: string;
}

export interface PageSizeChangeEvent {
  pageSize: number;
  updatedPageNumber: number;
}

export enum NodeMetadataCountType {
  Status = 'status',
  Platform = 'platform',
  Domain = 'domain',
  Environment = 'environment'
}

export interface NodeMetadataCountValue {
  value: string;
  count: number;
  disabled: boolean;
  checked: boolean;
}

export interface NodeMetadataCount {
  type: NodeMetadataCountType;
  label: string;
  values: NodeMetadataCountValue[];
}

export interface DesktopColumnOption {
  name: DesktopColumnName;
  label: DesktopColumnLabel;
  checked: boolean;
}

export interface DesktopColumnOptionUpdate {
  options: DesktopColumnOption[];
  saveAsDefault: boolean;
}

export enum DesktopColumnName {
  Platform = 'platform',
  Environment = 'environment',
  Domain = 'domain',
  Tag = 'tag',
  ChefVersion = 'chefVersion',
  IpAddress = 'ipaddress',
  Ip6Address = 'ip6address',
  MacAddress = 'macaddress',
  Uptime = 'uptimeSeconds',
  MemoryTotal = 'memoryTotal',
  VirtualizationSystem = 'virtualizationSystem',
  VirtualizationRole = 'virtualizationRole',
  KernelRelease = 'kernelRelease',
  KernelVersion = 'kernelVersion',
  Hostname = 'hostname',
  Timezone = 'timezone',
  DMIsystemManufacturer = 'dmiSystemManufacturer',
  DMIsystemSerialNumber = 'dmiSystemSerialNumber',
  CloudProvider = 'cloudProvider',
  Status = 'status'
}

export enum DesktopColumnLabel {
  Platform = 'Platform',
  Environment = 'Environment',
  Domain = 'Domain',
  Tag = 'Tag',
  ChefVersion = 'Chef Version',
  IpAddress = 'IP address',
  Ip6Address = 'IP 6 address',
  MacAddress = 'MAC address',
  Uptime = 'Uptime',
  MemoryTotal = 'Memory Total',
  VirtualizationSystem = 'Virtualization System',
  VirtualizationRole = 'Virtualization Role',
  KernelRelease = 'Kernel Release',
  KernelVersion = 'Kernel Version',
  Hostname = 'Hostname',
  Timezone = 'Timezone',
  DMIsystemManufacturer = 'DMI System Manufacturer',
  DMIsystemSerialNumber = 'DMI System Serial Number',
  CloudProvider = 'Cloud Provider',
  Status = 'Status'
}
