import { merge } from 'lodash';
import * as moment from 'moment';

export type Status = 'success' | 'failure' | 'missing' | 'skipped';
export type Compliance = 'compliant' | 'uncompliant' | 'skipped';
export type HealthStatus = 'ok' | 'critical' | 'warning' | 'unknown';
export type RollupServiceStatus = HealthStatus | 'total';
export type RollupState = Status | 'total';
export type RollupCompliance = Compliance | 'total';
export type SortDirection = 'asc' | 'desc' | 'ASC' | 'DESC';


export enum LoadingStatus {
  notLoaded,
  loading,
  loadingSuccess,
  loadingFailure
}

export interface ComplianceData {
  end_time: string;
  node_name: string;
  status: string;
  critical: number;
  major: number;
  minor: number;
  passed: number;
  skipped: number;
  organization_name: string;
  source_fqdn: string;
  environment: string;
}

export class ComplianceNode implements ComplianceData {
  entity_uuid: string;
  end_time: string;
  node_name: string;
  status: string;
  critical: number;
  major: number;
  minor: number;
  passed: number;
  skipped: number;
  organization_name: string;
  source_fqdn: string;
  environment: string;

  constructor(complianceData) {
    this.entity_uuid = complianceData._id;
    this.end_time = complianceData._source.compliance_summary.end_time;
    this.node_name = complianceData._source.compliance_summary.node_name;
    this.status = complianceData._source.compliance_summary.status;
    this.critical = complianceData._source.compliance_summary.failed.critical;
    this.major = complianceData._source.compliance_summary.failed.major;
    this.minor = complianceData._source.compliance_summary.failed.minor;
    this.passed = complianceData._source.compliance_summary.passed.total;
    this.skipped = complianceData._source.compliance_summary.skipped.total;
    this.organization_name = complianceData._source.organization_name;
    this.source_fqdn = complianceData._source.source_fqdn;
    this.environment = complianceData._source.environment;
  }
}

export interface Chicklet {
  text: string;
  type: string;
}

export interface SuggestionItem {
  name: string;
  title: string;
  icon: string;
}

export interface SearchBarCategoryItem {
  text: string;
  type: string;
  providedValues?: SuggestionItem[];
  allowWildcards?: boolean;
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

// Since some of the properties of NodeFilter are optional,
// we have a const that has the full list for use in places that
// want to iterate on the full list of filters. Please keep in
// sync with the above NodeFilter interface.
export const nodeFilterProperties =
  ['attribute', 'compliance', 'cookbook', 'environment', 'node_name', 'organizations',
   'page', 'pageSize', 'recipe', 'resource_name', 'role', 'searchBar', 'servers',
   'sortDirection', 'sortField', 'status', 'policy_group', 'policy_name', 'policy_revision'];

export const termMap = {
  cookbook: 'cookbooks',
  attribute: 'attributes',
  resource_name: 'resource_names',
  node_name: 'name',
  recipe: 'recipes',
  policy_group: 'policy_group',
  policy_name: 'policy_name',
  policy_revision: 'policy_revision'
};

// Converge status (run-history) gives us failed and success,
// where as compliance status (scan-history) fives is failed, passed, skipped
export enum SelectedStatus {
  All,
  Success,
  Failure,
  Missing,
  Skipped,
  Failed,
  Passed
}

export interface RespChefEvent {
  event_type: string;
  task: string;
  start_time: Date;
  entity_name: string;
  requestor_type: string;
  requestor_name: string;
  service_hostname: string;
  parent_name: string;
  parent_type: string;
  event_count: number;
  end_time: Date;
  end_id: string;
  start_id: string;
}

export interface RespChefEventCollection {
  events: RespChefEvent[];
  total_events: number;
}

export class ChefEventCollection {
  events: ChefEvent[];
  totalEvents: number;

  constructor(chefEvents: ChefEvent[], totalEvents: number) {
    this.events = chefEvents;
    this.totalEvents = totalEvents;
  }
}

export class ChefEvent {
  eventType: string;
  task: string;
  startTime: Date;
  entityName: string;
  requestorType: string;
  requestorName: string;
  serviceHostname: string;
  parentName: string;
  parentType: string;
  eventCount: number;
  endTime: Date;
  endId: string;
  startId: string;

  constructor(respChefEvent: RespChefEvent) {
    this.eventType = respChefEvent.event_type;
    this.task = respChefEvent.task;
    this.startTime = respChefEvent.start_time;
    this.entityName = respChefEvent.entity_name;
    this.requestorType = respChefEvent.requestor_type;
    this.requestorName = respChefEvent.requestor_name;
    this.serviceHostname = respChefEvent.service_hostname;
    this.parentName = respChefEvent.parent_name;
    this.parentType = respChefEvent.parent_type;
    this.eventCount = respChefEvent.event_count;
    this.endTime = respChefEvent.end_time;
    this.endId = respChefEvent.end_id;
    this.startId = respChefEvent.start_id;
  }
}

export class NodeHistory {
  response: any;

  constructor(resp?: any) {
    this.response = resp || {};
  }

  get runId(): string {
    return this.response._source.run_id;
  }
  get status(): Status {
    return this.response._source.status;
  }
  get startTime(): Date {
    return this.response._source.start_time;
  }
  get endTime(): Date {
    return this.response._source.end_time;
  }
}

export interface NodeHistoryFilter {
  nodeId?: string;
  startDate?: string;
  endDate?: string;
  status?: string;
  page: number;
  pageSize: number;
}

export interface NodeHistoryCountsFilter {
  nodeId?: string;
  startDate?: string;
  endDate?: string;
}

export interface ExpandedRunListItem {
  type: string;
  name: string;
  version: string;
  skipped: boolean;
  children: ExpandedRunListItem[];
}

export interface RespNodeRun {
  node_id: string;
  id: string;
  node_name: string;
  organization: string;
  resources: Resource[];
  start_time: Date;
  end_time: Date;
  source: string;
  status: string;
  total_resource_count: string;
  updated_resource_count: string;
  deprecations: Deprecation[];
  error: {
    class: string;
    message: string;
    backtrace: string[];
    description: {
      title: string;
      sections: object[];
    }
  };
  tags: string[];
  resource_names: string[];
  recipes: string[];
  chef_tags: string;
  cookbooks: string[];
  platform: string;
  platform_family: string;
  platform_version: string;
  chef_version: string;
  uptime_seconds: number;
  environment: string;
  roles: string;
  policy_name: string;
  policy_group: string;
  policy_revision: string;
  fqdn: string;
  ipaddress: string;
  source_fqdn: string;
  timestamp: Date;
  version: string;
  run_list?: string[];
  expanded_run_list?: {
    id: string;
    run_list: ExpandedRunListItem[];
  };
}

export interface Deprecation {
  message: string;
  url: string;
  location: string;
}

export interface Resource {
  type: string;
  name: string;
  id: string;
  duration: string;
  delta: string;
  ignore_failure: boolean;
  result: string;
  status: string;
  cookbook_name?: string;
  cookbook_version?: string;
  recipe_name?: string;
  conditional?: string;
}

export class NodeRun {
  static Null: NodeRun  = new NodeRun({
    node_id: '',
    node_name: '',
    organization: '',
    resources: [],
    chef_tags: '',
    id: '',
    run_list: [],
    start_time: new Date(0),
    end_time: new Date(0),
    source: '',
    deprecations: [],
    status: '',
    total_resource_count: '',
    updated_resource_count: '',
    tags: [''],
    resource_names: [''],
    recipes: [''],
    cookbooks: [''],
    platform: '',
    platform_family: '',
    platform_version: '',
    chef_version: '',
    uptime_seconds: 0,
    environment: '',
    roles: '',
    policy_group: '',
    policy_name: '',
    policy_revision: '',
    fqdn: '',
    ipaddress: '',
    source_fqdn: '',
    timestamp: new Date(0),
    version: '',
    error: {
      class: '',
      message: '',
      backtrace: [],
      description: {
        title: '',
        sections: []
      }
    },
    expanded_run_list: {
      id: '',
      run_list: []
   }});

  nodeId: string;
  nodeName: string;
  organization: string;
  resources: Resource[];
  deprecations: Deprecation[];
  runId: string;
  runList: string[];
  startTime: Date;
  endTime: Date;
  source: string;
  status: string;
  tags: string[];
  resourceNames: string[];
  recipes: string[];
  cookbooks: string[];
  platform: string;
  platformFamily: string;
  platformVersion: string;
  chefVersion: string;
  environment: string;
  roles: string;
  policyGroup: string;
  policyName: string;
  policyRevision: string;
  fqdn: string;
  ipaddress: string;
  sourceFqdn: string;
  timestamp: Date;
  version: string;
  uptimeSeconds: number;
  error?: {
    class: string;
    message: string;
    backtrace: string[];
    description: {
      title: string;
      sections: object[];
    };
  };
  expandedRunList: {
    id: string;
    run_list: ExpandedRunListItem[];
  };

  constructor(respNodeRun: RespNodeRun) {
    this.nodeId = respNodeRun.node_id;
    this.nodeName = respNodeRun.node_name;
    this.organization = respNodeRun.organization;
    this.resources = respNodeRun.resources;
    this.runId = respNodeRun.id;
    this.runList = respNodeRun.run_list;
    this.startTime = respNodeRun.start_time;
    this.endTime = respNodeRun.end_time;
    this.source = respNodeRun.source;
    this.status = respNodeRun.status;
    this.tags = respNodeRun.tags;

    this.resourceNames = respNodeRun.resource_names;
    this.recipes = respNodeRun.recipes;
    this.cookbooks = respNodeRun.cookbooks;
    this.platform = respNodeRun.platform;
    this.platformFamily = respNodeRun.platform_family;
    this.platformVersion = respNodeRun.platform_version;
    this.chefVersion = respNodeRun.chef_version;
    this.environment = respNodeRun.environment;
    this.roles = respNodeRun.roles;
    this.policyGroup = respNodeRun.policy_group;
    this.policyName = respNodeRun.policy_name;
    this.policyRevision = respNodeRun.policy_revision;
    this.fqdn = respNodeRun.fqdn;
    this.ipaddress = respNodeRun.ipaddress;
    this.sourceFqdn = respNodeRun.source_fqdn;
    this.timestamp = respNodeRun.timestamp;
    this.version = respNodeRun.version;
    this.error = respNodeRun.error;
    this.expandedRunList = respNodeRun.expanded_run_list;
    this.uptimeSeconds = respNodeRun.uptime_seconds;
    this.deprecations = respNodeRun.deprecations;
  }

  isPolicyFile(): boolean {
    return this.policyName !== null && this.policyName !== '';
  }
}

export interface RespNodeRunsCount {
  total: number;
  success: number;
  failure: number;
}

export class NodeRunsCount {
  total: number;
  success: number;
  failure: number;

  constructor(respNodeRunsCount: RespNodeRunsCount) {
    this.total = respNodeRunsCount.total;
    this.success = respNodeRunsCount.success;
    this.failure = respNodeRunsCount.failure;
  }
}

export interface AbridgedRespNodeRun {
  start_time: Date;
  id: string;
  end_time: Date;
  status: string;
}

export class AbridgedNodeRun {
  startTime: Date;
  runId: string;
  endTime: Date;
  status: string;

  constructor(abridgedRespNodeRun: AbridgedRespNodeRun) {
    this.startTime = new Date(abridgedRespNodeRun.start_time);
    this.runId = abridgedRespNodeRun.id;
    this.endTime = new Date(abridgedRespNodeRun.end_time);
    this.status = abridgedRespNodeRun.status;
  }
}

export interface RespNodeAttributes {
  node_id: string;
  name: string;
  run_list: string[];
  chef_environment: string;
  normal: string;
  normal_value_count: number;
  default: string;
  default_value_count: number;
  override: string;
  override_value_count: number;
  automatic: string;
  automatic_value_count: number;
  all_value_count: number;
}

export class NodeAttributes {
  normal: string;
  normalValueCount: number;
  default: string;
  defaultValueCount: number;
  override: string;
  overrideValueCount: number;
  automatic: string;
  automaticValueCount: number;
  all: string;
  allValueCount: number;

  constructor(resp: RespNodeAttributes) {
    this.normal = resp.normal && JSON.parse(resp.normal) || {};
    this.normalValueCount = resp.normal_value_count;
    this.default = resp.default && JSON.parse(resp.default) || {};
    this.defaultValueCount = resp.default_value_count;
    this.override = resp.override && JSON.parse(resp.override) || {};
    this.overrideValueCount = resp.override_value_count;
    this.automatic = resp.automatic && JSON.parse(resp.automatic) || {};
    this.automaticValueCount = resp.automatic_value_count;
    this.allValueCount = resp.all_value_count;

    this.all = merge(
      {},
      this.normal,
      this.default,
      this.override,
      this.automatic);
  }
}


export class RespPolicyCookbooks {
  policy_name: string;
  // CookbookLocks returned as an array because grpc does not like random keys.
  cookbook_locks: RespCookbookLock[];
}

export class PolicyCookbooks {
  policyName: string;
  cookbookLocks: any; // Map of cookbookName to policyIdentifer

  constructor(resp: RespPolicyCookbooks) {
     this.policyName = resp.policy_name;
     // Transform CookbookLocks array into a map for easier indexing by cookbook name.
     const cookbooks = {};
     resp.cookbook_locks.forEach((lock) => {
        cookbooks[lock.cookbook] = lock.policy_identifier;
     });
     this.cookbookLocks = cookbooks;
  }
}

export class GuitarStringItem {
  eventTypeCount: RespEventCount[];
  start: moment.Moment;
  end: moment.Moment;

  constructor(eventTypeCount: RespEventCount[], start: moment.Moment, end: moment.Moment) {
    this.eventTypeCount = eventTypeCount;
    this.start = start;
    this.end = end;
  }

  isEmpty(): boolean {
    return this.eventTypeCount.length === 0;
  }

  isMultiple(): boolean {
    if (this.eventTypeCount.length > 1) {
      return true;
    }
    if (this.eventTypeCount.length === 1 && this.eventTypeCount[0].count > 1) {
      return true;
    }
    return false;
  }

  getEventType(): string {
    if (this.eventTypeCount.length > 1) {
      return 'multiple';
    }
    if (this.eventTypeCount.length === 1 ) {
      return this.eventTypeCount[0].name;
    }
    return '';
  }

  getEventTypeLabel(name, count) {
    let label;

    switch (name) {
      case 'item':
        label = 'data bag item';
        break;
      case 'version':
        label = 'cookbook version';
        break;
      case 'bag':
        label = 'data bag';
        break;
      case 'scanjobs':
        label = 'scan job';
        break;
      default:
        label = name;
    }

    label = count > 1 ? label + 's' : label;
    return label;
  }
}

export class GuitarStringCollection {
  strings: GuitarString[];
  start: moment.Moment;
  end: moment.Moment;

  constructor(strings: GuitarString[], start: moment.Moment, end: moment.Moment) {
    this.strings = strings;
    this.start = start;
    this.end = end;
  }
}

export class GuitarString {
  eventAction: string;
  items: GuitarStringItem[];

  constructor(eventAction: string, items: GuitarStringItem[]) {
    this.eventAction = eventAction;
    this.items = items;
  }
}

export interface RespEventCollection {
  events_count: RespEventCount[];
}

export interface ResponseGuitarString {
  event_action: string; // update, create, delete, ...
  collection: RespEventCollection[];
}

export interface ResponseGuitarStringCollection {
  start: Date; // When the first item in the string starts
  end: Date;
  hours_between: number; // based off the request. example 3 hours
  strings: ResponseGuitarString[]; // has an equal number of eventTypeItems
}

export class RespCookbookLock {
  cookbook: string;
  policy_identifier: string;
}

export interface SidebarFilter {
  organizations?: Array<string>;
  servers?: Array<string>;
}

export interface EventFeedFilter {
  requestorName?: string;
  searchBar?: Array<Chicklet>;
  task?: string;
  collapse?: boolean;
  pageSize?: number;
  startDate?: moment.Moment;
  endDate?: moment.Moment;
  hoursBetween?: number;
}

export interface LocalTeam {
  id: string;
  name: string;
  description: string;
}
// Types
export interface DateRange {
  start: Date | moment.Moment;
  end: Date | moment.Moment;
}

export interface NodeCount {
  total: number;
  success: number;
  failure: number;
  missing: number;
}

export class EventTaskCount {
  static Null: EventTaskCount = new EventTaskCount({total: 0, counts: []});
  total = 0;
  update = 0;
  create = 0;
  delete = 0;

  constructor(response: RespEventCounts) {
    this.total = response.total;
    response.counts.forEach(count => {
      switch (count.name) {
        case 'update':
          this.update = count.count;
          break;
        case 'create':
          this.create = count.count;
          break;
        case 'delete':
          this.delete = count.count;
          break;
      }
    });
  }
}

export class EventTypeCount {
  static Null: EventTypeCount = new EventTypeCount({total: 0, counts: []});
  total = 0;
  cookbook = 0;
  version = 0;
  bag = 0;
  item = 0;
  environment = 0;
  node = 0;
  policyfile = 0;
  profile = 0;
  scanjobs = 0;
  role = 0;
  organization = 0;
  permission = 0;
  user = 0;
  group = 0;
  client = 0;

  constructor(response: RespEventCounts) {
    this.total = response.total;
    response.counts.forEach(count => {
      switch (count.name) {
        case 'policyfile':
          this.policyfile = count.count;
          break;
        case 'node':
          this.node = count.count;
          break;
        case 'cookbook':
          this.cookbook = count.count;
          break;
        case 'version':
          this.version = count.count;
          break;
        case 'item':
          this.item = count.count;
          break;
        case 'bag':
          this.bag = count.count;
          break;
        case 'environment':
          this.environment = count.count;
          break;
        case 'organization':
          this.organization = count.count;
          break;
        case 'permission':
          this.permission = count.count;
          break;
        case 'user':
          this.user = count.count;
          break;
        case 'group':
          this.group = count.count;
          break;
        case 'role':
          this.role = count.count;
          break;
        case 'profile':
          this.profile = count.count;
          break;
        case 'scanjobs':
          this.scanjobs = count.count;
          break;
        case 'client':
          this.client = count.count;
          break;
      }
    });
  }
}

export interface RespEventCount {
  name: string;
  count: number;
}

export interface RespEventCounts {
  total: number;
  counts: RespEventCount[];
}

export class RunInfo {
  runId: string;
  endTime: Date;
  constructor(runId: string, endTime: Date) {
    this.runId = runId;
    this.endTime = endTime;
  }
}

// Copied from https://www.npmjs.com/package/http-status-codes
export enum HttpStatus {
  BAD_REQUEST = 400,
  FORBIDDEN = 403,
  NOT_FOUND = 404,
  CONFLICT = 409,
  PRECONDITION_FAILED = 412
}

