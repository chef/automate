export class JobSchedulerStatus {
  jobs: IngestJob[];

  constructor(ingestJobs: IngestJob[]) {
    this.jobs = ingestJobs;
  }
}

export interface ConfigureSettingsRequest {
  jobs: IngestJob[];
}

export interface RespJob {
  name: string;
  nested_name?: string;
  disabled: boolean;
  recurrence: string;
  threshold: string;
  purge_policies: any; // TODO needs specific type
  last_elapsed?: Date;
  next_due_at?: Date;
  last_enqueued_at?: Date;
  last_started_at?: Date;
  last_ended_at?: Date;
}

export interface RespJobSchedulerStatus {
  infra: {
    jobs: RespJob[];
  };
  compliance: {
    jobs: RespJob[];
  };
  event_feed: {
    jobs: RespJob[];
  };
  services: {
    jobs: RespJob[];
  };
}

// IngestJobs is an enum that defines the list of jobs that the
// JobScheduler has inside the ingest-service
export enum IngestJobs {
  //  EventFeed
  EventFeedRemoveData = 'eventFeedRemoveData',
  EventFeedServerActions = 'eventFeedServerActions',

  // Service Groups
  ServiceGroupNoHealthChecks = 'serviceGroupNoHealthChecks',
  ServiceGroupRemoveServices = 'serviceGroupRemoveServices',

  // Client Runs
  ClientRunsRemoveData = 'clientRunsRemoveData',
  ClientRunsLabelMissing = 'clientRunsLabelMissing',
  ClientRunsRemoveNodes = 'clientRunsRemoveNodes',

  // Compliance
  ComplianceRemoveReports = 'complianceRemoveReports',
  ComplianceRemoveScans = 'complianceRemoveScans'
}

export enum JobCategories {
  Infra = 'infra',
  Compliance = 'compliance',
  EventFeed = 'event_feed',
  Services = 'services'
}

export enum InfraJobName {
  MissingNodes = 'missing_nodes',
  MissingNodesForDeletion = 'missing_nodes_for_deletion',
  DeleteNodes = 'delete_nodes',
  PeriodicPurgeTimeseries = 'periodic_purge_timeseries'
}

// Actions and ConvergeHistory are nested, but contained inside
// the InfraJobName of PeriodicPurgeTimeseries
export enum NestedJobName {
  ComplianceReports = 'compliance-reports',
  ComplianceScans = 'compliance-scans',
  Feed = 'feed',
  Actions = 'actions',
  ConvergeHistory = 'converge-history'
}

export class IngestJob {
  category: JobCategories;
  name: string;
  nested_name?: string;
  recurrence?: string;
  threshold: string;
  disabled: boolean;
  purge_policies?: any; // TODO needs specific type
  older_than_days?: number;
  last_elapsed?: Date;
  next_due_at?: Date;
  last_enqueued_at?: Date;
  last_started_at?: Date;
  last_ended_at?: Date;

  constructor(category: JobCategories, respJob: RespJob) {
    if (respJob !== null) {
      this.category = category;
      this.name = respJob.name;
      this.nested_name = respJob.nested_name;
      this.disabled = respJob.disabled;
      this.recurrence = respJob.recurrence;
      this.threshold = respJob.threshold;
      this.purge_policies = respJob.purge_policies;
      this.last_elapsed = new Date(respJob.last_elapsed);
      this.next_due_at = new Date(respJob.next_due_at);
      this.last_enqueued_at = new Date(respJob.last_enqueued_at);
      this.last_started_at = new Date(respJob.last_started_at);
      this.last_ended_at = new Date(respJob.last_ended_at);
    }
  }
}

export class UnfurledJob {
  disabled: boolean;
  policy_name?: string;
  older_than_days?: number;
  name?: string;
  threshold?: string;
}

export interface JobRequestComponent {
  name: string;
  purge_policies: {
    elasticsearch: UnfurledJob[];
  };
}

export interface JobRequestBody {
  infra: {
    // Infra is a special case and can contain a list of JobRequestComponents
    // or an UnfurledJob depending on the API
    job_settings: any[] // <-- this actually needs to be an Array of
                        // JobRequestComponents AND unFurlInjest Job
  };
  compliance: {
    job_settings: any[]
  };
  event_feed: {
    job_settings: any[]
  };
  // services has not yet been implemented so we will leave as optional for now
  services?: {
    job_settings: any[]
  };
}

export interface SingleDefaultForm {
  category: JobCategories;
  name?: string;                  // TODO; make stricter after services implemented
  nested_name?: NestedJobName;
  unit: {
    value: string;
    disabled: boolean;
  };
  threshold: {
    value: string;
    disabled: boolean;
  };
  disabled: boolean;
}

export interface DefaultFormData {
  eventFeedRemoveData: SingleDefaultForm;
  eventFeedServerActions: SingleDefaultForm;
  serviceGroupNoHealthChecks: SingleDefaultForm;
  serviceGroupRemoveServices: SingleDefaultForm;
  clientRunsRemoveData: SingleDefaultForm;
  clientRunsLabelMissing: SingleDefaultForm;
  clientRunsRemoveNodes: SingleDefaultForm;
  complianceRemoveReports: SingleDefaultForm;
  complianceRemoveScans: SingleDefaultForm;
}

