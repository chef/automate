export class JobSchedulerStatus {
  jobs: IngestJob[];

  constructor(ingestJobs: IngestJob[]) {
    this.jobs = ingestJobs;
  }

  // Will need to add category as an argument as well
  getJob(name: string): IngestJob {
    return this.jobs.find((job: IngestJob) => job.name === name);
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
  purge_policies: any; // needs specific type
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

  // ***************** NEEDS UPDATE TO MATCH NEW JOB SCHEDULER ***************** //

  // INFRA //
  // DeleteNodes: Removes completely from elasticsearch nodes that
  // have been marked for deletion
  // DeleteNodes = 'delete_nodes',

  // // MissingNodes: Checks when a node hasn't check-in
  // // for a period of time
  // MissingNodes = 'missing_nodes',

  // // MissingNodesForDeletion: Checks when a node has been missing
  // // for a period of time
  // MissingNodesForDeletion = 'missing_nodes_for_deletion',


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

export class IngestJob {
  category: string;
  name: string;
  nested_name?: string;
  disabled: boolean;
  recurrence: string;
  threshold: string;
  purge_policies: any; // needs specific type
  last_elapsed?: Date;
  next_due_at?: Date;
  last_enqueued_at?: Date;
  last_started_at?: Date;
  last_ended_at?: Date;

  constructor(category: string, respJob: RespJob) {
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
