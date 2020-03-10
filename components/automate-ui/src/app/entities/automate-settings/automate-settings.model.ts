export class JobSchedulerStatus {
  infra?: {
    jobs: IngestJob[];
  };
  compliance?: {
    jobs: IngestJob[];
  };
  event_feed?: {
    jobs: IngestJob[];
  };
  services?: {
    jobs: IngestJob[];
  };

  constructor(allJobs: any) {
    this.infra = allJobs.infra;
    this.compliance = allJobs.compliance;
    this.event_feed = allJobs.event_feed;
    this.services = allJobs.services;
  }

  // getJob(name: string): IngestJob {
  //   return this.jobs.find((job: IngestJob) => job.name === name);
  // }
}

export interface ConfigureSettingsRequest {
  jobs: IngestJob[];
}

export interface RespJob {
  name: string;
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

// Note: Can I nest an interface?
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

  // INFRA //
  // DeleteNodes: Removes completely from elasticsearch nodes that
  // have been marked for deletion
  DeleteNodes = 'delete_nodes',

  // MissingNodes: Checks when a node hasn't check-in
  // for a period of time
  MissingNodes = 'missing_nodes',

  // MissingNodesForDeletion: Checks when a node has been missing
  // for a period of time
  MissingNodesForDeletion = 'missing_nodes_for_deletion',

  PeriodicPurgeTimeseries = 'periodic_purge_timeseries',

  // COMPLIANCE //
  PeriodicPurge = 'periodic_purge'
}

export class IngestJob {
  name: string;
  disabled: boolean;
  recurrence: string;
  threshold: string;
  purge_policies: any; // needs specific type
  last_elapsed?: Date;
  next_due_at?: Date;
  last_enqueued_at?: Date;
  last_started_at?: Date;
  last_ended_at?: Date;

  constructor(respJob: RespJob) {
    if (respJob !== null) {
      this.name = respJob.name;
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
