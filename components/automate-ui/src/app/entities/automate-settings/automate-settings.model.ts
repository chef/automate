export class JobSchedulerStatus {
  running: boolean;
  jobs: IngestJob[];

  constructor(running: boolean, ingestJobs: IngestJob[]) {
    this.running = running;
    this.jobs = ingestJobs;
  }

  getJob(name: string): IngestJob {
    return this.jobs.find((job: IngestJob) => job.name === name);
  }
}

export interface ConfigureSettingsRequest {
  jobs: IngestJob[];
}

export interface RespJob {
  running: boolean;
  name: string;
  every: string;
  threshold: string;
  last_run: Date;
  next_run: Date;
  last_elapsed: Date;
  started_on: Date;
}

export interface RespJobSchedulerStatus {
  running: boolean;
  jobs: RespJob[];
}

// IngestJobs is an enum that defines the list of jobs that the
// JobScheduler has inside the ingest-service
export enum IngestJobs {
  // MissingNodes: Checks when a node hasn't check-in
  // for a period of time
  MissingNodes = 'missing_nodes',

  // MissingNodesForDeletion: Checks when a node has been missing
  // for a period of time
  MissingNodesForDeletion = 'missing_nodes_for_deletion',

  // DeleteNodes: Removes completely from elasticsearch nodes that
  // have been marked for deletion
  DeleteNodes = 'delete_nodes'
}

export class IngestJob {
  running: boolean;
  name: string;
  threshold: string;
  every?: string;
  lastRun?: Date;
  nextRun?: Date;
  lastElapsed?: Date;
  startedOn?: Date;

  constructor(respJob: RespJob) {
    if (respJob !== null) {
      this.running = respJob.running;
      this.name = respJob.name;
      this.every = respJob.every;
      this.threshold = respJob.threshold;
      this.lastRun = new Date(respJob.last_run);
      this.nextRun = new Date(respJob.next_run);
      this.lastElapsed = new Date(respJob.last_elapsed);
      this.startedOn = new Date(respJob.started_on);
    }
  }
}
