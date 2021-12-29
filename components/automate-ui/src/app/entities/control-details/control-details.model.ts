export interface ControlDetail {
  id: string;
  node_id: string;
  node_name: string;
  end_time: string;
  status: string;
  controls: null;
  environment: string;
  version: number;
  platform: {};
  statistics: {};
  profiles: [];
  job_id: string;
  ipaddress: string;
  fqdn: string;
  chef_server: string;
  chef_organization: string;
  roles: [];
  chef_tags: [];
  projects: [];
  status_message: string;
}

export interface Filters {
  report_id: string;
  filters: any;
}
