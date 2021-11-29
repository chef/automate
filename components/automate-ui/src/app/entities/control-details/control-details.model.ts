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
  profiles: [
    {
      name: string;
      title: string;
      maintainer: string;
      copyright: string;
      copyright_email: string;
      license: string;
      summary: string;
      version: number;
      owner: string;
      full: string;
      supports: [];
      depends: [];
      sha256: string;
      groups: [];
      controls: [
        {
          id: string;
          code: string;
          desc: string;
          impact: number;
          title: string;
          source_location: {};
          results: [
            {
              status: string;
              code_desc: string;
              run_time: number;
              start_time: string;
              message: string;
              skip_message: string
            }
          ];
          refs: [];
          tags: {};
          waived_str: string;
          waiver_data: null;
          removed_results_counts: null
        }
      ];
      attributes: [];
      latest_version: number;
      status: string;
      skip_message: string;
      status_message: string;
    }
  ];
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