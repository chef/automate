export interface InfraRole {
  name: string;
  chef_type: string;
  description: string;
  json_class: string;
  run_list: string[];
  expanded_run_list: ExpandList[];
}

export interface ExpandList {
  id: string;
  run_list: Lists[];
}

export interface Lists {
  type: string;
  name: string;
  version: string;
  skipped: string;
  children: string[];
}
