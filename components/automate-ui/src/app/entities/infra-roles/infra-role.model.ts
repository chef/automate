export interface InfraRole {
  name: string;
  chef_type: string;
  description: string;
  json_class: string;
  default_attributes: string;
  override_attributes: string;
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
  children: Lists[];
}

export interface ChildLists {
  type: string;
  name: string;
  version: string;
}
