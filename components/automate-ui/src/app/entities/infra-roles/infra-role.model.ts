

export interface InfraRole {
  server_id: string;
  org_id: string;
  name: string;
  environments: string[];
  chef_type: string;
  description: string;
  json_class: string;
  default_attributes: string;
  override_attributes: string;
  run_list: string[];
  expanded_run_list: ExpandedList[];
}

export interface ExpandedList {
  id: string;
  run_list: List[];
}

export interface List {
  type: string;
  name: string;
  version: string;
  children: List[];
}

export interface ChildList {
  type: string;
  name: string;
  version: string;
}

export class RoleAttributes {
  default_attributes: Object;
  override_attributes: Object;
  all: AllAttributes;
  constructor(resp: RespAttributes) {
    this.default_attributes =
      (resp.default_attributes && JSON.parse(resp.default_attributes)) || '';
    this.override_attributes =
      (resp.override_attributes && JSON.parse(resp.override_attributes)) || '';
    this.all = {
      default_attributes: this.default_attributes,
      override_attributes: this.override_attributes
    };
  }
}

export interface RespAttributes {
  default_attributes: string;
  override_attributes: string;
}

export interface AllAttributes {
  default_attributes: Object;
  override_attributes: Object;
}
