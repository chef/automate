import { merge } from 'lodash/fp';

export interface InfraRole {
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
  default_attributes: string;
  override_attributes: string;
  all: string;
  constructor(resp: RespRoleAttributes) {
    this.default_attributes =
      (resp.default_attributes && JSON.parse(resp.default_attributes)) || '';
    this.override_attributes =
      (resp.override_attributes && JSON.parse(resp.override_attributes)) || '';
    this.all = merge(
      this.default_attributes,
      this.override_attributes);
  }
}

export interface RespRoleAttributes {
  default_attributes: string;
  override_attributes: string;
}
