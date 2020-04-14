import { merge } from 'lodash';

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

export class RoleAttributes {
  default_attributes: string;
  override_attributes: string;
  all: string;
  constructor(resp: RespRoleAttributes) {
    this.default_attributes = resp.default_attributes && JSON.parse(resp.default_attributes) || {};
    this.override_attributes = resp.override_attributes && JSON.parse(resp.override_attributes) || {};
    this.all = merge(
      {},
      this.default_attributes,
      this.override_attributes);
  }
}

export interface RespRoleAttributes {
  default_attributes: string;
  override_attributes: string;
}