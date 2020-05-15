import { merge } from 'lodash/fp';

export interface Environment {
  name: string;
  chef_type: string;
  description: string;
  json_class: string;
  default_attributes: string;
  override_attributes: string;
  run_list: string[];
  cookbook_versions: CookbookVersion[];
}

export interface CookbookVersion {
  name: string;
  version: string;
}

export class EnvironmentAttributes {
  default_attributes: string;
  override_attributes: string;
  all: string;
  constructor(resp: RespEnvironmentAttributes) {
    this.default_attributes =
      (resp.default_attributes && JSON.parse(resp.default_attributes)) || {};
    this.override_attributes =
      (resp.override_attributes && JSON.parse(resp.override_attributes)) || {};
      this.all = merge(
        this.default_attributes,
        this.override_attributes);
  }
}

export interface RespEnvironmentAttributes {
  default_attributes: string;
  override_attributes: string;
}
