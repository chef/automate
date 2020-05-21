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
  default_attributes: Object;
  override_attributes: Object;
  all: AllAttributes;
  constructor(resp: RespEnvironmentAttributes) {
    this.default_attributes =
      (resp.default_attributes && JSON.parse(resp.default_attributes)) || {};
    this.override_attributes =
      (resp.override_attributes && JSON.parse(resp.override_attributes)) || {};
    this.all = {
      default_attributes: this.default_attributes,
      override_attributes: this.override_attributes
    };
  }
}

export interface RespEnvironmentAttributes {
  default_attributes: string;
  override_attributes: string;
}

export interface AllAttributes {
  default_attributes: Object;
  override_attributes: Object;
}
