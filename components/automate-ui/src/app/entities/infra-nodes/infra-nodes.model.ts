export interface InfraNode {
  id?: string;
  server_id: string;
  org_id: string;
  name: string;
  fqdn?: string;
  ip_address?: string;
  check_in?: string;
  uptime?: string;
  platform?: string;
  environment: string;
  policy_group: string;
  policy_name: string;
  default_attributes: string;
  override_attributes: string;
  normal_attributes: string;
  automatic_attributes: string;
  run_list: string[];
  tags?: string[];
}

export interface InfraNodeAttribute {
  server_id: string;
  org_id: string;
  name: string;
  attributes: string[];
}
