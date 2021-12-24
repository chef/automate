export interface Server {
  id: string;
  name: string;
  fqdn: string;
  ip_address: string;
  orgs_count?: number;
  webui_key?: string;
}

export interface User {
  id: string;
  server_id: string;
  infra_server_username: string;
  credential_id: string;
  connector: string;
  automate_user_id: string;
  is_server_admin: boolean;
}

export interface WebUIKey {
  id: string;
  webui_key: string;
}
