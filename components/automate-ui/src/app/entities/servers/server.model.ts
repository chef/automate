export interface Server {
  id: string;
  name: string;
  fqdn: string;
  ip_address: string;
  orgs_count?: number;
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
