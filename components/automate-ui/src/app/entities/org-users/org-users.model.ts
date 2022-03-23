export interface OrgUser {
  user_id: string;
  server_id: string;
  org_id: string;
  infra_server_username: string;
  first_name: string;
  last_name: string;
  email_id: string;
  middle_name: string;
  display_name: string;
  connector: string;
  automate_user_id: string;
  is_admin: boolean;
}
