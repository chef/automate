export interface SsoConfig {
  ca_contents: string;
  sso_url: string;
  email_attr: string;
  groups_attr: string;
  allowed_groups: [];
  entity_issuer: string;
  name_id_policy: string;
}
