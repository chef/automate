export interface Client {
  server_id: string;
  org_id: string;
  name: string;
  client_name: string;
  org_name: string;
  admin: string;
  validator: string;
  certificate: string;
  client_key: ClientKey;
  uri: string;
  json_class: string;
  chef_type: string;
}

export interface ClientKey {
  name: string;
  public_key: string;
  expiration_date: string;
  private_key: string;
}

export interface ResetKey {
  server_id: string;
  org_id: string;
  name: string;
}
