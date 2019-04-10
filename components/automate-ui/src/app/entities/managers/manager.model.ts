export interface Manager {
  id: string;
  name: string;
  type: string;
  credential?: string;
  status?: string;
  date_added?: string;
  instance_credentials?: CredentialTag[];
}

export interface CredentialTag {
  tag_key: string;
  tag_value: string;
  credential_ids: string[];
}
