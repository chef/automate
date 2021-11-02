export interface Credential {
  id: string;
  name: string;
  type: string;
  last_modified?: string;
  tags: Array<KVData>;
  data: Array<KVData>;
}

export interface KVData {
  key?: string;
  value?: string;
}

