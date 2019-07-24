export interface Rule {
  id?: string;
  project_id?: string;
  name: string;
  type: string;
  status: string;
  conditions: Condition[];
}

export interface Condition {
  attribute: string;
  values: any;
  operator: string;
}
