export type RuleStatus = 'applied' | 'staged';
export type RuleType = 'node' | 'event';

export interface Rule {
  id?: string;
  project_id?: string;
  name: string;
  type: RuleType;
  status: RuleStatus;
  conditions: Condition[];
}

export interface Condition {
  attribute: string;
  values: any;
  operator: string;
}
