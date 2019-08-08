export type RuleStatus = 'APPLIED' | 'STAGED';
export type RuleType = 'NODE' | 'EVENT';
export type ConditionOperator = 'MEMBER_OF' | 'EQUALS';

interface KVPair {
  key: string;
  value: string;
}

// Using array of pairs instead of a hashmap to allow ordering
export type RuleTypeMappedObject = {
  [K in 'node' | 'event']: KVPair[]
};

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
  operator: ConditionOperator;
  values: string[];
}
