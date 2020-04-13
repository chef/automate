// must match automate-ui/src/entities/policies/policy.model.ts
export type IAMType = 'CHEF_MANAGED' | 'CUSTOM';

export interface Policy {
  id: string;
  name: string;
  type: IAMType;
  members: string[];
  statements?: Statement[];
}

export interface Statement {
  effect: 'ALLOW' | 'DENY';
  role?: string;
  actions?: string[];
  resources?: string[];
}

// must match automate-ui/src/entities/projects/project.model.ts
export interface Project {
  id: string;
  name: string;
  skip_policies?: boolean;
  // only returned by the API
  type?: IAMType;
  status?: ProjectStatus;
}

export class ProjectConstants {
  static readonly UNASSIGNED_PROJECT_ID = '(unassigned)';
  static readonly UNASSIGNED_PROJECT_LABEL = '(unassigned)';
  static readonly ALL_PROJECTS_LABEL = 'All projects';
  static readonly MULTIPLE_PROJECTS_LABEL = 'Multiple projects';
}

// must match automate-ui/src/entities/rules/rule.model.ts
export type ProjectStatus =
  'PROJECT_RULES_STATUS_UNSET' | 'RULES_APPLIED' | 'EDITS_PENDING' | 'NO_RULES';
export type RuleStatus = 'APPLIED' | 'STAGED';
export type RuleType = 'NODE' | 'EVENT';
export type ConditionOperator = 'MEMBER_OF' | 'EQUALS';

export interface Rule {
  id: string;
  project_id: string;
  name: string;
  type: RuleType;
  conditions: Condition[];
  // only returned by API
  status?: RuleStatus;
}

export interface Condition {
  attribute: string;
  operator: ConditionOperator;
  values: string[];
}
