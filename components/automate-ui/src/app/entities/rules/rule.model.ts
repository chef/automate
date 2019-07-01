export interface Rule {
  id?: string;
  project_id?: string;
  name: string;
  type: string;
  edits: string;
  conditions: Condition[];
}

export interface Condition {
  attribute: string;
  values: any;
  operator: string;
}

export class RuleConstants {
  static readonly UNASSIGNED_PROJECT_ID = '(unassigned)';
  static readonly UNASSIGNED_PROJECT_LABEL = '(unassigned)';
  static readonly ALL_PROJECTS_LABEL = 'All projects';
  static readonly MULTIPLE_PROJECTS_LABEL = 'Multiple projects';
}
