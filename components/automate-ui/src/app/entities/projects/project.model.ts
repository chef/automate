import { IAMType } from '../policies/policy.model';
import { ProjectStatus } from '../rules/rule.model';

export interface Project {
  id: string;
  name: string;
  type: IAMType;
  status: ProjectStatus;
  skip_policies?: boolean;
}

// Extend the project model with the checked field.
// This represents whether the project's checkbox is unchecked or not
// in this component's UI
export interface ProjectChecked extends Project {
  checked: boolean;
}

export interface ProjectCheckedMap {
  [id: string]: ProjectChecked;
}

export class ProjectConstants {
  static readonly UNASSIGNED_PROJECT_ID = '(unassigned)';
  static readonly UNASSIGNED_PROJECT_LABEL = '(unassigned)';
  static readonly ALL_PROJECTS_LABEL = 'All projects';
  static readonly MULTIPLE_PROJECTS_LABEL = 'Multiple projects';
}
