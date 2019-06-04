import { IAMType } from '../policies/policy.model';

export interface Project {
  id: string;
  name: string;
  type: IAMType;
}

export class ProjectConstants {
  static readonly UNASSIGNED_PROJECT_ID = '(unassigned)';
  static readonly UNASSIGNED_PROJECT_LABEL = 'Unassigned resources';
  static readonly ALL_PROJECTS_LABEL = 'All projects';
  static readonly MULTIPLE_PROJECTS_LABEL = 'Multiple projects';
}
