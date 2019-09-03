import { orderBy } from 'lodash/fp';

import { IAMType } from '../policies/policy.model';
import { ProjectStatus } from '../rules/rule.model';

export interface Project {
  id: string;
  name: string;
  type: IAMType;
  status: ProjectStatus;
}

export function sortProjectsByName(projects: Array<Project>): Array<Project> {
  return orderBy(['name'], ['asc'], projects);
}

export class ProjectConstants {
  static readonly UNASSIGNED_PROJECT_ID = '(unassigned)';
  static readonly UNASSIGNED_PROJECT_LABEL = '(unassigned)';
  static readonly ALL_PROJECTS_LABEL = 'All projects';
  static readonly MULTIPLE_PROJECTS_LABEL = 'Multiple projects';
}
