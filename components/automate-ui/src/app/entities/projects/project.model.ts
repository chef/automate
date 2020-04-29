import { xor } from 'lodash/fp';
import { IAMType } from 'app/entities/policies/policy.model';
import { ProjectStatus } from 'app/entities/rules/rule.model';

export interface Project {
  id: string;
  name: string;
  type: IAMType;
  status: ProjectStatus;
  skip_policies?: boolean;
}

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

export function noProjectsUpdated(
  previousProjects: string[], currentProjects: ProjectCheckedMap): boolean {
  const projectsUpdated = xor(
    previousProjects,
    Object.keys(currentProjects).filter(id => currentProjects[id].checked));
  return projectsUpdated.length === 0;
}
