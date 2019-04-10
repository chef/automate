import { IAMType } from '../policies/policy.model';

export interface Project {
  id: string;
  name: string;
  type: IAMType;
}
