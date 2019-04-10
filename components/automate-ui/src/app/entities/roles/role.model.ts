import { IAMType } from '../policies/policy.model';

export interface Role {
  id: string;
  name: string;
  actions: string[];
  type: IAMType;
}
