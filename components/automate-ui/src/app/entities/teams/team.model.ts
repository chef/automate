export interface Team {
  id: string;
  name: string;
  guid: string; // to be deprecated after GA
  projects: string[];
}
