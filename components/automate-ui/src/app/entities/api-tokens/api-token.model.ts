export interface ApiToken {
  id: string;
  name: string;
  value: string;
  active: boolean;
  created_at: string;
  updated_at: string;
  projects: string[];
}
