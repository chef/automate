export interface Org {
  id: string;
  server_id: string;
  name: string;
  admin_user: string;
  projects?: string[];
}
