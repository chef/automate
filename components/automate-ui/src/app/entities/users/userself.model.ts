export interface SelfUser {
  id: string;
  name: string;
  username: string;
  password?: string; // only used for updating, never returned by the API
  previous_password?: string; // only used for updating, never returned by the API
}
