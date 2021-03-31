
export interface UserPreference {
  id: string;
  name: string;
  value: any;
  enabled:  boolean;
}

export interface UserPreferencesPayload {
  user_preferences: UserPreference[];
}
