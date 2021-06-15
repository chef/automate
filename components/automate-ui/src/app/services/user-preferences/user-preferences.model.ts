export interface UserPreference {
  value: string;
  disabled: boolean;
}

export interface UserPreferencesPayload {
  user_preferences: {
    timezone: UserPreference
  };
}
