
// export interface UserPreference {
//   id: string;
//   name: string;
//   value: any;
//   enabled:  boolean;
// }
export interface UserPreference {
  value: string;
  disabled: boolean;
}

export interface UserPreferencesPayload {
  user_preferences: {
    timezone: UserPreference
  };
}
