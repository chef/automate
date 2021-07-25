export interface UserPreferenceResponse {
  user: {
    name: string,
    connector: string
  };
  time_format: UserPreferenceTimeformat;
}

export interface UserPreferencesPayload {
  user: {
    name: string,
    connector: string
  };
  settings: {
    date_format: {
      value: string
    }
  };
}

export interface UserPreferenceTimeformat {
    value: string;
    valid_values?: string[];
}
