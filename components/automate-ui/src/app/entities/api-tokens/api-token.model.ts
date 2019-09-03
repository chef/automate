export interface ApiToken {
  id: string;
  name: string;
  value: string;
  active: boolean;
  created_at: string;
  updated_at: string;
  projects: string[];
}

export interface ApiTokenV1 {
    id: string;
    description: string;
    value: string;
    active: boolean;
    created: string;
    updated: string;
}

export function ensureApiTokenV2(token: ApiToken | ApiTokenV1): ApiToken {
  if (isApiTokenV1(token)) {
    return {
      id: token.id,
      active: token.active,
      value: token.value,
      name: token.description,
      created_at: token.created,
      updated_at: token.updated,
      projects: []
    };
  }
  return token;
}

export function isApiTokenV1(token: ApiToken | ApiTokenV1): token is ApiTokenV1 {
  return (<ApiTokenV1>token).description !== undefined;
}
