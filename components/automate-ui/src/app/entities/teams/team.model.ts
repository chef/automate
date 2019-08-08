export interface Team {
  id: string;
  name: string;
  guid: string; // to be deprecated after GA
  projects: string[]; // TODO revisit projects
}

export interface TeamV1 {
  description: string;
  id: string;
  name: string;
}

export function ensureTeamV2(team: Team | TeamV1): Team {
  if (isTeamV1(team)) {
    return {
      id: team.name, // v1 team names are unique
      name: team.description, // v1 team description corresponds to v2 team name
      guid: team.id,
      // WARNING (tc): /api/v0/teams never returns the projects field.
      // We should never use the v0 teams API in the UI when projects matter.
      projects: []
    };
  }
  return team;
}

export function isTeamV1(team: Team | TeamV1): team is TeamV1 {
  return (<TeamV1>team).description !== undefined;
}
