import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { mapKeys, snakeCase } from 'lodash/fp';

import { environment as env } from 'environments/environment';
import { IAMMajorVersion } from 'app/entities/policies/policy.model';
import { Team, TeamV1, ensureTeamV2 } from './team.model';
import { CreateTeamPayload, TeamUserMgmtPayload, CreateTeamV1Payload } from './team.actions';

export interface TeamsResponse {
  teams: Team[];
}

export interface TeamResponse {
  team: Team;
}

export interface UsersResponse {
  user_ids: string[];
}

export function versionizeTeam(resp: TeamV1 | Team): Team {
  return ensureTeamV2(resp);
}

@Injectable()
export class TeamRequests {

  constructor(private http: HttpClient) { }

  public getTeams(version: IAMMajorVersion = 'v1'): Observable<TeamsResponse> {
    switch (version) {
      case 'v2': {
        return this.http.get<TeamsResponse>(`${env.auth_v2_url}/teams`);
      }
      default: {
        return this.http.get<TeamsResponse>(`${env.auth_url}/teams`);
      }
    }
  }

  // TODO try passing a Team object so we can get the id or guid depending on v1 or v2
  // or else we need to migrate v1 team names -> v1 team ids before completing this work
  public getTeam(id: string, version: IAMMajorVersion = 'v1'): Observable<TeamResponse> {
    switch (version) {
      case 'v2': {
        return this.http.get<TeamResponse>(`${env.auth_v2_url}/teams/${id}`);
      }
      default: {
        return this.http.get<TeamResponse>(`${env.auth_url}/teams/${id}`);
      }
    }
  }

  public getTeamUsers(id: string, version: IAMMajorVersion = 'v1'): Observable<UsersResponse> {
    switch (version) {
      case 'v2': {
        return this.http.get<UsersResponse>(`${env.auth_v2_url}/teams/${id}/users`);
      }
      default: {
        return this.http.get<UsersResponse>(`${env.auth_url}/teams/${id}/users`);
      }
    }
   }

  public createTeam(
    teamData: CreateTeamPayload, version: IAMMajorVersion = 'v1'): Observable<TeamResponse> {
    switch (version) {
      case 'v2': {
        return this.http.post<TeamResponse>(
          `${env.auth_v2_url}/teams`, mapKeys(snakeCase, teamData));
      }
      default: {
        return this.http.post<TeamResponse>(`${env.auth_url}/teams`,
          mapKeys(snakeCase, <CreateTeamV1Payload>{
            name: teamData.id,
            description: teamData.name
          }));
      }
    }
  }

  public updateTeam(
    teamData: Team, version: IAMMajorVersion = 'v1'): Observable<TeamResponse> {
      switch (version) {
        case 'v2': {
          return this.http.put<TeamResponse>(
            `${env.auth_v2_url}/teams/${teamData.id}`, mapKeys(snakeCase, teamData));
        }
        default: {
          return this.http.put<TeamResponse>(
            `${env.auth_url}/teams/${teamData.guid}`, mapKeys(snakeCase, <TeamV1>{
                id: teamData.guid,
                name: teamData.id,
                description: teamData.name
            }));
        }
      }
  }

  public deleteTeam(team: Team, version: IAMMajorVersion = 'v1'): Observable<TeamResponse> {
    switch (version) {
      case 'v2': {
        return this.http.delete<TeamResponse>(`${env.auth_v2_url}/teams/${team.id}`);
      }
      default: {
        return this.http.delete<TeamResponse>(`${env.auth_url}/teams/${team.guid}`);
      }
    }
  }

  public addTeamUsers(
    teamData: TeamUserMgmtPayload, version: IAMMajorVersion = 'v1'):
      Observable<UsersResponse | Team> {
      switch (version) {
        case 'v2': {
          return this.http.post<UsersResponse>(
            `${env.auth_v2_url}/teams/${teamData.id}/users:add`, mapKeys(snakeCase, teamData));
        }
        default: {
          return this.http.post<Team>(
            `${env.auth_url}/teams/${teamData.id}/users`, mapKeys(snakeCase, teamData));
        }
      }
  }

   public removeTeamUsers(
    teamData: TeamUserMgmtPayload, version: IAMMajorVersion = 'v1'):
      Observable<UsersResponse | Team> {
      switch (version) {
        case 'v2': {
          return this.http.post<UsersResponse>(
            `${env.auth_v2_url}/teams/${teamData.id}/users:remove`, mapKeys(snakeCase, teamData));
        }
        default: {
          return this.http.put<Team>(
            `${env.auth_url}/teams/${teamData.id}/users`, mapKeys(snakeCase, teamData));
        }
      }
  }
}
