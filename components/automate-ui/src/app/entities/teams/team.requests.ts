import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { mapKeys, snakeCase } from 'lodash/fp';

import { environment as env } from 'environments/environment';
import { Team } from './team.model';
import { CreateTeamPayload, TeamUserMgmtPayload } from './team.actions';

export interface TeamsResponse {
  teams: Team[];
}

export interface TeamResponse {
  team: Team;
}

export interface UsersResponse {
  membership_ids: string[];
}

@Injectable()
export class TeamRequests {

  constructor(private http: HttpClient) { }

  public getTeams(): Observable<TeamsResponse> {
    return this.http.get<TeamsResponse>(`${env.iam_url}/teams`);
  }

  public getTeam(id: string): Observable<TeamResponse> {
    return this.http.get<TeamResponse>(`${env.iam_url}/teams/${id}`);
  }

  public getTeamUsers(id: string): Observable<UsersResponse> {
    return this.http.get<UsersResponse>(`${env.iam_url}/teams/${id}/users`);
  }

  public createTeam(teamData: CreateTeamPayload): Observable<TeamResponse> {
    return this.http.post<TeamResponse>(`${env.iam_url}/teams`, mapKeys(snakeCase, teamData));
  }

  public updateTeam(teamData: Team): Observable<TeamResponse> {
    return this.http.put<TeamResponse>(`${env.iam_url}/teams/${teamData.id}`,
      mapKeys(snakeCase, teamData));
  }

  public deleteTeam(team: Team): Observable<TeamResponse> {
    return this.http.delete<TeamResponse>(`${env.iam_url}/teams/${team.id}`);
  }

  public addTeamUsers(teamData: TeamUserMgmtPayload):
    Observable<UsersResponse> {

    return this.http.post<UsersResponse>(
      `${env.iam_url}/teams/${teamData.id}/users:add`, mapKeys(snakeCase, teamData));
  }

   public removeTeamUsers(teamData: TeamUserMgmtPayload):
    Observable<UsersResponse | Team> {

    return this.http.post<UsersResponse>(
      `${env.iam_url}/teams/${teamData.id}/users:remove`, mapKeys(snakeCase, teamData));
  }
}
