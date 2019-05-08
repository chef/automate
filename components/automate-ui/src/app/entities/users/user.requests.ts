import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { mapKeys, snakeCase } from 'lodash/fp';

import { environment as env } from 'environments/environment';
import { IAMMajorVersion } from 'app/entities/policies/policy.model';
import { CreateUserPayload } from './user.actions';
import { User } from './user.model';
import { SelfUser } from './userself.model';

export interface UsersResponse {
  users: User[];
}

export interface UsersResponseV1 {
  users: UserV1[];
}

interface UserResponse {
  user: User;
}

interface CreateUserPayloadV1 {
  username: string;
  name: string;
  password: string;
}

interface UserV1 {
  id: string;
  name: string;
  username: string;
  password?: string; // only used for updating, never returned by the API
}

function convertUserV1ToUser(user: UserV1): User {
  return {
    id: user.username,
    name: user.name,
    membership_id: user.id,
    projects: [] // TODO add real projects
  };
}

function convertUserToUserV1(user: User): UserV1 {
  return {
    username: user.id,
    name: user.name,
    id: user.membership_id,
    password: user.password
  };
}

function convertCreateUserPayloadToV1(userPayload: CreateUserPayload): CreateUserPayloadV1 {
  return {
    username: userPayload.id,
    name: userPayload.name,
    password: userPayload.password
  };
}

@Injectable()
export class UserRequests {

  constructor(private http: HttpClient) { }

  public getUsers(version: IAMMajorVersion = 'v1'):
    Observable<UsersResponse> {
    switch (version) {
      case 'v2': {
        return this.http.get<UsersResponse>(`${env.auth_v2_url}/users`);
      }
      default: {
        return this.http.get(`${env.auth_url}/users`).pipe(map((usersResponse: UsersResponseV1) => {
          return <UsersResponse>{
            users: usersResponse.users.map((user: UserV1) => convertUserV1ToUser(user))
          };
        }));
      }
    }
  }

  public getUser(id: string,
    version: IAMMajorVersion = 'v1'): Observable<User> {

    switch (version) {
      case 'v2': {
        return this.http.get<UserResponse>(`${env.auth_v2_url}/users/${id}`)
          .pipe(map(userResponse => userResponse.user));
      }
      default: {
        return this.http.get<UserV1>(`${env.auth_url}/users/${id}`)
          .pipe(map(userV1 => convertUserV1ToUser(userV1)));
      }
    }
  }

  public createUser(userData: CreateUserPayload,
    version: IAMMajorVersion = 'v1'): Observable<User> {

    switch (version) {
      case 'v2': {
        return this.http.post<UserResponse>(`${env.auth_v2_url}/users`,
          mapKeys(snakeCase, userData))
            .pipe(map(userResponse => userResponse.user));
      }
      default: {
        return this.http.post<UserV1>(`${env.auth_url}/users`,
          mapKeys(snakeCase, convertCreateUserPayloadToV1(userData)))
            .pipe(map(userV1 => convertUserV1ToUser(userV1)));
      }
    }
  }

  public updateUser(userData: User,
    version: IAMMajorVersion = 'v1'): Observable<User> {

    switch (version) {
      case 'v2': {
        return this.http.put<UserResponse>(`${env.auth_v2_url}/users/${userData.id}`,
          mapKeys(snakeCase, userData))
            .pipe(map(userResponse => userResponse.user));
      }
      default: {
        return this.http.put<UserV1>(`${env.auth_url}/users/${userData.id}`,
          mapKeys(snakeCase, convertUserToUserV1(userData)))
            .pipe(map(userV1 => convertUserV1ToUser(userV1)));
      }
    }
  }

  public updateSelf(userData: SelfUser, version: IAMMajorVersion = 'v1'): Observable<User> {
      switch (version) {
        case 'v2': {
          return this.http.put<UserResponse>(`${env.auth_v2_url}/self/${userData.id}`,
            mapKeys(snakeCase, userData))
              .pipe(map(userResponse => userResponse.user));
        }
        default: {
          const updateSelfV1Payload = {
            username: userData.id,
            name: userData.name,
            id: userData.membership_id,
            password: userData.password,
            previous_password: userData.previous_password
          };
          return this.http.put<UserV1>(`${env.users_url}/${userData.id}`,
            mapKeys(snakeCase, updateSelfV1Payload))
              .pipe(map(userV1 => convertUserV1ToUser(userV1)));
        }
      }
  }

  public deleteUser(user: User,
    version: IAMMajorVersion = 'v1'): Observable<User> {

    switch (version) {
      case 'v2': {
        return this.http.delete<UserResponse>(`${env.auth_v2_url}/users/${user.id}`).pipe(
          map(() => user)); // bounce it back, so it can be used
      }
      default: {
        return this.http.delete<UserV1>(`${env.auth_url}/users/${user.id}`).pipe(
          map(() => user)); // bounce it back, so it can be used
      }
    }
  }
}
