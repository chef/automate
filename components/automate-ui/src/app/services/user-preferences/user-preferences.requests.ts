import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { Injectable } from '@angular/core';

import {
  UserPreferenceResponse,
  UserPreferencesPayload
} from './user-preferences.model';
import { UserPreferencesService } from './user-preferences.service';
import { environment } from 'environments/environment';
const USER_PREFERENCE_URL = environment.user_preference_url;

@Injectable()
export class UserPreferencesRequests {

  constructor(private httpClient: HttpClient, private userPrefService: UserPreferencesService) {}

  public fetchUserPreferences(): Observable<UserPreferencesPayload> {
    const url = `${USER_PREFERENCE_URL}` + this.userPrefService.apiEndpoint;
    return this.httpClient.get<UserPreferencesPayload>(url);
  }

  public updateUserPreferences(userPreferences: UserPreferencesPayload): Observable<any> {
    const url = `${USER_PREFERENCE_URL}` + this.userPrefService.apiEndpoint;
    return this.httpClient.put<UserPreferenceResponse[]>(url, userPreferences);
  }
}
