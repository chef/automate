import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { environment as env } from '../../../environments/environment';
import { Profile } from './profile.model';
import { ProfilesSearchPayload } from './profile.actions';

export interface ProfilesSearchResponse {
  profiles: Profile[];
  total: number;
}

@Injectable()
export class ProfileRequests {

  constructor(private http: HttpClient) {}

  public profilesSearch(body: ProfilesSearchPayload): Observable<ProfilesSearchResponse> {
    return this.http.post<ProfilesSearchResponse>(`${env.compliance_url}/profiles/search`, body)
      .pipe(
        map(resp => {
          // API doesn't give profiles an ID
          const profiles = resp.profiles.map(profile => {
            const {owner, name, version} = profile;
            const id = `compliance://${owner}/${name}#${version}`;
            return {...profile, id};
          });
          return {...resp, profiles};
        }));
  }

}
