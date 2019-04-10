import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient, HttpParams, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { ChefSessionService } from '../chef-session/chef-session.service';

import { environment } from 'environments/environment';

const CC_API_URL = environment.compliance_url;

@Injectable()
export class AvailableProfilesService {

  constructor(
    private chefSessionService: ChefSessionService,
    private httpClient: HttpClient
  ) {}

  // Get all the profiles
  getAllProfiles(): Observable<any> {
    return this.httpClient.post<any>(`${CC_API_URL}/profiles/search`, {}).pipe(
      map(resp => resp.profiles || []));
  }

  // Get a specific profile (name, version)
  getProfile(name: string, version: string): Observable<any> {
    return this.httpClient.get<any>(`${CC_API_URL}/market/read/${name}/version/${version}`);
  }

  installMarketProfile(name: string, version: string): Observable<any> {
    const url = `${CC_API_URL}/profiles`;
    const body = { name, version };
    const params = new HttpParams().set('owner', this.chefSessionService.username);
    const headers = new HttpHeaders({'Content-Type': 'application/json'});

    return this.httpClient.post(url, body, {params, headers});
  }

  // Get a tarfile for a profile (name required, version optional)
  // no version returns latest
  getTarFile(name: string, version: string): Observable<Blob> {
    const url = `${CC_API_URL}/profiles/tar`;
    const body = {name, version};
    return this.httpClient.post(url, body, {responseType: 'blob'});
  }
}
