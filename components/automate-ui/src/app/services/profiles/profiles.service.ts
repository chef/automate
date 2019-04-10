import { map } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from 'environments/environment';

const CC_API_URL = environment.compliance_url;

@Injectable()
export class ProfilesService {

  constructor(
    private httpClient: HttpClient
  ) {}

  getVersion() {
    const url = `${CC_API_URL}/version`;
    return this.httpClient.get<any>(url);
  }

  // Get all the compliance profiles for the asset storage
  getAllProfiles(owner: string): Observable<any> {
    const url = `${CC_API_URL}/profiles/search`;
    const body = {owner};
    return this.httpClient.post<any>(url, body).pipe(
      map(resp => resp.profiles || []));
  }

  // Get a specific compliance profile for the asset storage
  getProfile(owner: string, name: string, version: string): Observable<any> {
    const url = `${CC_API_URL}/profiles/read/${owner}/${name}/version/${version}`;
    return this.httpClient.get<any>(url);
  }

  // Delete a specific compliance profile for the asset storage
  deleteProfile(owner: string, name: string, version: string): Observable<any> {
    const url = `${CC_API_URL}/profiles/${owner}/${name}/version/${version}`;
    return this.httpClient.delete(url);
  }

  getTarFile(owner: string, name: string, version: string): Observable<Blob> {
    const url = `${CC_API_URL}/profiles/tar`;
    const body = {owner, name, version};
    return this.httpClient.post(url, body, {responseType: 'blob'});
  }
}
