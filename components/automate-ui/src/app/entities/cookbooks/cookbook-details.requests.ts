import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { environment as env } from 'environments/environment';
import { CookbookDetails, RespCookbookDetails } from './cookbook-details.model';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

@Injectable()
export class CookbookDetailsRequests {

  constructor(private http: HttpClient) { }

  public getCookbookDetails(
    server_id: string, org_id: string, cookbook_name: string, cookbook_version: string):
    Observable<CookbookDetails> {
    const headers = new HttpHeaders().set(InterceptorSkipHeader, '');
    return this.http.get<RespCookbookDetails>(
      `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/cookbooks/${cookbook_name}/${cookbook_version}`, { headers })
      .pipe(
        map((respCookbookDetails:
          RespCookbookDetails) => this.createCookbookDetails(respCookbookDetails)));
  }

  private createCookbookDetails(respCookbookDetails: RespCookbookDetails): CookbookDetails {
    return {
      cookbook_name: respCookbookDetails.cookbook_name,
      name_and_version: name,
      version: respCookbookDetails.version,
      chef_type: respCookbookDetails.chef_type,
      frozen: respCookbookDetails.frozen,
      json_class: respCookbookDetails.json_class,
      files: respCookbookDetails.files,
      templates: respCookbookDetails.templates,
      attributes: respCookbookDetails.attributes,
      recipes: respCookbookDetails.recipes,
      definitions: respCookbookDetails.definitions,
      libraries: respCookbookDetails.libraries,
      providers: respCookbookDetails.providers,
      resources: respCookbookDetails.resources,
      root_files: respCookbookDetails.root_files,
      metadata: respCookbookDetails.metadata,
      access: respCookbookDetails.access
    };
  }
}
