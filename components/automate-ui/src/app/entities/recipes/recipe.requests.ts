import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment as env } from 'environments/environment';
import { RecipesSuccessPayload } from './recipe.action';
import { InterceptorSkipHeader } from 'app/services/http/http-client-auth.interceptor';

const headers = new HttpHeaders().set(InterceptorSkipHeader, '');

@Injectable()
export class RecipeRequests {

  constructor(private http: HttpClient) { }

  public getRecipes(server_id: string, org_id: string, name: string):
    Observable<RecipesSuccessPayload> {
      return this.http.get<RecipesSuccessPayload>(
        `${env.infra_proxy_url}/servers/${server_id}/orgs/${org_id}/environments/${name}/recipes`,
        {headers});
  }
}
