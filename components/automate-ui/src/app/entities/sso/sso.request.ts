import { HttpClient } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Observable } from "rxjs";

import { environment as env } from 'environments/environment';
import { Sso } from "./sso.model";

export interface SsoPayloadResponse {
    sso : Sso;
}

export interface GetAllConfigResponse<S> {
    sso: S[];
}

@Injectable()
export class SsoRequests {
    constructor(private http: HttpClient) {}

    public getAll(): Observable<GetAllConfigResponse<Sso>> {
        return this.http.get<GetAllConfigResponse<Sso>>(`${env.gateway_url}/sso/config`);
    }
}