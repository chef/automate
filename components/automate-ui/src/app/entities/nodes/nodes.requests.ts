import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import * as actions from './nodes.actions';
import { environment } from '../../../environments/environment';


export interface RespNodes {
  nodes: Node[];
  total: number;
  total_unreachable: number;
  total_reachable: number;
  total_unknown: number;
}

export interface Node {
  id: string;
  name: string;
}

@Injectable()
export class NodesRequests {

  constructor(private http: HttpClient) { }

  public listNodes(payload: actions.SearchNodesPayload):
  Observable<any> {
    const url = `${environment.nodes_url}/search`;
    const { page, per_page, order, sort, filters } = payload;
    const body = { filters, page, per_page, order, sort };
    return this.http.post(url, body);
  }
}
