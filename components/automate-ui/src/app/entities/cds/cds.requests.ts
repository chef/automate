import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { ContentItem } from './cds.model';

import { environment } from '../../../environments/environment';
const CDS_URL = environment.cds_url;

@Injectable()
export class CdsRequests {

  constructor(private http: HttpClient) { }

  public getContentItems(): Observable<ContentItem[]> {
    const url = `${CDS_URL}/items`;

    return this.http.get<ContentItem[]>(url);
  }
}
