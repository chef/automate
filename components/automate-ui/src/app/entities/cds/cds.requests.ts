import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { ContentItem } from './cds.model';
import { map as mapRxjs } from 'rxjs/operators';
import { map } from 'lodash/fp';

import { environment } from '../../../environments/environment';
const CDS_URL = environment.cds_url;

interface RespContentItems {
  items: RespContentItem[];
}

interface RespContentItem {
  id: string;
  name: string;
  type: string;
  description: string;
  status: string;
  version: string;
  platforms: string[];
  can_be_installed: boolean;
  filename: string;
}

@Injectable()
export class CdsRequests {

  constructor(private http: HttpClient) { }

  public getContentItems(): Observable<ContentItem[]> {
    const url = `${CDS_URL}/items`;

    return this.http.get<RespContentItems>(url).pipe(
      mapRxjs(resp => this.convertToContentItems(resp)));
  }

  public installContentItem(id: string, user: string): Observable<any> {
    const url = `${CDS_URL}/install`;

    return this.http.post<any>(url, {id, request_user: user});
  }

  public downloadContentItem(id: string): Observable<Blob> {
    const url = `${CDS_URL}/download`;
    const body = {id};
    return this.http.post(url, body, {responseType: 'blob'});
  }

  private convertToContentItems(resp: RespContentItems): ContentItem[] {
    return map((respItem: RespContentItem) => this.convertToContentItem(respItem), resp.items);
  }

  private convertToContentItem(respItem: RespContentItem): ContentItem {
    return {
      id: respItem.id,
      name: respItem.name,
      type: respItem.type,
      description: respItem.description,
      version: respItem.version,
      platforms: respItem.platforms,
      canBeInstall: respItem.can_be_installed,
      filename: respItem.filename
    };
  }
}
