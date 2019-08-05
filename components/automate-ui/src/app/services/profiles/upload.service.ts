/*
 This service provides upload with progress reporting. At the moment, Angular2 doesn't supply
 this functionality in its $http module. As soon as this changes, we can safely replace the
 UploadService class with an upload method in profiles.service.ts
*/
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { environment } from 'environments/environment';

@Injectable()
export class UploadService {

  constructor(
    private chefSessionService: ChefSessionService
  ) {}

  sendFile(file: File): Observable<any> { /* tslint:disable */
    let contentType = this.estimateContentType(file.type, file.name);
    let owner = this.chefSessionService.username;
    let url = `${environment.compliance_url}/profiles?contentType=${contentType}&owner=${owner}`;
    let done = false;
    return new Observable(observer => {
      let formData: FormData = new FormData();
      let xhr: XMLHttpRequest = new XMLHttpRequest();

      if (file) {
        formData.append('file', file, file.name);
      }

      xhr.onreadystatechange = () => {
        if (xhr.readyState === 4) {
          done = true;
          let progress = this.getProgress(file.name, 100, xhr.status, xhr.response);
          if (xhr.status === 200) {
            progress = {...progress, loading: false, loaded: true};
          } else {
            progress = {...progress, loading: false, failed: true};
          }
          observer.next(progress);
          observer.complete();
        }
      };

      xhr.upload.onprogress = (event) => {
        // done prevents percent <100 from being sent after a remote server error
        if (done == false) {
          let percent = Math.round(event.loaded / event.total * 100);
          let progress = this.getProgress(file.name, percent);
          progress = {...progress, loading: true};
          observer.next(progress);
        }
      };

      xhr.onloadend = _event => {
        if (xhr.status === 401) {
          this.chefSessionService.logout();
        }
      }

      xhr.open('POST', url, true);
      xhr.setRequestHeader('Authorization', `Bearer ${this.chefSessionService.id_token}`);
      xhr.send(formData);
    });
  }

  private getProgress(file: string, percent: number, status?: number, response?: string): any {
    return {
      'name': file,
      'percent': percent,
      'status': status,
      'response': response
    };
  }

  private estimateContentType(mime, name): string {
    switch (mime) {
      case 'application/gzip':
      case 'application/x-gzip':
      case 'application/x-gtar':
      case 'application/zip':
      case 'application/x-zip-compressed':
        return mime;
    }
    if (name == null) return null;
    if (name.endsWith('.tgz') || name.endsWith('.tar.gz')) return 'application/gzip';
    if (name.endsWith('.zip')) return 'application/zip';
    return null;
  }
}
