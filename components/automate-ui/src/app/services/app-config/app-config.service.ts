import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})

// Angular APP_INITIALIZER documentation https://angular.io/api/core/APP_INITIALIZER
// Excellent detailed instructions https://stackoverflow.com/a/54793384
export class AppConfigService {

  private appConfig: any;

  constructor(private http: HttpClient) { }

  loadAppConfig() {
    return this.http.get('/assets/app-config.json')
      .toPromise()
      .then(data => this.appConfig = data);
  }

  get showBanner() {
    if (!this.appConfig) {
      throw Error('Config file not loaded');
    }

    return this.appConfig.showBanner;
  }

  get bannerMessage() {
    return this.appConfig.bannerMessage;
  }

  get bannerBackgroundColor() {
    return this.appConfig.bannerBackgroundColor;
  }

  get bannerTextColor() {
    return this.appConfig.bannerTextColor;
  }
}
