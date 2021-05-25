import { HttpClient, HttpBackend } from '@angular/common/http';
import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})

interface ConfigTypes {
  show?: boolean;
  message?: string;
  background_color?: string;
  text_color?: string;
}

export class AppConfigService {

  public appConfig: ConfigTypes;

  constructor(private handler: HttpBackend) { }

  public loadAppConfig() {
    return new HttpClient(this.handler).get('/banner.js')
      .toPromise()
      .then(data => this.appConfig = data);
  }

  get showBanner(): boolean {
    return this.appConfig.show;
  }

  get bannerMessage(): string {
    return this.appConfig.message;
  }

  get bannerBackgroundColor(): string {
    const backgroundColor = this.convertToHex(this.appConfig.background_color)
    return backgroundColor;
  }

  get bannerTextColor(): string {
    const textColor = this.convertToHex(this.appConfig.text_color);
    return textColor;
  }

  public convertToHex(color: string): string {
    return `#${color}`
  }
}
