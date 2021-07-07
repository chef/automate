import { HttpClient, HttpBackend } from '@angular/common/http';
import { Injectable } from '@angular/core';

interface ConfigTypes {
  show?: boolean;
  message?: string;
  background_color?: string;
  text_color?: string;
  idle_timeout?: number
}

const initialConfig = {
    show: null,
    message: null,
    background_color: null,
    text_color: null,
    idle_timeout: null
};

@Injectable({
  providedIn: 'root'
})

export class AppConfigService {

  public appConfig: ConfigTypes = initialConfig;

  constructor(private handler: HttpBackend) { }

  public loadAppConfig() {
    debugger
    return new HttpClient(this.handler).get('/banner.js')
      .toPromise()
      .then(data => this.appConfig = data)
      .then(data => console.log(data, "dataaß"))
      // when there is no config, we can just reset the config to its initial empty values
      .catch(_error => console.log(_error, "error"));

  }

  get showBanner(): boolean {
    return this.appConfig.show;
  }

  get bannerMessage(): string {
    return this.appConfig.message;
  }

  get bannerBackgroundColor(): string {
    return this.convertToHex(this.appConfig.background_color);
  }

  get idleTimeout(): number {
    return Number(this.appConfig.idle_timeout);
  }

  get bannerTextColor(): string {
    return this.convertToHex(this.appConfig.text_color);
  }

  private convertToHex(color: string): string {
    return `#${color}`;
  }
}
