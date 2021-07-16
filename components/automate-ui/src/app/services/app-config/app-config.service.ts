import { HttpClient, HttpBackend } from '@angular/common/http';
import { Injectable } from '@angular/core';

interface BannerConfigTypes {
  show?: boolean;
  message?: string;
  background_color?: string;
  text_color?: string;
}

interface SessionSettings {
  enable_idle_timeout?: boolean;
  idle_timeout_minutes?: number;
}

interface ConfigTypes {
  banner?: BannerConfigTypes;
  session_settings?: SessionSettings;
}

const initialConfig = {
    banner: {
      show: null,
      message: null,
      background_color: null,
      text_color: null
    },
    session_settings: {
      enable_idle_timeout: null,
      idle_timeout_minutes: null
    }
};

@Injectable({
  providedIn: 'root'
})

export class AppConfigService {

  public appConfig: ConfigTypes = initialConfig;

  constructor(private handler: HttpBackend) { }

  public loadAppConfig() {
    return new HttpClient(this.handler).get('/custom_settings.js')
      .toPromise()
      .then((data: any) => this.appConfig = data)
      // when there is no config, we can just reset the config to its initial empty values
      .catch(_error => this.appConfig = initialConfig);
  }

  get showBanner(): boolean {
    return this.appConfig.banner.show;
  }

  get bannerMessage(): string {
    return this.appConfig.banner.message;
  }

  get idleTimeout(): number {
    return this.appConfig.session_settings.idle_timeout_minutes;
  }

  get isIdleTimeoutEnabled(): boolean {
    return this.appConfig.session_settings.enable_idle_timeout;
  }

  get bannerBackgroundColor(): string {
    return this.convertToHex(this.appConfig.banner.background_color);
  }

  get bannerTextColor(): string {
    return this.convertToHex(this.appConfig.banner.text_color);
  }

  private convertToHex(color: string): string {
    return `#${color}`;
  }
}
