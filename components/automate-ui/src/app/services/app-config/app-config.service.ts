import { HttpClient, HttpBackend } from '@angular/common/http';
import { Injectable } from '@angular/core';

interface BannerConfigTypes {
  show?: boolean | null;
  message?: string | null;
  background_color?: string | null;
  text_color?: string | null;
}

interface SessionSettings {
  enable_idle_timeout?: boolean | null;
  idle_timeout_minutes?: number | null;
}

interface LargeReportingSettings {
  enable_large_reporting?: boolean | null;
}

interface ConfigTypes {
  banner?: BannerConfigTypes;
  session_settings?: SessionSettings;
  large_reporting?: LargeReportingSettings;
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
    },
    large_reporting: {
      enable_large_reporting: null
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
    return this.appConfig?.banner?.show || false;
  }

  get bannerMessage(): string {
    return this.appConfig?.banner?.message || "";
  }

  get idleTimeout(): number | null {
    return this.appConfig?.session_settings?.idle_timeout_minutes || null;
  }

  get isIdleTimeoutEnabled(): boolean {
    return this.appConfig?.session_settings?.enable_idle_timeout || false;
  }

  get bannerBackgroundColor(): string {
    return this.convertToHex(this.appConfig?.banner?.background_color || "");
  }

  get bannerTextColor(): string {
    return this.convertToHex(this.appConfig?.banner?.text_color || "");
  }

  get isLargeReportingEnabled(): boolean {
    return this.appConfig?.large_reporting?.enable_large_reporting || false;
  }

  private convertToHex(color: string): string {
    return `#${color}`;
  }
}
