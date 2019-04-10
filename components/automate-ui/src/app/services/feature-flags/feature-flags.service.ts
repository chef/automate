import { Injectable } from '@angular/core';

const FFKEY = 'featureFlags.undefined';

@Injectable()
export class FeatureFlagsService {

  private _features: Object = {};

  constructor() {
    this.loadLocalStorage();
  }

  loadLocalStorage() {
    Object.keys(localStorage)
      .filter((item: string) => item.startsWith(FFKEY))
      .forEach(item => {
        this._features[item.substr(23)] = (localStorage.getItem(item) === 'true');
    });
  }

  setFeature(key: string, status: boolean): void {
    this._features[key] = status;
    this.serialize();
  }

  getFeatureStatus(key: string): boolean {
    return this._features[key] || false;
  }

  private serialize(): void {
    Object.keys(this._features)
      .forEach((feature: string) => {
        localStorage.setItem(`${FFKEY}.${feature}`,
          this._features[feature]);
      });
  }
}
