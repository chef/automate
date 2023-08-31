import { Component } from '@angular/core';
import { environment } from '../environments/environment';
import { LicenseUsageService } from './services/license-usage/license-usage.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html'
})
export class AppComponent {
  library_url: string;

  constructor(private licenseUsageService: LicenseUsageService) {
    this.library_url = environment.remote_library_url;
    this.loadScript(this.library_url, () => {
      this.licenseUsageService.postData();
    });
  }

  loadScript(path, callback) {
    const scriptTag = document.createElement('script');
    scriptTag.src = path;
    scriptTag.onload = callback;
    document.body.appendChild(scriptTag);
  }
}
