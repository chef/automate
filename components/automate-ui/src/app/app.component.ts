import { Component } from '@angular/core';
import { environment } from "../environments/environment"

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html'
})
export class AppComponent {
  library_url: string;
  
  constructor() {
    this.library_url = environment.remote_library_url;
    this.loadScript(this.library_url, function (){});
  }

  loadScript(path, callback) {
    const scriptTag = document.createElement("script");
    scriptTag.src = path;
    scriptTag.onload = callback;
    document.body.appendChild(scriptTag);
  }
}
