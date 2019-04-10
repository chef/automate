import { Component, Input } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { last, startCase, isUndefined } from 'lodash';

@Component({
  selector: 'app-component-library-screen',
  templateUrl: './component-library-screen.component.html',
  styleUrls: ['./component-library-screen.component.scss']
})
export class ComponentLibraryScreenComponent {
  @Input() title: string;

  constructor(route: ActivatedRoute) {
    if (isUndefined(this.title)) {
      route.url.subscribe((url) => {
        if (url.length > 0) {
          this.title = startCase(last(url).path);
        }
      });
    }
  }
}
