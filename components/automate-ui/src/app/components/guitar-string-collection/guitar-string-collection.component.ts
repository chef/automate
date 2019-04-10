import { Component, Input } from '@angular/core';

@Component({
  selector: 'chef-guitar-string-collection',
  templateUrl: './guitar-string-collection.component.html'
})
export class GuitarStringCollectionComponent {
  @Input() height: number;
}
