import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-select-list-item',
  templateUrl: './select-list-item.component.html'
})
export class SelectListItemComponent {
  @Input() item: string;
  @Input() filter: string;
  constructor() { }
}

