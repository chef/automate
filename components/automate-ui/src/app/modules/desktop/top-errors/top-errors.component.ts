import { Component, Input } from '@angular/core';

import {
  TopErrorsItem
} from 'app/entities/desktop/desktop.model';

@Component({
  selector: 'app-top-errors',
  templateUrl: './top-errors.component.html',
  styleUrls: ['./top-errors.component.scss']
})
export class TopErrorsComponent {

  @Input() topErrorsItems: TopErrorsItem[];

  constructor() { }
}
