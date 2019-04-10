import { Component, Input } from '@angular/core';


@Component({
  selector: 'app-cl-api',
  templateUrl: './cl-api.component.html',
  styleUrls: ['./cl-api.component.scss']
})
export class ClApiComponent {
  @Input() for: string;
}
