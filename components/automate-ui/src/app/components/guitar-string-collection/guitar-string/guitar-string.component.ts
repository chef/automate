import { Component, Input } from '@angular/core';

@Component({
  selector: '[chef-guitar-string]',
  templateUrl: './guitar-string.component.html',
  styleUrls: ['./guitar-string.component.scss']
})
export class GuitarStringComponent {
  @Input() y: number;
}
