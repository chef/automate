import { Component, Input, OnInit, OnChanges, SimpleChange } from '@angular/core';

@Component({
  selector: '[chef-guitar-string-section]',
  templateUrl: './guitar-string-section.component.html',
  styleUrls: ['./guitar-string-section.component.scss']
})
export class GuitarStringSectionComponent implements OnInit, OnChanges {
  @Input() width: number;
  @Input() index: number;
  x: number;
  className: string;
  y = 0;

  ngOnInit() {
    this.x = this.width * this.index;
    if (this.index % 2 === 0) {
      this.className = 'guitar-string-section-even';
    } else {
      this.className = 'guitar-string-section-odd';
    }
  }

  ngOnChanges(changes: {[propertyName: string]: SimpleChange}) {
    if (changes['width']) {
      this.x = this.width * this.index;
    }
  }
}
