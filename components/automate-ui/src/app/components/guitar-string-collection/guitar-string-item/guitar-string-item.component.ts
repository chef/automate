import { Component, Input, OnInit,
  OnChanges, SimpleChange } from '@angular/core';
import { EventHelper } from 'app/helpers/event-feed/event-helper';

@Component({
  selector: '[chef-guitar-string-item]',
  templateUrl: './guitar-string-item.component.html',
  styleUrls: ['./guitar-string-item.component.scss']
})
export class GuitarStringItemComponent implements OnInit, OnChanges  {
  @Input() y: number;
  @Input() index: number;
  @Input() space: number;
  @Input() iconRadius = 7;
  @Input() eventType = 'help';
  @Input() multiple = false;
  @Input() task = 'create';
  x: number;

  ngOnInit() {
    this.updateX();
  }

  ngOnChanges(changes: {[propertyName: string]: SimpleChange}) {
    if (changes['space']) {
      this.updateX();
    }
  }

  getEventIcon(): string {
    return EventHelper.getEventIcon(this.eventType);
  }

  isUpdate(): boolean {
    return this.task === 'update';
  }

  private updateX(): void {
    // Find the x position based off the items radiuses, space between items, and its index
    // with in the collection.
    // The x is the center of the item.
    // The zeroth item it would be located at space + the icon's
    // radius (this.space + this.iconRadius.)
    // For each follow on item, add a space plus the width of the icon
    // (this.index * (this.space + this.iconRadius * 2).)
    this.x = this.space + this.iconRadius + this.index * (this.space + this.iconRadius * 2) ;
  }
}
