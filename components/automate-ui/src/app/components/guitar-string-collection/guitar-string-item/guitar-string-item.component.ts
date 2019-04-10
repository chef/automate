import { Component, Input, OnInit,
  OnChanges, SimpleChange } from '@angular/core';

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
    switch (this.eventType) {
      case 'policy': return 'add_to_photos';
      case 'user': return 'person';
      case 'group': return 'people';
      case 'permission': return 'lock_open';
      case 'organization': return 'layers';
      case 'node': return 'storage';
      case 'cookbook': return 'chrome_reader_mode';
      case 'version': return 'chrome_reader_mode';
      case 'item': return 'business_center';
      case 'bag': return 'business_center';
      case 'environment': return 'public';
      case 'role': return 'book';
      case 'profile': return 'library_books';
      case 'scanjobs': return 'wifi_tethering';
      case 'client': return 'assignment_ind';
      case 'multiple': return 'more_horiz';
      default: return 'help';
    }
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
