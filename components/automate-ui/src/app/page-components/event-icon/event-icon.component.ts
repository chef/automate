import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-event-icon',
  templateUrl: './event-icon.component.html',
  styleUrls: ['./event-icon.component.scss']
})
export class EventIconComponent {
  @Input() group: boolean;
  @Input() type: number;
  @Input() task: number;

  getEventIcon(eventType): string {
    switch (eventType) {
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

  getEventTaskClass(task): string {
    switch (task) {
      case 'delete':
          return 'delete';
      case 'edit':
          return 'edit';
      case 'update':
          return 'edit';
      case 'create':
          return 'create';
      default:
          return '';
    }
  }

  getEventGroupClass() {
    return this.group ? 'group' : '';
  }
}
