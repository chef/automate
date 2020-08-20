import { Component, Input } from '@angular/core';
import { EventHelper } from 'app/helpers/event-feed/event-helper';

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
    return EventHelper.getEventIcon(eventType);
  }

  getEventTaskClass(task): string {
    return EventHelper.getEventTaskClass(task);
  }

  getEventGroupClass() {
    return this.group ? 'group' : '';
  }
}
