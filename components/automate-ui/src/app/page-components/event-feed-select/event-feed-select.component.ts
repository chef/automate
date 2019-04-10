import {
  Component,
  EventEmitter,
  Input,
  Output
} from '@angular/core';
import { EventTypeCount } from '../../types/types';

@Component({
  selector: 'app-event-feed-select',
  templateUrl: './event-feed-select.component.html',
  styleUrls: ['./event-feed-select.component.scss']
})
export class EventFeedSelectComponent {
  @Output() newValue: EventEmitter<string> = new EventEmitter();
  @Input() eventTypeCount: EventTypeCount = EventTypeCount.Null;
  @Input() selectedValue = 'total';
  @Input() disabled = false;

  constructor() {}

  getEventCount(eventType: string): number {
    if (eventType === '') {
      eventType = 'total';
    }

    switch (eventType) {
      case 'cookbook':
        return +this.eventTypeCount[eventType] + +this.eventTypeCount['version'];
      case 'bag':
        return +this.eventTypeCount[eventType] + +this.eventTypeCount['item'];
      default:
        return this.eventTypeCount[eventType];
    }
  }
}

