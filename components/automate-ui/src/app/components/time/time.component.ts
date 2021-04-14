import { Component, Input } from '@angular/core';
import { UserPreferencesService } from 'app/services/user-preferences/user-preferences.service';

import { trigger, transition, animate, style } from '@angular/animations';
const flash = trigger('myAnim', [
  transition('* => *', [
    animate('.2s ease-out', style({ transform: 'scale(1.02)', color: 'blue' })),
    animate('.2s ease-in', style({ transform: 'scale(1)' }))
  ])
]);

@Component({
  selector: 'chef-time',
  templateUrl: './time.component.html',
  styleUrls: ['./time.component.scss'],
  animations: [ flash ]
})
export class TimeComponent {
  @Input() time;

  constructor(
    public userPrefsService: UserPreferencesService
  ) {}
}
