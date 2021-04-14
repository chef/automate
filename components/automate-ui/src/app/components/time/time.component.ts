import { Component, Input } from '@angular/core';
import { UserPreferencesService } from 'app/services/user-preferences/user-preferences.service';

@Component({
  selector: 'chef-time',
  templateUrl: './time.component.html',
  styleUrls: ['./time.component.scss']
})
export class TimeComponent {
  @Input() time;

  constructor(
    public userPrefsService: UserPreferencesService
  ) {}
}
