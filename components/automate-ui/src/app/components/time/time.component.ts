import { Component, Input } from '@angular/core';
import { UserPreferencesService } from '../../services/user-preferences/user-preferences.service';

@Component({
  selector: 'app-time',
  templateUrl: './time.component.html'
})
export class TimeComponent {
  @Input() time;

  constructor(
    public userPrefsService: UserPreferencesService
  ) {}

}
