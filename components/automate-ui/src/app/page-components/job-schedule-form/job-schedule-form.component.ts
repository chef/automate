import { Component, Input } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { RRule } from 'rrule';
import * as moment from 'moment/moment';

@Component({
  selector: 'chef-job-schedule-form',
  templateUrl: './job-schedule-form.component.html',
  styleUrls: ['./job-schedule-form.component.scss']
})
export class JobScheduleFormComponent {
  @Input() form: FormGroup;

  public RRule = RRule;

  public months = Array(12).fill(1).map((_, i) => i);

  // returns an Array of months in short form i.e. ['Jan', 'Feb', 'Mar', ...]
  public monthsStringArray = moment.monthsShort();

  public dates = Array(31).fill(1).map((_, i) => i + 1);

  public years = Array(20).fill(1).map((_, i) => i + 2018);

  public hours = Array(24).fill(1).map((_, i) => i);

  public minutes = Array(60).fill(1).map((_, i) => i);

}
