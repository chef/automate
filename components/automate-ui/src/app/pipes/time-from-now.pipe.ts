import { Pipe, PipeTransform } from '@angular/core';
import * as moment from 'moment';

@Pipe({
  name: 'timeFromNow'
})
export class TimeFromNowPipe implements PipeTransform {

  transform(value): string {
    const dateTime = moment.isMoment(value) ? value : moment(value);
    // `null` dates from API are currently sent as "beginning of time"
    return dateTime.isBefore('2000-01-01T00:00:00.000Z') ? '-' : dateTime.fromNow();
  }

}
