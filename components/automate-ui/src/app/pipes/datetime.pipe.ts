import { Pipe, PipeTransform } from '@angular/core';
import * as moment from 'moment/moment';

/**
 * The datetime pipe formats datetime strings or moment objects. It's
 * based around moment, so any of the format strings used with moment
 * should work. https://momentjs.com/docs/#/displaying/format/
 */
@Pipe({
  name: 'datetime'
})
export class DatetimePipe implements PipeTransform {

  public transform(value: moment.Moment | Date | string, formatStr: string): string {
    const datetime = moment.isMoment(value) ? value : moment.utc(value);
    return datetime.format(formatStr);
  }

}
