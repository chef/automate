import { Pipe, PipeTransform } from '@angular/core';
import * as moment from 'moment';

/**
 * The datetime pipe formats datetime strings or moment objects. It's
 * based around moment, so any of the format strings used with moment
 * should work. https://momentjs.com/docs/#/displaying/format/
 */
@Pipe({
  name: 'datetime'
})
export class DatetimePipe implements PipeTransform {

  public transform(value: moment.Moment | string, formatStr: string): string {
    const datetime = moment.isMoment(value) ? value : moment(value);
    return datetime.format(formatStr);
  }

}
