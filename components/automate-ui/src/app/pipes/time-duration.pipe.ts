import { Pipe, PipeTransform } from '@angular/core';
import * as moment from 'moment';

@Pipe({
  name: 'timeDuration'
})
export class TimeDurationPipe implements PipeTransform {

  transform(amount: number, unit: string): string {
    return moment.duration(amount, unit as moment.unitOfTime.DurationConstructor).humanize();
  }

}
