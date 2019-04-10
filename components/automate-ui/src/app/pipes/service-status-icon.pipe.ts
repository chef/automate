import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'serviceStatusIcon'
})

export class ServiceStatusIconPipe implements PipeTransform {
  transform(value: string): string {
    switch (value) {
      case 'OK':
        return 'check_circle';

      case 'CRITICAL':
      case 'UNKNOWN':
        return 'warning';

      case 'DEPLOYING':
        return 'help';

      case 'WARNING':
        return 'error';

      default:
        return 'lens';
    }
  }
}
