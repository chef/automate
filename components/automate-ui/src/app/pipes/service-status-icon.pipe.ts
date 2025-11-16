import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  standalone: false,
  name: 'serviceStatusIcon'
})

export class ServiceStatusIconPipe implements PipeTransform {
  transform(value: string): string {
    switch (value) {
      case 'OK':
        return 'check_circle';

      case 'CRITICAL':
        return 'warning';

      case 'UNKNOWN':
      case 'DEPLOYING':
        return 'help';

      case 'WARNING':
        return 'error';

      default:
        return 'lens';
    }
  }
}
