import { Directive, HostBinding } from '@angular/core';

@Directive({
  standalone: false,
  selector: 'chef-error',
  exportAs: 'chefError'
})
export class ErrorDirective {
  @HostBinding('class') class = 'chef-error';
}
