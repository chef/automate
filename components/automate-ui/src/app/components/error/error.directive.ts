import { Directive, HostBinding } from '@angular/core';

@Directive({
  selector: 'chef-error',
  exportAs: 'chefError'
})
export class ErrorDirective {
  @HostBinding('class') class = 'chef-error';
}
