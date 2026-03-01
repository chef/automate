import { Directive, HostBinding } from '@angular/core';

@Directive({
  standalone: false,
  selector: 'input[chefInput], textarea[chefInput]',
  exportAs: 'chefInput'
})
export class InputDirective {
  @HostBinding('class') class = 'chef-input';
}
