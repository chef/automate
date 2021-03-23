import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-infra-tab',
  template: `
  <div [hidden]="!active" class="test">
    <ng-content></ng-content>
  </div>
`
})
// TODO:eng-ex The active tab should be tracked in the parent and not the individual tab
export class InfraTabComponent {
  @Input() tabTitle: string;
  @Input() active = false;
  @Input() disabled = true;
}
