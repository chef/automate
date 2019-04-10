import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-tab',
  template: `
  <div [hidden]="!active" class="pane">
    <ng-content></ng-content>
  </div>
`
})
// TODO:eng-ex The active tab should be tracked in the parent and not the individual tab
export class TabComponent {
  @Input() tabTitle: string;
  @Input() active = false;
}
