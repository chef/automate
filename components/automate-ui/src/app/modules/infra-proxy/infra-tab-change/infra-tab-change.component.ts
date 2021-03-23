import { Component, ContentChildren, EventEmitter, Output, QueryList } from '@angular/core';
import { InfraTabComponent } from './infra-tab/infra-tab.component';

@Component({
  selector: 'app-infra-tab-change',
  templateUrl: './infra-tab-change.component.html',
  styleUrls: ['./infra-tab-change.component.scss']
})

export class InfraTabChangeComponent {

  @Output() tabChange: EventEmitter<number> = new EventEmitter<number>();

  @ContentChildren(InfraTabComponent) tabs: QueryList<InfraTabComponent>;

  selectTab(tab: InfraTabComponent) {
    const tabs = this.tabs.toArray();

    // deactivate all tabs
    tabs.forEach(t => t.active = false);

    // activate the tab the user has clicked on.
    tab.active = true;

    this.tabChange.emit(tabs.indexOf(tab));
  }
}
