import { Component, ContentChildren, EventEmitter, Output, QueryList } from '@angular/core';
import { TabComponent } from '../tab/tab.component';

@Component({
  selector: 'app-tabs',
  templateUrl: './tabs.component.html',
  styleUrls: ['./tabs.component.scss']
})
export class TabsComponent {

    @Output() tabChange: EventEmitter<number> = new EventEmitter<number>();

    @ContentChildren(TabComponent) tabs: QueryList<TabComponent>;

    selectTab(tab: TabComponent) {
      const tabs = this.tabs.toArray();

      // deactivate all tabs
      tabs.forEach(t => t.active = false);

      // activate the tab the user has clicked on.
      tab.active = true;

      this.tabChange.emit(tabs.indexOf(tab));
    }
  }

