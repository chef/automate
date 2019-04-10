import { Component } from '@angular/core';

@Component({
  selector: 'app-demo-sidebar',
  templateUrl: './demo-sidebar.component.html',
  styleUrls: ['./demo-sidebar.component.scss']
})
export class DemoSidebarComponent {
  selectActionClicked: string;

  doSomethingClever(item: string) {
    this.selectActionClicked = item;
  }
}
