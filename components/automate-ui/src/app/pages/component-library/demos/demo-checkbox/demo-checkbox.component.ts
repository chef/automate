import { Component } from '@angular/core';

@Component({
  selector: 'app-demo-checkbox',
  templateUrl: './demo-checkbox.component.html',
  styleUrls: []
})
export class DemoCheckboxComponent {
  checkedValue = false;

  updateCheckedValue(event) {
    this.checkedValue = event;
  }
}
