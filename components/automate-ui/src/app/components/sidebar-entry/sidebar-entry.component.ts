import { Component, Input } from '@angular/core';

@Component({
  selector: 'chef-sidebar-entry',
  templateUrl: './sidebar-entry.component.html',
  styleUrls: []
})
export class SidebarEntryComponent {
  @Input() icon: string;
  @Input() route: string;
  @Input() exact: string;
  // (optional) 0-365 number to rotate icon via CSS.
  @Input() iconRotation: number;

  isExactPresent() {
    // If the exact attribute is present, its value is the empty string.
    // If it is absent, it is undefined.
    return this.exact === '';
  }
}
