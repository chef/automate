import { Component, Input } from '@angular/core';

@Component({
  selector: 'chef-sidebar-entry',
  templateUrl: './sidebar-entry.component.html',
  styleUrls: ['./sidebar-entry.component.scss']
})
export class SidebarEntryComponent {
  @Input() route: string;
  @Input() exact: string;
  // (optional) 0-365 number to rotate icon via CSS.
  @Input() iconRotation: number;
  // (optional) if true, opens the link in a new page
  @Input() openInNewPage: boolean;

  // (optional) both icon and custom icon are optional.
  // icon is use to show material icon provided in service
  // customIcon is use to show customized icon.
  // In custom icon we have to pass class name as a value to property from layout-sidebar service.
  @Input() icon: string;
  @Input() customIcon: string;

  isExactPresent() {
    // If the exact attribute is present, its value is the empty string.
    // If it is absent, it is undefined.
    return this.exact === '';
  }

  isExternal() {
    return this.route && (this.route.startsWith('http://') || this.route.startsWith('https://'));
  }
}
