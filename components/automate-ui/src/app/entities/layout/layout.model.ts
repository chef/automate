import { Observable } from 'rxjs';

// Sidebar {
//   active: "dashboards",
//   dashboards: []
// }

export interface Sidebars {
  active?: string;
  dashboards?: MenuItemGroup[];
  applications?: MenuItemGroup[];
  infrastructure?: MenuItemGroup[];
  compliance?: MenuItemGroup[];
  settings?: MenuItemGroup[];
  profile?: MenuItemGroup[];
}

// MenuItemGroup {
//   name: "Node Management",
//   items: [],
//   visible: Observable
// }

export interface MenuItemGroup {
  name: string;
  items: MenuItem[];
  visible?: any;
}

// MenuItem {
//   name: "Notifications",
//   icon: "notifications",
//   route: "/settings/data-feed",
//   authorized: {},
//   visible: Observable
// }

export interface MenuItem {
  name: string;
  icon: string;
  iconRotation?: number;
  route: string;
  authorized?: Authorized;
  visible?: Observable<any>;
  openInNewPage?: boolean;
}

// Authorized {
//   isAuthorized: false,
//   type: "notifications",
//   permissions: "['/notifications/rules', 'get']"
// }

export interface Authorized {
  isAuthorized?: boolean;
  allOf?: any[];
  anyOf?: any[];
}
