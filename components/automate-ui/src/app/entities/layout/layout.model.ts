import { Observable } from 'rxjs';

// Sidebars {
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
//   hasVisibleMenuItems: true,
//   visible$: Observable<boolean>
// }

export interface MenuItemGroup {
  name: string;
  items: MenuItem[];
  hasVisibleMenuItems?: boolean;
  visible$?: Observable<boolean>;
}

// MenuItem {
//   name: "Notifications",
//   icon: "notifications",
//   route: "/settings/data-feed",
//   authorized: {},
//   visible$: Observable<boolean>
// }

export interface MenuItem {
  name: string;
  icon?: string;
  customIcon?: string;
  iconRotation?: number;
  route: string;
  authorized?: Authorized;
  visible$?: Observable<boolean>;
  openInNewPage?: boolean;
}

// Authorized {
//   isAuthorized: false,
//   allOf: "['/api/v0/notifications/rules', 'get']"
//   anyOf: "['/api/v0/notifications/rules', 'get']"
// }

export interface Authorized {
  isAuthorized?: boolean;
  allOf?: any[];
  anyOf?: any[];
}
