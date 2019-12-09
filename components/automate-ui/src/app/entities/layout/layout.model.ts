// MenuItemGroup {
//   name: "Node Management",
//   items: [],
//   visible: true
// }

export interface MenuItemGroup {
  name: string;
  items: MenuItem[];
  visible: any; // boolean or observable
}

// MenuItem {
//   name: "Notifications",
//   icon: "notifications",
//   route: "/settings/data-feed",
//   authorized: {},
//   visible: true
// }

export interface MenuItem {
  name: string;
  icon: string;
  iconRotation?: number;
  route: string;
  authorized?: Authorized;
  visible: boolean;
  openInNewPage?: boolean;
}

// Authorized {
//   type: "notifications",
//   permissions: "['/notifications/rules', 'get']"
// }

export interface Authorized {
  name: string;
  allOf?: any[];
  anyOf?: any[];
}
