// MenuItemGroup {
//   name: "Node Management",
//   items: [],
//   visible: true
// }

export interface MenuItemGroup {
  name: string;
  items: MenuItem[];
  visible: boolean;
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
  route: string;
  authorized?: Authorized;
  visible: boolean;
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
