export interface DataBag {
  server_id: string;
  org_id: string;
  name: string;
}

export interface DataBagItems {
  name: string;
  databag_name?: string;
  active?: boolean;
}

export interface DataBagsItemDetails {
  server_id?: string;
  org_id?: string;
  data_bag_name?: string;
  id?: string;
  name: string;
  data: string;
  item_id?: string;
}

export interface DataBagItem {
  server_id: string;
  org_id: string;
  name: string;
  data: {};
}
