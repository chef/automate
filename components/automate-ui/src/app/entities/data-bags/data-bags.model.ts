export interface DataBag {
  server_id: string;
  org_id: string;
  name: string;
}

export interface DataBagItems {
  name: string;
  active?: boolean;
}

export interface DataBagsItemDetails {
  name: string;
  id: string;
  data: string;
}
