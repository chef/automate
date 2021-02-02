export interface DataBags {
  name: string;
  active?: boolean;
}

export interface DataBagsItemDetails {
  name: string;
  id: string;
  data: string;
}

export interface DataBagSearch {
  name: string;
  page: number;
  count: number;
  filter: string;
  sort: string;
}
