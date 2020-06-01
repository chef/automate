export interface DataBags {
  name: string;
}

export interface DataBagsItemDetails {
  name: string;
  id: string;
  data: string;
}

export class ItemAttributes {
  id: string;
  name: string;
  data: object;

  constructor(resp: DataBagsItemDetails) {
    this.id = resp.id;
    this.name = resp.name;
    this.data = (resp.data && JSON.parse(resp.data)) || {};
  }
}
