export interface DestinationInterface {
  id?: number;
  name: string; // name of the destination
  url?: string; // URL of the endpoint
  secret_id?: string; // Slack or Custom
}

export class Destination implements DestinationInterface {

  constructor(
    public id: number,
    public name: string,
    public url: string,
    public secret_id: string
  ) {
  }

  public static fromResponse(destination: Object): Destination {
    return new Destination(destination['id'], destination['name'],
    destination['url'], destination['secret']);
  }

  public toRequest(): Object {
    return {
        'id': this.id,
        'name': this.name,
        'url': this.url,
        'secret': this.secret_id
    };
  }
}
