export interface Destination {
  id?: string;
  name: string;
  url?: string;
  secret_id?: string;
}

// export interface DestinationInterface {
//   id: number;
//   name: string; // name of the destination
//   targetUrl?: string; // URL of the endpoint
//   targetSecretId?: string; // Slack or Custom
// }


// export class Destination implements DestinationInterface {

//   constructor(
//     public id: number,
//     public name: string,
//     public targetUrl: string,
//     public targetSecretId: string
//   ) {
//   }

//   public static fromResponse(destination: Object): Destination {
//     return new Destination(destination['id'], destination['name'],
//     destination['url'], destination['secret']);
//   }

//   public toRequest(): Object {
//     return {
//         'id': this.id,
//         'name': this.name,
//         'url': this.targetUrl,
//         'secret': this.targetSecretId
//     };
//   }
// }
