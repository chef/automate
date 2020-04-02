import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { of as observableOf, Observable } from 'rxjs';
import { map, mergeMap } from 'rxjs/operators';
import { environment } from 'environments/environment';
import { compact, concat } from 'lodash';
import { Destination } from '../../pages/data-feed/destination';

export interface DestinationsResponse {
  destinations: Destination[];
}

export interface DestinationResponse {
  destination: Destination;
}

const DATA_FEED_URL = environment.data_feed_url;
const SECRETS_URL = environment.secrets_url;

interface KVData {
  key?: string;
  value?: string;
}

interface SecretId {
  id: string;
}

interface Secret {
  id?: string;
  name: string;
  type: string;
  data: Array<KVData>;
}

@Injectable()
export class DestinationRequests {

  constructor(private http: HttpClient) { }

  public getDestinations(): Observable<DestinationsResponse> {
    return this.http.post<DestinationsResponse>(encodeURI(
      this.joinToDatafeedUrl(['destinations'])), {});
  }

  public getDestination(id: string): Observable<Destination> {
    return this.http.get<DestinationResponse>(this.joinToDatafeedUrl(['destination', id]))
    .pipe(map((destinationsJson: DestinationResponse) =>
    Destination.fromResponse(destinationsJson)));
  }

  public createDestination(destinationData: Destination,
    targetUsername: string, targetPassword: string):
    Observable<DestinationResponse> {
    return this.createSecret(destinationData, targetUsername, targetPassword)
      .pipe(mergeMap((secretId: string) => {
        destinationData.targetSecretId = secretId;
        return this.http.post<DestinationResponse>(
          this.joinToDatafeedUrl(['destination']), destinationData.toRequest());
      }));
  }

  public updateDestination(destination: Destination): Observable<DestinationResponse> {
    return this.http.patch<DestinationResponse>(encodeURI(
      this.joinToDatafeedUrl(['destination', destination.id.toString()])), destination.toRequest());
  }

  public deleteDestination(id: number): Observable<DestinationResponse> {
    return this.http.delete<DestinationResponse>(encodeURI(
      this.joinToDatafeedUrl(['destination', id.toString()])));
  }

  private createSecret(destination: Destination, targetUsername: string,
    targetPassword: string): Observable<string> {
    if ( targetUsername.length > 0 || targetPassword.length > 0 ) {
      const secret = this.newSecret('', destination.name, targetUsername, targetPassword);

      return this.http.post<SecretId>(`${SECRETS_URL}`, secret)
        .pipe(map(secretId => secretId.id));
    } else {
      return observableOf('');
    }
  }

  private newSecret(id: string, name: string, username: string,
    password: string): Secret {
    return {
      id: id,
      name: name,
      type: 'data_feed',
      data: Array<KVData>(
        {key: 'username', value: username},
        {key: 'password', value: password})
    };
  }

  // Generate an notifier url from a list of words.
  // falsey values; false, null, 0, "", undefined, and NaN are ignored
  // example: ['foo', 'bar', null, 'baz'] == '${NOTIFIERURL}/foo/bar/baz'
  private joinToDatafeedUrl(words: string[]): string {
    return compact(concat([DATA_FEED_URL], words)).join('/');
  }
}
