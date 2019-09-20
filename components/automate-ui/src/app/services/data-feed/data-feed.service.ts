import { of as observableOf, Observable, throwError } from 'rxjs';

import { map, mergeMap, filter, catchError } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { compact, concat } from 'lodash';

import { environment } from 'environments/environment';
import { Destination } from '../../pages/data-feed/destination';

const DATA_FEED_URL = environment.data_feed_url;
const SECRETS_URL = environment.secrets_url;

export interface DestinationResponse {
  destination: object;
}

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
export class DatafeedService {
  constructor(private httpClient: HttpClient) { }

  public fetchDestinations(): Observable<Destination[]> {
    return this.httpClient.post(encodeURI(this.joinToDatafeedUrl(['destinations'])), {}).pipe(
      map((res: Object) => res['destinations']),
      filter(destinationsJson => destinationsJson),
      map(destinationsJson =>
        destinationsJson.map(destination => Destination.fromResponse(destination))
      ),
      catchError(this.handleError)
    );
  }

  handleError(error) {
    let msg = '';
    if (error.error instanceof ErrorEvent) {
      msg = `Error: ${error.error.message}`;
    } else {
      msg = `Error code: ${error.status}\nMessage: ${error.message}`;
    }
    return throwError(msg);
  }

  public fetchDestination(id: string): Observable<Destination> {
      return this.httpClient.get<DestinationResponse>(this.joinToDatafeedUrl(['destination', id]))
    .pipe(
      map((destinationsJson: DestinationResponse) => Destination.fromResponse(destinationsJson)),
      catchError(this.handleError)
    );
  }

  public createDestination(destination: Destination, targetUsername: string,
    targetPassword: string): Observable<DestinationResponse> {
    return this.createSecret(destination, targetUsername, targetPassword)
      .pipe(mergeMap((secretId: string) => {
        destination.targetSecretId = secretId;
        return this.httpClient.post<DestinationResponse>(
          this.joinToDatafeedUrl(['destination']), destination.toRequest());
      }));
  }

  public deleteDestination(destination: Destination): Observable<DestinationResponse> {
    return this.httpClient.delete<DestinationResponse>(encodeURI(
      this.joinToDatafeedUrl(['destination', destination.id.toString()])));
  }

  public editDestination(destination: Destination): Observable<DestinationResponse> {
    const response: any = destination.toRequest();
    return this.httpClient.patch<DestinationResponse>(encodeURI(
      this.joinToDatafeedUrl(['destination', destination.id.toString()])), response);
  }


  private createSecret(destination: Destination, targetUsername: string,
    targetPassword: string): Observable<string> {
    if ( targetUsername.length > 0 || targetPassword.length > 0 ) {
      const secret = this.newSecret('', destination.name, targetUsername, targetPassword);

      return this.httpClient.post<SecretId>(`${SECRETS_URL}`, secret)
        .pipe(map(secretId => secretId.id));
    } else {
      return observableOf('');
    }
  }

  private newSecret(id: string, name: string, targetUsername: string,
    targetPassword: string): Secret {
    return {
      id: id,
      name: name,
      type: 'data_feed',
      data: Array<KVData>(
        {key: 'username', value: targetUsername},
        {key: 'password', value: targetPassword})
    };
  }

  public testDestinationWithUsernamePassword(url: string,
    username: string, password: string): Observable<Object> {
    return this.httpClient.post(encodeURI(
      this.joinToDatafeedUrl(['destinations', 'test'])),
      { url, 'username_password': {username, password} });
  }

  public testDestinationWithSecretId(url: string, secretId: string): Observable<Object> {
    return this.httpClient.post(encodeURI(
      this.joinToDatafeedUrl(['destinations', 'test'])), { url, 'secret_id': { 'id': secretId } });
  }

  public testDestinationWithNoCreds(url: string): Observable<Object> {
    return this.httpClient.post(encodeURI(
      this.joinToDatafeedUrl(['destinations', 'test'])), { url, 'none': {}});
  }

  // Generate an notifier url from a list of words.
  // falsey values; false, null, 0, "", undefined, and NaN are ignored
  // example: ['foo', 'bar', null, 'baz'] == '${NOTIFIERURL}/foo/bar/baz'
  private joinToDatafeedUrl(words: string[]): string {
    return compact(concat([DATA_FEED_URL], words)).join('/');
  }
}
