import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { of as observableOf, Observable } from 'rxjs';
import { map, mergeMap } from 'rxjs/operators';
import { environment } from 'environments/environment';
import { compact, concat } from 'lodash';
import { Destination } from './destination.model';
import { CreateDestinationPayload } from './destination.actions';

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
      this.joinToDataFeedUrl(['destinations'])), {});
  }

  public getDestination(id: string): Observable<Destination> {
    return this.http.get<Destination>(this.joinToDataFeedUrl(['destination', id]))
    .pipe(map((destinationsJson: Destination) => destinationsJson));
  }

  public createDestination(destinationData: CreateDestinationPayload,
    headers: string, storage: { accessKey: string; secretKey: string }):
    Observable<DestinationResponse> {
    if (headers != null) {
      return this.createSecretHeaders(destinationData, headers)
        .pipe(mergeMap((secretId: string) => {
          destinationData.secret = secretId;
          return this.http.post<DestinationResponse>(
            this.joinToDataFeedUrl(['destination']), destinationData);
        }));
    }
    if (storage != null) {
      return this.createSecretStorage(destinationData, storage)
          .pipe(mergeMap((secretId: string) => {
            destinationData.secret = secretId;
            return this.http.post<DestinationResponse>(
              this.joinToDataFeedUrl(['destination']), destinationData);
          }));
    }
    return observableOf(<DestinationResponse>{});
  }

  public updateDestination(destination: Destination): Observable<DestinationResponse> {
    return this.http.patch<DestinationResponse>(encodeURI(
      this.joinToDataFeedUrl(['destination', destination.id.toString()])), destination);
  }

  public deleteDestination(id: string): Observable<DestinationResponse> {
    return this.http.delete<DestinationResponse>(encodeURI(
      this.joinToDataFeedUrl(['destination', id.toString()])));
  }

  private createSecretHeaders(
    destination: CreateDestinationPayload,
    headers: string
  ): Observable<string> {
    if ( headers.length > 0 ) {
      const secret = this.newSecret('', destination.name, headers);

      return this.http.post<SecretId>(`${SECRETS_URL}`, secret)
        .pipe(map(secretId => secretId.id));
    } else {
      return observableOf('');
    }
  }

  private createSecretStorage(
    destination: CreateDestinationPayload,
    storage: { accessKey: string; secretKey: string }
  ): Observable<string> {
    if ( storage.accessKey && storage.secretKey ) {
      const secret = this.newStorageSecret('', destination.name, storage);

      return this.http.post<SecretId>(`${SECRETS_URL}`, secret)
        .pipe(map(secretId => secretId.id));
    } else {
      return observableOf('');
    }
  }

  private newSecret(id: string, name: string, headers: string): Secret {
    return {
      id: id,
      name: name,
      type: 'data_feed',
      data: Array<KVData>(
        {key: 'headers', value: headers},
        {key: 'auth_type', value: 'header_auth'})
    };
  }

  private newStorageSecret(
    id: string, name: string,
    storage: { accessKey: string; secretKey: string }
  ): Secret {
    return {
      id: id,
      name: name,
      type: 'data_feed',
      data: Array<KVData>(
        {key: 'access_key', value: storage.accessKey},
        {key: 'secret_access_key', value: storage.secretKey})
    };
  }

  public testDestination(destination: Destination): Observable<Object> {
    if (destination.secret) {
      if (destination.services === 'Minio') {
        return this.testDestinationWithSecretIdMinio(
          destination.url,
          destination.secret,
          destination.services,
          destination.integration_types,
          destination.enable,
          destination.meta_data
        );
      } else {
        return this.testDestinationWithSecretId(destination.url, destination.secret);
      }
    }
  }
  public testDestinationWithUsernamePassword(url: string,
    username: string, password: string): Observable<Object> {
        return this.http.post(encodeURI(
          this.joinToDataFeedUrl(['destinations', 'test'])),
          { url, 'username_password': {username, password} });
  }

  public testDestinationWithUsernamePasswordWithHeaders(url: string,
    username: string, password: string, headers: string): Observable<Object> {
      if(headers!=""){
        return this.http.post(encodeURI(
          this.joinToDataFeedUrl(['destinations', 'test'])),
          { url, headers, 'username_password': {username, password},});
      }
      else{
        return this.http.post(encodeURI(
          this.joinToDataFeedUrl(['destinations', 'test'])),
          { url, 'username_password': {username, password} });
    }
  }

  public testDestinationWithHeaders(url: string,
    value: string): Observable<Object> {
    return this.http.post(encodeURI(
      this.joinToDataFeedUrl(['destinations', 'test'])),
      { url, 'header': {value} });
  }

  public testDestinationForMinio(data: any): Observable<Object> {
    return this.http.post(encodeURI(
      this.joinToDataFeedUrl(['destinations', 'test'])),
      { ...data });
  }

  public testDestinationWithSecretId(url: string, secretId: string): Observable<Object> {
    return this.http.post(encodeURI(
      this.joinToDataFeedUrl(['destinations', 'test'])), { url, 'secret_id': { 'id': secretId } });
  }

  public testDestinationWithSecretIdMinio(
    url: string,
    secretId: string,
    services: string,
    integration_types: string,
    enable: boolean,
    meta_data: any
  ): Observable<Object> {
    return this.http.post(encodeURI(
      this.joinToDataFeedUrl(['destinations', 'test'])), {
        url,
        'secret_id_with_addon': {
          'id': secretId,
          services,
          integration_types,
          enable,
          meta_data
        }
      }
    );
  }

  public testDestinationWithNoCreds(url: string): Observable<Object> {
    return this.http.post(encodeURI(
      this.joinToDataFeedUrl(['destinations', 'test'])), { url, 'none': {}});
  }

  // Generate an notifier url from a list of words.
  // falsey values; false, null, 0, "", undefined, and NaN are ignored
  // example: ['foo', 'bar', null, 'baz'] == '${NOTIFIERURL}/foo/bar/baz'
  private joinToDataFeedUrl(words: string[]): string {
    return compact(concat([DATA_FEED_URL], words)).join('/');
  }
}
