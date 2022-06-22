import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from 'environments/environment';
import { compact, concat } from 'lodash';
import { GlobalConfig } from './destination-config.model';
import { GlobalDataFeedConfigSuccess } from './destination-config.actions';

export interface GlobalDataFeedConfigResponse {
  config: GlobalConfig;
}

const DATA_FEED_URL = environment.data_feed_url;


@Injectable()
export class DataFeedGlobalConfigRequests {

  constructor(private http: HttpClient) { }

  public globalDataFeedConfig(): Observable<GlobalDataFeedConfigSuccess> {
    const a = this.http.get<GlobalDataFeedConfigSuccess>(encodeURI(
      this.joinToDataFeedUrl(['config'])));
    return a;
  }

  // Generate an notifier url from a list of words.
  // falsey values; false, null, 0, "", undefined, and NaN are ignored
  // example: ['foo', 'bar', null, 'baz'] == '${NOTIFIERURL}/foo/bar/baz'
  private joinToDataFeedUrl(words: string[]): string {
    return compact(concat([DATA_FEED_URL], words)).join('/');
  }
}
