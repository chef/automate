import { Component, OnInit } from '@angular/core';
import { Store, select } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import {
  GlobalDataFeedConfig
} from 'app/entities/global-config/destination-config.actions';
import {
  globalDataFeed
} from 'app/entities/global-config/destination-config.selectors';
import { Observable } from 'rxjs';
import { GlobalConfig } from 'app/entities/global-config/destination-config.model';
import { map } from 'rxjs/operators';
import { EntityStatus } from 'app/entities/entities';

enum UrlTestState {
  Inactive,
  Loading,
  Success,
  Failure
}

@Component({
  selector: 'app-data-feed-config-details',
  templateUrl: './data-feed-config-details.component.html',
  styleUrls: ['./data-feed-config-details.component.scss']
})
export class DataFeedConfigDetailsComponent implements OnInit {
  public creatingDataFeed: boolean;
  public hookStatus = UrlTestState.Inactive;
  public Fetchconfig$: Observable<GlobalConfig>;
  public config: GlobalConfig;
  public configNotFound = true;
  getFirstFiveDataStatusCodesShow: boolean;
  getStatusCodes: number[] = [];
  getCidrFilters: string[] = [];
  getFirstFiveDataCidrFiltersShow: boolean;
  public feedIntervaloutput: string[] = [];
  public finalFeedIntervaloutput: string;

  constructor(
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit(): void {
    this.store.dispatch(new GlobalDataFeedConfig({}));
      this.getGlobalDataFeedConfig();
  }

  public getGlobalDataFeedConfig() {
    this.store.pipe(
      select(globalDataFeed)).pipe(
        map((res: any) =>  res))
        .subscribe((res: any) => {
          if (res.globalConfigStatus ===  EntityStatus.loadingSuccess) {
            this.configNotFound = false;
            this.config = res.globalConfig;
            this.getStatusCodes = this.getData(this.config?.accepted_status_codes, true);
            this.getCidrFilters = this.getData(this.config?.cidr_filter, true);
            const statusCodesLen = this.config?.accepted_status_codes.length;
            const filterLen = this.config?.cidr_filter.length;
            this.getFirstFiveDataStatusCodesShow =  statusCodesLen === this.getStatusCodes.length;
            this.getFirstFiveDataCidrFiltersShow =  filterLen === this.getCidrFilters.length;
            this.finalFeedIntervaloutput = this.covertFeedInterval(this.config?.feed_interval);
          }
        });
  }

  covertFeedInterval(feedInterval: string): string {
    // let output: string[] = []
      const SplitFeedInterval = feedInterval.split('');
      this.findArrayIndexAndRaplaceName(SplitFeedInterval, 'h', ' Hour ,');
      this.findArrayIndexAndRaplaceName(SplitFeedInterval, 'm', ' Minute ,');
      this.findArrayIndexAndRaplaceName(SplitFeedInterval, 's', ' Seconds ,');
      const SplitIntoTime = SplitFeedInterval.join('');
      SplitIntoTime.split(',').forEach((v) => {
        this.pushDataInArray('Hour', v);
        this.pushDataInArray('Minute', v);
        this.pushDataInArray('Seconds', v);
      });
      return this.feedIntervaloutput.join('');
  }
  public pushDataInArray(findString: string, v: string): void {
    if (v.includes(findString)) {
      if (parseInt(v, 10) !== 0) {
        this.feedIntervaloutput.push(v);
      }
    }
  }
  public findArrayIndexAndRaplaceName(FeedInterval: string[], find: string, replaceString): void {
    if (FeedInterval.includes(find)) {
      FeedInterval.splice(FeedInterval.indexOf(find), 1, replaceString);
    }
  }

  getAlldata(flag: string): void {
    const statusCodesLen = this.config?.accepted_status_codes.length;
    const filterLen = this.config?.cidr_filter.length;
    if (flag === 'StatusCodes') {
      this.getFirstFiveDataStatusCodesShow = false;
      this.getStatusCodes = this.getData(this.config.accepted_status_codes, false);
      this.getFirstFiveDataStatusCodesShow = statusCodesLen === this.getStatusCodes.length;
    } else if (flag === 'DataCidrFilters') {
      this.getFirstFiveDataCidrFiltersShow = false;
      this.getCidrFilters = this.getData(this.config.cidr_filter, false);
      this.getFirstFiveDataCidrFiltersShow = filterLen === this.getCidrFilters.length;
    }
  }

  getData(arrValue?: any, getFirstFiveData?: boolean): any {
    if (arrValue != null) {
      if (getFirstFiveData) {
        if (arrValue.length >= 5) {
          return arrValue.slice(0, 5);
        } else {
          return arrValue.slice(0, arrValue.length);
        }
      } else {
        return arrValue;
      }
    }
    return [];
  }
}

