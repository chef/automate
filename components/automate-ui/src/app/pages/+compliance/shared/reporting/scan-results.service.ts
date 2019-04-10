import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable } from 'rxjs';

@Injectable()
export class ScanResultsService {

  title = 'Scan Results';
  opened = false;

  params: BehaviorSubject<any>;

  node: any = {};
  profile: any = {};
  control: any = {};

  showNodesList = false;
  nodesList: any = {
    items: []
  };

  showControlDetail = false;
  controlDetail: any = {
    item: {}
  };

  constructor() {
    this.params = new BehaviorSubject({
      nodeId: null,
      profileId: null,
      controlId: null
    });
  }

  getParams(): Observable<any> {
    return this.params.asObservable();
  }

  setParams(params: any): void {
    this.params.next(Object.assign({}, params));
  }

  setParam(key: any, val: any): void {
    const params = this.params.getValue();
    params[key] = val;
    this.setParams(params);
  }

  clearParams(): void {
    this.setParams({
      nodeId: null,
      profileId: null,
      controlId: null
    });
  }
}
