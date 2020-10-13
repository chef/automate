import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs';

@Injectable()
export class NodeDetailsService {
  showModal$ = new BehaviorSubject<LogModalObject>({isVisible: false});

  showModal(isVisible: boolean, resourceId?: string) {
    this.showModal$.next({isVisible, resourceId});
  }
}

export interface LogModalObject {
  isVisible: boolean;
  resourceId?: string;
}
