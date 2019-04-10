import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs';

@Injectable()
export class NodeDetailsService {
  showModal$ = new BehaviorSubject<boolean>(false);

  showModal(show: boolean) {
    this.showModal$.next(show);
  }
}
