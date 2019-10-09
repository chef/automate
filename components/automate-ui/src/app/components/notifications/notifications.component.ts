import { Component } from '@angular/core';
import { Observable } from 'rxjs';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';

import { notificationState } from 'app/entities/notifications/notification.selectors';
import { Notification } from 'app/entities/notifications/notification.model';
import { DeleteNotification } from 'app/entities/notifications/notification.actions';

@Component({
  selector: 'chef-notifications',
  templateUrl: './notifications.component.html',
  styleUrls: ['./notifications.component.scss']
})
export class ChefNotificationsComponent {
  notifications$: Observable<Notification[]>;

  constructor(
    private store: Store<NgrxStateAtom>
  ) {
    this.notifications$ = store.select(notificationState);
  }

  handleNotificationDismissal(id: string): void {
    this.store.dispatch(new DeleteNotification({ id }));
  }

}
