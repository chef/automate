import { Component } from '@angular/core';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from '../../ngrx.reducers';

import { LayoutFacadeService } from '../../entities/layout/layout.facade';
import { notificationState } from '../../entities/notifications/notification.selectors';
import { Notification, Type } from '../../entities/notifications/notification.model';
import { DeleteNotification } from '../../entities/notifications/notification.actions';

@Component({
  selector: 'app-chef-notifications',
  templateUrl: './notifications.component.html',
  styleUrls: ['./notifications.component.scss']
})
export class ChefNotificationsComponent {
  notifications: Notification[];

  constructor(
    private store: Store<NgrxStateAtom>,
    public layoutFacade: LayoutFacadeService
  ) {
    store.select(notificationState).subscribe(
      (notifications: Notification[]) => {
        this.notifications = notifications;
        this.layoutFacade.layout.header.license =
          notifications &&  notifications.some(n => n.type === Type.license);
        this.layoutFacade.updateDisplay();
      });
  }

  handleNotificationDismissal(id: string): void {
    this.store.dispatch(new DeleteNotification({ id }));
  }
}
