import { Component } from '@angular/core';
import { Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { withLatestFrom } from 'rxjs/operators';

import { HttpStatus } from 'app/types/types';
import { EntityStatus } from 'app/entities/entities';
import { LicenseFacadeService } from 'app/entities/license/license.facade';
import { FetchStatus } from 'app/entities/license/license.model';
import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { notificationState } from 'app/entities/notifications/notification.selectors';
import { Notification, Type } from 'app/entities/notifications/notification.model';
import { DeleteNotification } from 'app/entities/notifications/notification.actions';

@Component({
  selector: 'app-chef-notifications',
  templateUrl: './notifications.component.html',
  styleUrls: ['./notifications.component.scss']
})
export class ChefNotificationsComponent {
  notifications: Notification[];

  constructor(
    private store: Store<NgrxStateAtom>,
    public licenseFacade: LicenseFacadeService,
    public layoutFacade: LayoutFacadeService
  ) {
    store.select(notificationState)
      .pipe(
        withLatestFrom(this.licenseFacade.fetchLicense$)
      )
      .subscribe(
        ([notifications, license]: [Notification[], FetchStatus]) => {
          this.notifications = notifications
            .filter(n =>
              n.type !== Type.error
              || license.status !== EntityStatus.loadingFailure
              || license.errorResp.status !== HttpStatus.NOT_FOUND);
          this.layoutFacade.layout.header.license =
            notifications && notifications.some(n => n.type === Type.license);
          this.layoutFacade.updateDisplay();
        });
  }

  handleNotificationDismissal(id: string): void {
    this.store.dispatch(new DeleteNotification({ id }));
  }

}
