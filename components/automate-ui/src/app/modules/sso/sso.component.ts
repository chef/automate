import { Component, OnInit } from '@angular/core';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { ConfigService } from 'app/services/config/config.service';
import { select, Store } from '@ngrx/store';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { GetAllSsoConfig } from 'app/entities/sso/sso.actions';
import { saveStatus } from 'app/entities/sso/sso.selectors';
import { filter, takeUntil } from 'rxjs/operators';
import { Subject } from 'rxjs';
import { EntityStatus , pending} from 'app/entities/entities';

@Component({
  selector: 'app-sso',
  templateUrl: './sso.component.html'
})
export class SsoComponent implements OnInit {

  isNotificationVisible = false;
  private isDestroyed = new Subject<boolean>();
  public createModalVisible = false;
  public createconfig = false
  constructor(
    private layoutFacade: LayoutFacadeService,
    private configService: ConfigService,
    private store: Store<NgrxStateAtom>
  ) { }

  ngOnInit(): void {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.isNotificationVisible = false;
    this.getConfigDetails();
    this.store.dispatch(new GetAllSsoConfig());

    this.store.pipe(
      select(saveStatus),
      takeUntil(this.isDestroyed),
      filter(state => this.createModalVisible && !pending(state)))
        .subscribe(state => {
          this.createconfig = false;
          if(state === EntityStatus.loadingSuccess) {
            console.log("Api was called!")
          }
        })
  }

  getConfigDetails() {
    this.configService.getConfig().subscribe(
      config => {
        if (config.deploymentType !== 'SAAS') {
          this.isNotificationVisible = true;
        }
      }
    );
  }

  hideNotification() {
    this.isNotificationVisible = false;
  }
}
