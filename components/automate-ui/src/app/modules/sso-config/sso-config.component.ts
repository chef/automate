import { Component, EventEmitter, OnInit } from "@angular/core";
import {
  LayoutFacadeService,
  Sidebar,
} from "app/entities/layout/layout.facade";
import {  Store } from "@ngrx/store";
import { NgrxStateAtom } from "app/ngrx.reducers";
import { GetAllSsoConfig } from "app/entities/sso-config/sso-config.actions";
import { filter, takeUntil } from "rxjs/operators";
import { combineLatest, Subject } from "rxjs";
import { EntityStatus } from "app/entities/entities";
import { isNil } from "lodash/fp";
import { getAllStatus as getAllConfigStatus, allSsoConfigs} from "app/entities/sso-config/sso-config.selectors";
import { SsoConfig } from "app/entities/sso-config/sso-config.model";

@Component({
  selector: "app-sso-config",
  templateUrl: "./sso-config.component.html",
})
export class SsoConfigComponent implements OnInit {
  isNotificationVisible = false;
  private isDestroyed = new Subject<boolean>();
  public createModalVisible = false;
  public configs: SsoConfig;
  public conflictErrorEvent = new EventEmitter<boolean>();
  constructor(
    private layoutFacade: LayoutFacadeService,
    private store: Store<NgrxStateAtom>
  ) {}

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.isNotificationVisible = false;

    // this.getConfigDetails();
    this.store.dispatch(new GetAllSsoConfig());

    combineLatest([
      this.store.select(getAllConfigStatus),
      this.store.select(allSsoConfigs),
    ])
      .pipe(
        filter(
          ([getConfigStatus, allConfigState]) =>
            getConfigStatus === EntityStatus.loadingSuccess &&
            !isNil(allConfigState)
        ),
        takeUntil(this.isDestroyed)
      )
      .subscribe(([_getConfigSt, allConfigState]) => {
        this.configs = allConfigState;
      });
  }

  
  hideNotification() {
    this.isNotificationVisible = false;
  }
}


