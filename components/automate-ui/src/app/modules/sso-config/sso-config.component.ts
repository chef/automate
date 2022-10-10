import { Component, OnInit } from "@angular/core";
import {
  LayoutFacadeService,
  Sidebar,
} from "app/entities/layout/layout.facade";
import {  Store } from "@ngrx/store";
import { NgrxStateAtom } from "app/ngrx.reducers";
import { GetSsoConfig } from "app/entities/sso-config/sso-config.actions";
import { takeUntil } from "rxjs/operators";
import { combineLatest, Subject } from "rxjs";
import { EntityStatus } from "app/entities/entities";
import { isNil } from "lodash/fp";
import { getStatus, ssoConfig} from "app/entities/sso-config/sso-config.selectors";
import { SsoConfig } from "app/entities/sso-config/sso-config.model";

@Component({
  selector: "app-sso-config",
  templateUrl: "./sso-config.component.html",
})
export class SsoConfigComponent implements OnInit {
  private isDestroyed = new Subject<boolean>();
  public ssoConfig: SsoConfig;

  constructor(
    private layoutFacade: LayoutFacadeService,
    private store: Store<NgrxStateAtom>
  ) {}

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.getSsoConfig();
  }

  // get sso config
  private getSsoConfig(): void {
    this.store.dispatch(new GetSsoConfig());
    combineLatest([
      this.store.select(getStatus),
      this.store.select(ssoConfig)
    ]).pipe(takeUntil(this.isDestroyed))
      .subscribe(([getSsoConfigStatus, ssoConfigState]) => {
        if (getSsoConfigStatus === EntityStatus.loadingSuccess && !isNil(ssoConfigState)) {
          this.ssoConfig = ssoConfigState;
        }
    });
  }  
}
