import { Component, OnInit } from "@angular/core";
import { FormBuilder, FormGroup, Validators } from "@angular/forms";
import {
  LayoutFacadeService,
  Sidebar,
} from "app/entities/layout/layout.facade";
import { Store } from "@ngrx/store";
import { NgrxStateAtom } from "app/ngrx.reducers";
import { GetSsoConfig } from "app/entities/sso-config/sso-config.actions";
import { takeUntil } from "rxjs/operators";
import { combineLatest, Subject } from "rxjs";
import { EntityStatus } from "app/entities/entities";
import { isNil } from "lodash/fp";
import {
  getStatus,
  ssoConfig
} from "app/entities/sso-config/sso-config.selectors";
import { SsoConfig } from "app/entities/sso-config/sso-config.model";
import { Regex } from "app/helpers/auth/regex";

@Component({
  selector: "app-sso-config",
  templateUrl: "./sso-config.component.html",
  styleUrls: ["./sso-config.component.scss"]
})
export class SsoConfigComponent implements OnInit {
  private isDestroyed = new Subject<boolean>();
  public ssoConfig: SsoConfig;
  public ssoConfigForm: FormGroup;
  public loading: boolean = false;

  constructor(
    private layoutFacade: LayoutFacadeService,
    fb: FormBuilder,
    private store: Store<NgrxStateAtom>
  ) {
    this.ssoConfigForm = fb.group({
      serviceProvider: ["azureAd"],
      ssoUrl: [
        "",
        [Validators.required, Validators.pattern(Regex.patterns.VALID_FQDN)],
      ],
      emailAttribute: ["", [Validators.required, Validators.minLength(5)]],
      usernameAttribute: ["", [Validators.required, Validators.minLength(5)]],
      groupAttribute: [],
      allowedGroups: [],
      entityIssuer: [
        "",
        [Validators.required, Validators.pattern(Regex.patterns.VALID_FQDN)],
      ],
      caInfo: ["", [Validators.required]],
      nameIdPolicyFormat: [],
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.getSsoConfig();
  }

  // get sso config
  private getSsoConfig(): void {
    this.store.dispatch(new GetSsoConfig());
    combineLatest([this.store.select(getStatus), this.store.select(ssoConfig)])
      .pipe(takeUntil(this.isDestroyed))
      .subscribe(([getSsoConfigStatus, ssoConfigState]) => {
        if (
          getSsoConfigStatus === EntityStatus.loadingSuccess &&
          !isNil(ssoConfigState)
        ) {
          this.ssoConfig = ssoConfigState;
          console.log(this.ssoConfig);
          this.ssoConfigForm.patchValue({
            ssoUrl: this.ssoConfig.sso_url,
            emailAttribute: this.ssoConfig.email_attr,
            usernameAttribute: this.ssoConfig.username_attr,
            groupAttribute: this.ssoConfig.groups_attr,
            entityIssuer: this.ssoConfig.entity_issuer,
            caInfo: this.ssoConfig.ca_contents,
            nameIdPolicyFormat: this.ssoConfig.name_id_policy_format
          });
        }
      });
  }

  removeSsoConfig() {
    console.log("remove sso config");
    this.loading = true;
    setTimeout(() => {
      this.loading = false;
    }, 5000);
  }

  saveSsoConfig() {
    console.log("save sso config");
    // console.log(this.ssoConfigForm);
    console.log(this.ssoConfigForm.value);
  }

  hasRequiredError(field: string): boolean {
    return (
      this.ssoConfigForm.get(field).hasError("required") &&
      this.ssoConfigForm.get(field).dirty
    );
  }

  hasAttemptedInput(field: string): boolean {
    return (
      this.ssoConfigForm.get(field).touched &&
      this.ssoConfigForm.get(field).dirty
    );
  }
}
