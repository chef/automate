import { Component, OnInit } from "@angular/core";
import { FormBuilder, FormGroup, Validators } from "@angular/forms";
import {
  LayoutFacadeService,
  Sidebar,
} from "app/entities/layout/layout.facade";
import { Store } from "@ngrx/store";
import { NgrxStateAtom } from "app/ngrx.reducers";
import { CreateSsoConfig, DeleteSsoConfig, GetSsoConfig } from "app/entities/sso-config/sso-config.actions";
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
  public ssoConfigLoading: boolean = false;

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
      groupAttribute: [""],
      allowedGroups: [""],
      entityIssuer: [
        "",
        [Validators.required, Validators.pattern(Regex.patterns.VALID_FQDN)],
      ],
      caInfo: ["", [Validators.required]],
      nameIdPolicyFormat: [""]
    });
  }

  ngOnInit() {
    this.layoutFacade.showSidebar(Sidebar.Settings);
    this.getSsoConfig();
  }

  // get sso config
  private getSsoConfig(): void {
    this.ssoConfigLoading = true;
    this.store.dispatch(new GetSsoConfig());
    combineLatest([this.store.select(getStatus), this.store.select(ssoConfig)])
      .pipe(takeUntil(this.isDestroyed))
      .subscribe(([getSsoConfigStatus, ssoConfigState]) => {
        if (
          getSsoConfigStatus === EntityStatus.loadingSuccess &&
          !isNil(ssoConfigState)
        ) {
          this.ssoConfig = ssoConfigState;
          this.populateForm(this.ssoConfig);
          this.ssoConfigLoading = false;
        }
      });
  }

  saveSsoConfig() {
    const ssoConfig: SsoConfig = {
      ca_contents: this.ssoConfigForm.value.caInfo,
      sso_url: this.ssoConfigForm.value.ssoUrl,
      email_attr: this.ssoConfigForm.value.emailAttribute,
      username_attr: this.ssoConfigForm.value.usernameAttribute,
      groups_attr: this.ssoConfigForm.value.groupAttribute,
      allowed_groups: this.convertToArray(this.ssoConfigForm.value.allowedGroups),
      entity_issuer: this.ssoConfigForm.value.entityIssuer,
      name_id_policy_format: this.ssoConfigForm.value.nameIdPolicyFormat
    };

    this.store.dispatch(new CreateSsoConfig(ssoConfig));
    // TODO: save sso config api status polling
    // this.store.dispatch(new CreateSsoConfigSuccess()); 
  }

  cancelSsoConfig() {
    this.getSsoConfig();
    this.ssoConfigForm.markAsPristine();
  }

  removeSsoConfig() {
    this.store.dispatch(new DeleteSsoConfig());
    // TODO: remove sso config api status polling
    // this.ssoConfigForm.reset({serviceProvider: 'azureAd'});
    // this.store.dispatch(new DeleteSsoConfigSuccess());
  }

  populateForm(ssoConfig) {
    this.ssoConfigForm.patchValue({
      serviceProvider: 'azureAd',
      ssoUrl: ssoConfig.sso_url,
      emailAttribute: ssoConfig.email_attr,
      usernameAttribute: ssoConfig.username_attr,
      groupAttribute: ssoConfig.groups_attr,
      allowedGroups: ssoConfig.allowed_groups.toString(),
      entityIssuer: ssoConfig.entity_issuer,
      caInfo: ssoConfig.ca_contents,
      nameIdPolicyFormat: ssoConfig.name_id_policy_format
    });
  }

  isSsoConfigAvailable() {
    return this.ssoConfig && this.ssoConfig.sso_url;
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

  convertToArray(value: string) {
    return value.split(",").map(item => item.trim()).filter(item => item !== "");
  }
}
