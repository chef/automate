import { Component, HostBinding, EventEmitter, Input, OnInit, OnDestroy } from '@angular/core';
import { FormGroup,  FormBuilder, Validators } from '@angular/forms';
import { Regex } from 'app/helpers/auth/regex';
import { Utilities } from 'app/helpers/utilities/utilities';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Store } from '@ngrx/store';
import { UpdateWebUIKey } from 'app/entities/servers/server.actions';
import { saveError, updateWebUIKey } from 'app/entities/servers/server.selectors';
import { Subject, combineLatest } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { isNil } from 'lodash/fp';

@Component({
  selector: 'app-update-web-uikey-slider',
  templateUrl: './update-web-uikey-slider.component.html',
  styleUrls: ['./update-web-uikey-slider.component.scss']
})
export class UpdateWebUIKeySliderComponent implements OnInit, OnDestroy {

  @Input() openEvent: EventEmitter<boolean>;
  @HostBinding('class.active') isSlideOpen = false;
  @Input() serverId: string;

  public server: string;
  public conflictError = false;
  public updated = false;
  public updateKeyForm: FormGroup;
  public updating = false;
  public checkedValidator = false;
  public ui_key: any;
  public updatedKey: string;
  public privateKey: string;
  public error: string;
  private isDestroyed = new Subject<boolean>();
  public conflictErrorEvent = new EventEmitter<boolean>();

  constructor(
    private formBuilder: FormBuilder,
    private store: Store<NgrxStateAtom>
    ) {
    this.updateKeyForm = this.formBuilder.group({
      webUiKey: ['', [Validators.required,
        Validators.pattern(Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN)]]
    });
  }

  ngOnInit() {
    this.openEvent.pipe(takeUntil(this.isDestroyed))
    .subscribe(() => {
    this.updated = false;
    this.conflictError = false;
    this.server = this.serverId;
    this.error = '';
    this.privateKey = '';
  });

  combineLatest([
    this.store.select(saveError),
    this.store.select(updateWebUIKey)
  ]).pipe(
    takeUntil(this.isDestroyed))
    .subscribe(([ errorSt, webKeyState]) => {
      if ( !isNil(errorSt) ) {
        this.updated = false;
        this.updating = false;
        this.error = errorSt?.message;
      } else {
        this.updating = false;
        this.updated = true;
        this.updatedKey = webKeyState;
      }
    });
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  public handleInput(event: KeyboardEvent): void {
    if (Utilities.isNavigationKey(event)) {
      return;
    }
    this.conflictError = false;
  }

  onSubmit() {
    this.updating = true;
    const webuikey = {
      key: this.updateKeyForm.controls['webUiKey'].value,
      server_id: this.serverId
    };
    console.log('Your form data : ', this.updateKeyForm.value, webuikey );
    this.store.dispatch(new UpdateWebUIKey(webuikey));
    this.updateKeyForm.reset();
    this.toggleSlide();
}

  closeUpdateWebUIKeySlider() {
    this.toggleSlide();
  }

  toggleSlide() {
    this.isSlideOpen = !this.isSlideOpen;
  }

  slidePanel() {
    this.isSlideOpen = true;
  }

  closeUpdateModal(): void {
    this.resetUpdateModal();
    this.toggleSlide();
  }

  private resetUpdateModal(): void {
    this.updating = false;
    this.updated = false;
    this.updateKeyForm.reset();
    this.checkedValidator = false;
    this.conflictErrorEvent.emit(false);
  }
}
