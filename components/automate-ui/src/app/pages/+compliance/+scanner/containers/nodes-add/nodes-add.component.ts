import { Observable, BehaviorSubject, Subject } from 'rxjs';
import { filter, map, startWith, takeUntil } from 'rxjs/operators';
import { Component, OnDestroy, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Router } from '@angular/router';
import { FormArray, FormBuilder, FormGroup, FormControl, Validators } from '@angular/forms';
import { environment as env } from '../../../../../../environments/environment';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { select, Store } from '@ngrx/store';
import { NodeCredentialsSearchPayload, SearchCredentials } from 'app/entities/credentials/credential.actions';
import { allCredentials, credStatus, credtotal } from 'app/entities/credentials/credential.selectors';
import { Credential } from 'app/entities/credentials/credential.model';
import { pending, EntityStatus } from 'app/entities/entities';

@Component({
  templateUrl: './nodes-add.component.html',
  styleUrls: ['./nodes-add.component.scss']
})
export class NodesAddComponent implements OnInit, OnDestroy {
  isScroll: any;

  constructor(
    private fb: FormBuilder,
    private router: Router,
    private httpClient: HttpClient,
    private layoutFacade: LayoutFacadeService,
    private store: Store<NgrxStateAtom>
  ) {}

  private isDestroyed = new Subject<boolean>();
  public secretType: string;
  public secrets: Credential[] = [];
  public sortBy: string;
  public total: number;
  public scrollLoadingValue: boolean;
  public instanceNodeCredentials$: Observable<Credential[]>;
  public dataCy = 'cred-accordion';
  public credentialType = [
     {name: 'SSH', asset: 'ssh'},
     {name: 'WinRM', asset: 'winrm'}
    ];

  public typeFieldName = 'type';
  public uniqueFiledName = 'id';
  form: FormGroup;
  activeStep = 1;
  isLoading = false;
  accordionTitle = 'Add credentials to connect to your nodes';

  addTypeControl: FormGroup;
  backendControl: FormGroup;
  backendValue$: Observable<string>;
  sslControl: FormGroup;
  sslValue$: Observable<boolean>;

  // Array of nodes to add (nodesToAdd == POST request body)
  nodesToAdd$: BehaviorSubject<any[]> = new BehaviorSubject([]);
  searchData: string;
  pageNumber = 0;
  secretFilter: NodeCredentialsSearchPayload;
  ngOnInit() {
    this.scrollLoadingValue = false;
    this.searchData = '';
    this.layoutFacade.showSidebar(Sidebar.Compliance);
    this.form = this.createForm();

    // Populate nodesToAdd$ with serialized form body
    this.form.valueChanges.pipe(
      map(() => this.form.getRawValue()),
      map(this.serializeForm))
      .subscribe(this.nodesToAdd$);

    // Swap fields based on selected "backend" value (ssh, winrm)
    this.backendControl = this.form.get('wizardStep2').get('backend') as FormGroup;
    this.backendValue$ =
    this.backendControl.valueChanges.pipe(startWith(this.backendControl.value));
    this.backendValue$.subscribe(backend => {
      const step = this.form.get('wizardStep2') as FormGroup;
      step.get('secrets').setValue([]);
      switch (backend) {
        case 'ssh':
          step.addControl('sudo', new FormControl(false));
          step.removeControl('ssl');
          step.removeControl('self_signed');
          this.form.get('wizardStep2').get('port').patchValue(22);
          break;
        case 'winrm':
          step.removeControl('sudo');
          step.addControl('ssl', new FormControl(false));
          step.addControl('self_signed', new FormControl(false));
          this.setWinRmPort();
          break;
      }
    });
    this.searchData = '';
    this.secrets = [];
    this.pageNumber = 1;
    this.secretType = 'ssh';
    let secretCred$;
    this.scrollLoadingValue = false;
    secretCred$ = this.store.pipe(
      takeUntil(this.isDestroyed),
      select(allCredentials)
    );
    this.store.select(credtotal).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe((total) => {
      this.total = total;
    });
    secretCred$.subscribe((secret) => {
      if (this.isScroll) {
        this.secrets = [...this.secrets, ...secret];
      } else {
        this.secrets = secret;
      }
    });
    this.store.pipe(
      select(credStatus),
      takeUntil(this.isDestroyed),
      filter(status => !pending(status)))
      .subscribe(response => {
        if (response === EntityStatus.loadingSuccess || EntityStatus.loadingFailure) {
            this.scrollLoadingValue = false;
        }
      });

    this.getCredList(false);
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  getCredList(isScroll: boolean): void {
    this.secretFilter = {
      filters:  [
        {
            key: 'name',
            values: [this.searchData]
        },
        {
            key: 'type',
            values: [this.secretType ]
        }
    ],
    page: this.pageNumber,
    per_page: 100
    };

    this.store.dispatch(new SearchCredentials(this.secretFilter));
    this.isScroll = isScroll;
  }

  setWinRmPort(): void {
    this.sslControl = this.form.get('wizardStep2').get('ssl') as FormGroup;
    this.sslValue$ = this.sslControl.valueChanges.pipe(startWith(this.sslControl.value));
    this.sslValue$.subscribe(ssl => {
      if (ssl && this.backendControl.value === 'winrm') {
        this.form.get('wizardStep2').get('port').patchValue(5986);
      } else {
        this.form.get('wizardStep2').get('port').patchValue(5985);
      }
    });
  }

  search(data: any) {
    this.searchData = data;
    this.pageNumber = 1;
    this.getCredList(false);
  }

  selected(data: any) {
    const secretIds: string[] = [];
    data.forEach((listOfSecrets) => {
      secretIds.push(listOfSecrets.id);
    });
    if (secretIds.length === data.length) {
      this.form.controls['wizardStep2']['controls']['secrets'].setValue(secretIds);
    }
  }

  scroll() {
    this.scrollLoadingValue = true;
    if (this.secrets.length === this.total) {
      this.scrollLoadingValue = false;
    }
    if (this.secrets.length < this.total) {
      this.pageNumber++;
      this.getCredList(true);
    }
  }

  private createForm(): FormGroup {
    const wizardStep1 = this.fb.group({
      hosts: ['', Validators.required],
      customPrefix: ''
    });

    const wizardStep2 = this.fb.group({
      backend: ['ssh', Validators.required],
      secrets: [[], Validators.minLength(1)],
      port: '',
      sudo: false,
      ssl: false,
      self_signed: false
    });

    const tags = this.fb.array([]);

    return this.fb.group({
      wizardStep1,
      wizardStep2,
      tags
    });
  }

  addTag(form: FormGroup, index: number) {
    const tags = form.get('tags') as FormArray;
    tags.insert(index, this.fb.group({ key: '', value: '' }));
  }

  removeTag(form: FormGroup, index: number) {
    const tags = form.get('tags') as FormArray;
    tags.removeAt(index);
  }

  navToStep(num: number): void {
    this.activeStep = num;
  }

  stepIsActive(num: number): boolean {
    return this.activeStep === num;
  }

  stepIsValid(num: number): boolean {
    return this.form.get(`wizardStep${num}`).valid;
  }

  isFormValid(): boolean {
    return this.form.valid;
  }

  submit(_form): void {
    this.isLoading = true;

    this.createNodes(this.nodesToAdd$.getValue())
      .subscribe(() => this.router.navigate(['/compliance', 'scan-jobs', 'nodes']));
  }

  serializeForm(data) {
    const {hosts, customPrefix} = data.wizardStep1;
    const targetConfig = data.wizardStep2;
    const tags = data.tags.filter(({key, value}) => key.length && value.length);

    Object.keys(targetConfig).forEach(k => {
      const v = targetConfig[k];
      if (v === '' || v === null || v === undefined) {
        delete targetConfig[k];
      }
    });

    // https://stackoverflow.com/a/106223/319074
    const ipv4Regex = new RegExp([
      /^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}/,
      /([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$/
    ].map(r => r.source).join(''));

    const hostnameRegex = new RegExp([
      /^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])/,
      /(\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9]))*$/
    ].map(r => r.source).join(''));

    return hosts.split(',')
      .map(host => host.trim())
      .filter(host => ipv4Regex.test(host) || hostnameRegex.test(host))
      .map(host => {
        const name = customPrefix.length ? `${customPrefix}-${host}` : host;
        return {
          name,
          manager: 'automate',
          target_config: Object.assign({}, targetConfig, {hosts: [host]}),
          tags
        };
      });
  }



  onTypeSelect(type: string) {
    this.secretType = type;
    this.pageNumber = 1;
    this.form.controls['wizardStep2']['controls']['backend'].setValue(this.secretType);
    this.getCredList(false);
  }

  // TODO move to ngrx/effects
  createNodes(nodes): Observable<any> {
    return this.httpClient.post(`${env.nodes_url}/bulk-create`, {nodes});
  }
}
