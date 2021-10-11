import { Observable, BehaviorSubject, Subject, combineLatest } from 'rxjs';
import { map, startWith, takeUntil } from 'rxjs/operators';
import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Router } from '@angular/router';
import { FormArray, FormBuilder, FormGroup, FormControl, Validators } from '@angular/forms';
import { environment as env } from '../../../../../../environments/environment';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { AvailableType } from 'app/modules/infra-proxy/infra-roles/infra-roles.component';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { select, Store } from '@ngrx/store';
import { NodeCredentialsSearch } from 'app/entities/node-credentials/node-credential.actions';
import { NodeCredentialOrder, SortParams } from 'app/pages/+compliance/+node-credentials/node-credentials-list/node-credential-list.reducer';
import { allCredentials, totalNodeCredential } from 'app/entities/node-credentials/node-credential.selectors';
import { NodeCredential } from 'app/entities/node-credentials/node-credential.model';
import { nodeCredentialListState } from 'app/pages/+compliance/+node-credentials/node-credentials-list/node-credential-list.selectors';
import { get, pick, toUpper } from 'lodash/fp';

@Component({
  templateUrl: './nodes-add.component.html',
  styleUrls: ['./nodes-add.component.scss']
})
export class NodesAddComponent implements OnInit {
  secretType: string;
  public params: SortParams;
  private isDestroyed = new Subject<boolean>();
  public secrets: NodeCredential[] = [];
  public sortBy: string;
  public orderBy: NodeCredentialOrder;
  public total: number;

  public instanceNodeCredentials$: Observable<NodeCredential[]>;
  constructor(
    private fb: FormBuilder,
    private router: Router,
    private httpClient: HttpClient,
    private layoutFacade: LayoutFacadeService,
    private store: Store<NgrxStateAtom>
  ) {}

  public availablelist: AvailableType[] = [];
  public credentialType = [
     {name: 'SSH', asset: 'ssh'},
     {name: 'WinRM', asset: 'winrm'}
    ];
  form: FormGroup;
  activeStep = 1;
  isLoading = false;
  accordionTitle = 'Add credentials to connect to your nodes';

  addTypeControl: FormGroup;
  backendControl: FormGroup;
  backendValue$: Observable<string>;
  sslControl: FormGroup;
  sslValue$: Observable<boolean>;

  // Array of secrets available for user to select
  // TODO make ngrx/store selection

  // Array of nodes to add (nodesToAdd == POST request body)
  nodesToAdd$: BehaviorSubject<any[]> = new BehaviorSubject([]);
  searchData: string;
  pageNumber = 0;

  ngOnInit() {
    this.searchData = '';
    // this.availablelist = [{name: 'a', type: 2}];
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

    this.instanceNodeCredentials$ = this.store.pipe(
      takeUntil(this.isDestroyed),
      select(allCredentials)
    );

    combineLatest([
      this.instanceNodeCredentials$,
      this.backendValue$
    ]).pipe(
      map(([secrets, backend]: [NodeCredential[], string]) =>
        secrets.filter(s => s.type === backend || s.type === 'sudo'))
    ).subscribe((results) => {
        this.secrets = results;
        console.log(results.length);
        console.log(this.secrets);
    });

    this.instanceNodeCredentials$ = this.store.pipe(select(allCredentials));

    this.store.select(nodeCredentialListState).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe((state) => {
      this.params = pick(['filters', 'page', 'per_page'], state);
      this.sortBy = get('sort', state);
      this.orderBy = get('order', state);
      if (this.orderBy !== 'none') {
        this.params = { ...this.params, order: toUpper(this.orderBy), sort: this.sortBy };
      }
    });

    this.store.select(totalNodeCredential).pipe(
      takeUntil(this.isDestroyed)
    ).subscribe((total) => {
      this.total = total;
    });
    this.getNodeList();
    console.log(this.instanceNodeCredentials$);

  }


  getNodeList(): void {
    this.store.dispatch(new NodeCredentialsSearch(this.params));
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
    console.log(data);
    this.params.filters = [
      {
          'key': 'name',
          'values': [data]
      }
    ];
    this.searchData = data;
    this.getNodeList();
  }

  selected(data: any) {
    const secrets: string[] = [];
    data.forEach((listOfSecrets) => {
      secrets.push(listOfSecrets.id);
    });
    if (secrets.length === data.length) {
      this.form.controls['wizardStep2']['controls']['secrets'].setValue(secrets);
    }
  }

  scroll() {
    this.params.page++;
    if (this.secrets.length < this.total) {
      console.log(this.secrets.length, this.total);
      this.params.page++;
      this.getNodeList();
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



  onSelectedTab(type: string) {
    this.secretType = type;
    this.form.controls['wizardStep2']['controls']['backend'].setValue(this.secretType);
  }

  // TODO move to ngrx/effects
  fetchSecrets(body: any): Observable<any[]> {
    return this.httpClient.post<any>(`${env.secrets_url}/search`, body).pipe(
      map(({secrets}) => secrets));
  }

  // TODO move to ngrx/effects
  createNodes(nodes): Observable<any> {
    return this.httpClient.post(`${env.nodes_url}/bulk-create`, {nodes});
  }
}
