import { Component, OnDestroy, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { ActivatedRoute, Router } from '@angular/router';
import { FormArray, FormBuilder, FormGroup, FormControl, Validators } from '@angular/forms';
import {  Observable, Subject  } from 'rxjs';
import { startWith, switchMap, map, takeUntil, filter } from 'rxjs/operators';
import { environment as env } from 'environments/environment';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { NodeCredentialsSearchPayload, SearchCredentials } from 'app/entities/credentials/credential.actions';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { select, Store } from '@ngrx/store';
import { allCredentials, credStatus, credtotal } from 'app/entities/credentials/credential.selectors';
import { allCredentials as allCredentialsGetById } from 'app/entities/node-credentials/node-credential.selectors';
import { EntityStatus, pending } from 'app/entities/entities';
import { Credential } from 'app/entities/credentials/credential.model';
import { GetNodeCredential, ResetStore } from 'app/entities/node-credentials/node-credential.actions';

@Component({
  templateUrl: './nodes-edit.component.html',
  styleUrls: ['./nodes-edit.component.scss']
})
export class NodesEditComponent implements OnInit, OnDestroy {
  constructor(
    private fb: FormBuilder,
    private route: ActivatedRoute,
    private router: Router,
    private httpClient: HttpClient,
    private layoutFacade: LayoutFacadeService,
    private store: Store<NgrxStateAtom>
  ) {}

  node: any;
  private isDestroyed = new Subject<boolean>();
  public secretType: string;
  public secrets: Credential[] = [];
  public selectedSecrets$ = [];
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
  activeStep = 1;
  isLoading = false;
  accordionTitle = 'credentials to connect to your nodes';
  isScroll: any;
  searchData: string;
  pageNumber = 0;
  secretFilter: NodeCredentialsSearchPayload;

  form: FormGroup;
  backendControl: FormGroup;
  backendValue$: Observable<string>;
  // Array of secrets available for user to select
  // TODO make ngrx/store selection
  secrets$: Observable<any[]>;

  ngOnInit() {
    this.scrollLoadingValue = false;
    this.searchData = '';
    this.layoutFacade.showSidebar(Sidebar.Compliance);
    this.route.paramMap.pipe(
      switchMap(params => this.fetchNode(params.get('id'))))
      .subscribe(node => {
        this.node = node;
        this.form = this.createForm(node);

        // Some fields are read-only based on manager type
        switch (node.manager) {
          case ('aws-api'):
          case ('azure-api'): {
            this.form.disable();
            break;
          }
          case ('aws-ec2'):
          case ('azure-vm'): {
            this.form.get('name').disable();
            this.form.get('name').setValidators([]);
            this.form.get('target_config.host').disable();
            this.form.get('target_config.host').setValidators([]);
            break;
          }
        }

        // Swap fields based on selected "backend" value (ssh, winrm)
        this.backendControl = this.form.get('target_config').get('backend') as FormGroup;
        this.backendValue$ = this.backendControl
          .valueChanges
          .pipe(startWith(this.backendControl.value));
        this.backendValue$.subscribe(backend => {
          const step = this.form.get('target_config') as FormGroup;
          // step.get('secrets').setValue([]);
          switch (backend) {
            case 'ssh':
              step.addControl('sudo', new FormControl(false));
              step.removeControl('ssl');
              step.removeControl('self_signed');
              break;
            case 'winrm':
              step.removeControl('sudo');
              step.addControl('ssl', new FormControl(false));
              step.addControl('self_signed', new FormControl(false));
              break;
          }
        });
        this.store.pipe(
          takeUntil(this.isDestroyed),
          select(allCredentialsGetById)
        ).subscribe((secretList) => {
          this.selectedSecrets$ = secretList;
        });
        this.store.dispatch(new ResetStore());
        for (let i = 0; i < node['target_config']['secrets'].length; i++) {
          this.store.dispatch(new GetNodeCredential({
            id: node['target_config']['secrets'][i]
          }));
        }
        this.searchData = '';
        this.secrets = [];
        this.pageNumber = 1;
        this.secretType = node['target_config']['backend'];
        this.scrollLoadingValue = false;
        this.store.select(credtotal).pipe(
          takeUntil(this.isDestroyed)
        ).subscribe((total) => {
          this.total = total;
        });
        this.store.pipe(
          takeUntil(this.isDestroyed),
          select(allCredentials)
        ).subscribe((secretList) => {
          if (this.isScroll) {
            this.secrets = [...this.secrets, ...secretList];
          } else {
            this.secrets = secretList;
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
      });

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

  search(data: any) {
    this.searchData = data;
    this.pageNumber = 1;
    this.getCredList(false);
  }

  selected(data: any) {
    const secrets: string[] = [];
    data.forEach((listOfSecrets) => {
      secrets.push(listOfSecrets.id);
    });
    if (secrets.length === data.length) {
      this.form.controls['target_config']['controls']['secrets'].setValue(secrets);
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

  onTypeSelect(type: string) {
    this.secretType = type;
    this.pageNumber = 1;
    this.form.controls['target_config']['controls']['backend'].setValue(this.secretType);
    this.getCredList(false);
  }

  submit(node): void {
    Object.keys(node['target_config']).forEach(k => {
      const v = node['target_config'][k];
      if (v === '' || v === null || v === undefined) {
        delete node['target_config'][k];
      } else {
        // port num can't be a string
        if (k === 'port') {
          node['target_config'][k] = parseInt(v, 10);
        }
      }
    });

    node.tags = node.tags.filter(({key, value}) => key.length && value.length);

    this.updateNode(node)
      .subscribe(() => this.router.navigate(['/compliance', 'scan-jobs', 'nodes']));
  }

  private createForm(node): FormGroup {
    const target_config = this.fb.group({
      host: [node.target_config.host, Validators.required],
      backend: [node.target_config.backend, Validators.required],
      secrets: [node.target_config.secrets, Validators.minLength(1)],
      port: node.target_config.port,
      sudo: node.target_config.sudo,
      ssl: node.target_config.ssl,
      self_signed: node.target_config.self_signed
    });

    const tags = this.fb.array(node.tags.map(t => this.fb.group(t)));

    return this.fb.group({
      id: [node.id],
      name: [node.name, Validators.required],
      target_config,
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

  // TODO move to ngrx/effects
  fetchSecrets(): Observable<any[]> {
    return this.httpClient.post<any>(`${env.secrets_url}/search`, {}).pipe(
      map(({secrets}) => secrets));
  }

  // TODO move to ngrx/effects
  updateNode(node): Observable<any> {
    return this.httpClient.put(`${env.nodes_url}/id/${node.id}`, node);
  }

  // TODO move to ngrx/effects
  fetchNode(id): Observable<any> {
    return this.httpClient.get<any>(`${env.nodes_url}/id/${id}`);
  }
}
