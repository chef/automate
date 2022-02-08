import { map } from 'rxjs/operators';
import { Component, ElementRef, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, FormArray } from '@angular/forms';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { getOr } from 'lodash/fp';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { ManagerSearchFields } from '../../entities/managers/manager.actions';
import * as selectors from '../../entities/managers/manager.selectors';
import { Manager } from 'app/entities/managers/manager.model';
@Component({
  selector: 'chef-job-nodes-form',
  templateUrl: './job-nodes-form.component.html',
  styleUrls: ['./job-nodes-form.component.scss']
})
export class JobNodesFormComponent implements OnInit {
  @Input() form: FormGroup;
  @Input() loadMore: any;
  @Input() nodeManagerLength: any;
  @Input() pageNo: any;
  @Output() firstCall = new EventEmitter<boolean>();
  @Output() load = new EventEmitter<{search: string[], nodearray: string[]}>();
  @Output() clickCalled = new EventEmitter<{search: string[], nodearray: string[], checked: any}>();
  @ViewChild('searchInput') searchval: ElementRef;
  @Input() checked;

  public managers$: Observable<Manager[]>;
  public automateCheck = false;
  public awsCheck = false;
  public azureCheck = false;
  public gcpCheck = false;
  public nodeSource = [];
  public throttle = 300;
  public scrollDistance = 2;
  public scrollingLoader = false;
  public total: number;
  public searchName: any;
  public NodeManagerArray: Manager[] = [];
  public pagenumber = 1;

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder
  ) {}


  ngOnInit() {
    this.awsCheck = this.checked?.aws;
    this.azureCheck = this.checked?.azure;
    this.gcpCheck = this.checked?.gcp;
    this.automateCheck = this.checked?.automate;
  }

  addRegionValue(regionsGroup: FormGroup, index: number) {
    const valuesArray = regionsGroup.controls['values'] as FormArray;
    valuesArray.insert(index, this.fb.control(''));
  }

  removeRegionValue(regionsGroup: FormGroup, index: number) {
    const valuesArray = regionsGroup.controls['values'] as FormArray;
    valuesArray.removeAt(index);
  }

  addTag(manager: FormGroup) {
    const tagsArray = manager.controls['tagsArray'] as FormArray;
    const tagGroup = this.fb.group({
      key: '',
      values: this.fb.array(['']),
      include: true
    });
    tagsArray.push(tagGroup);

    tagGroup.controls['key'].valueChanges.subscribe((key) => {
      const managerId = manager.value.id;
      const field = `tags:${key}`;
      this.store.dispatch(new ManagerSearchFields({ managerId, field }));
    });
  }

  removeTag(manager: FormGroup, index: number) {
    const tagsArray = manager.controls['tagsArray'] as FormArray;
    tagsArray.removeAt(index);
  }

  addTagValue(tag: FormGroup) {
    const valuesArray = tag.controls['values'] as FormArray;
    valuesArray.push(this.fb.control(''));
  }

  removeTagValue(tag: FormGroup, index: number) {
    const valuesArray = tag.controls['values'] as FormArray;
    valuesArray.removeAt(index);
  }

  fieldValuesFor(managerId: string, field: string): Observable<string[]> {
    return this.store
      .select(selectors.fieldsByManager)
      .pipe(
        map(getOr([], `${managerId}.fields.${field}`))
        ) as Observable<string[]>;
  }

  previewNodesFor(managerId: string): Observable<string[]> {
    return this.store
      .select(selectors.nodesByManager)
      .pipe(
        map(getOr([], `${managerId}.nodes`))
        ) as Observable<string[]>;
  }

  isLoadingPreviewNodesFor(managerId: string): Observable<boolean> {
    return this.store
      .select(selectors.nodesByManager)
      .pipe(
        map(getOr(false, `${managerId}.loading`))
        ) as Observable<boolean>;
  }

  previewNodesCountFor(managerId: string): Observable<number> {
    return this.previewNodesFor(managerId).pipe(map((nodes) => nodes.length));
  }

  availableNodesCountFor(managerId: string): Observable<number> {
    return this.store
      .select(selectors.nodesByManager)
      .pipe(
        map(getOr(0, `${managerId}.allTotal`))
        ) as Observable<number>;
  }

  logoFor(managerType: string) {
    const dir = 'assets/img';
    switch (managerType) {
      case ('automate'): {
        return `${dir}/logos/AutomateLogo-default.svg`;
      }
      case ('aws-ec2'):
      case ('aws-api'): {
        return `${dir}/logo-aws.svg`;
      }
      case ('azure-vm'):
      case ('azure-api'): {
        return `${dir}/logo-azure.svg`;
      }
      case ('gcp-api'): {
        return `${dir}/logo-gcp.svg`;
      }
    }
  }

  isAwsApi(managerType: string): boolean {
    return managerType === 'aws-api';
  }

  supportsFilterByName(managerType: string): boolean {
    return ['automate'].includes(managerType);
  }

  supportsFilterByRegion(managerType: string): boolean {
    return ['aws-ec2', 'azure-vm'].includes(managerType);
  }

  supportsFilterByTag(managerType: string): boolean {
    return ['automate', 'aws-ec2', 'azure-vm', 'azure-api'].includes(managerType);
  }

  search(searchName: string[]) {
    this.pageNo = 1 ;
    this.firstCall.emit(true);
    this.searchName = searchName;
    this.clickCalled.emit({search: searchName, nodearray: this.nodeSource, checked: {
      aws: this.awsCheck,
      azure: this.azureCheck,
      gcp: this.gcpCheck,
      automate: this.automateCheck
    }});
  }

  onclickCheckbox(e: any, val: string) {
    this.pageNo = 1;
    this.searchval.nativeElement.value = '';
    this.firstCall.emit(true);
    switch (val) {
      case 'automate': {
        this.automateCheck = e.target.checked;
        if (e.target.checked) {
          this.nodeSource.push('automate');
        } else {
          this.nodeSource.splice(this.nodeSource.indexOf('automate'), 1);
        }
        break;
      }
      case 'aws': {
        this.awsCheck = e.target.checked;
        if (e.target.checked) {
          this.nodeSource.push('aws-ec2');
          this.nodeSource.push('aws-api');
        } else {
          this.nodeSource.splice(this.nodeSource.indexOf('aws-ec2'), 1);
          this.nodeSource.splice(this.nodeSource.indexOf('aws-api'), 1);
        }
        break;
      }
      case 'azure': {
        this.azureCheck = e.target.checked;
        if (e.target.checked) {
          this.nodeSource.push('azure-api');
          this.nodeSource.push('azure-vm');
        } else {
          this.nodeSource.splice(this.nodeSource.indexOf('azure-api'), 1);
          this.nodeSource.splice(this.nodeSource.indexOf('azure-vm'), 1);
        }
        break;
      }
      case 'gcp': {
        this.gcpCheck = e.target.checked;
        if (e.target.checked) {
          this.nodeSource.push('gcp-api');
        } else {
          this.nodeSource.splice(this.nodeSource.indexOf('gcp-api'), 1);
        }
        break;
      }
    }
    this.search(null);
  }

  onSearchInput(value: string) {
    const nameArray = [];
    nameArray.push(value);
    this.search(nameArray);
  }

  loadMoreFunc() {
    this.load.emit({search: null, nodearray: this.nodeSource});
  }

  showSpinner() {
   return (this.nodeManagerLength === 10 && !this.loadMore);
  }


}
