import { map } from 'rxjs/operators';
import { Component, Input } from '@angular/core';
import { FormBuilder, FormGroup, FormArray } from '@angular/forms';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { getOr } from 'lodash/fp';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { ManagerSearchFields } from '../../entities/managers/manager.actions';
import * as selectors from '../../entities/managers/manager.selectors';

@Component({
  selector: 'chef-job-nodes-form',
  templateUrl: './job-nodes-form.component.html',
  styleUrls: ['./job-nodes-form.component.scss']
})
export class JobNodesFormComponent {
  @Input() form: FormGroup;

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder
  ) {}

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

    tagGroup.controls['key'].valueChanges.subscribe(key => {
      const managerId = manager.value.id;
      const field = `tags:${key}`;
      this.store.dispatch(new ManagerSearchFields({managerId, field}));
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
    return this.previewNodesFor(managerId).pipe(
      map(nodes => nodes.length));
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
    return ['automate', 'aws-ec2', 'azure-vm'].includes(managerType);
  }
}
