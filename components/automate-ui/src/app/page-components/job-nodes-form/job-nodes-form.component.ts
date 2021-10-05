import { map } from 'rxjs/operators';
import { Component, Input, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, FormArray } from '@angular/forms';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { getOr } from 'lodash/fp';
import { NgrxStateAtom } from '../../ngrx.reducers';
import { ManagerSearchFields, ManagersSearch} from '../../entities/managers/manager.actions';
import * as selectors from '../../entities/managers/manager.selectors';
//import { ManagerRequests } from 'app/entities/managers/manager.requests';

@Component({
  selector: "chef-job-nodes-form",
  templateUrl: "./job-nodes-form.component.html",
  styleUrls: ["./job-nodes-form.component.scss"],
})
export class JobNodesFormComponent implements OnInit {
  @Input() form: FormGroup;
  public automateCheck = false;
  public awsCheck = false;
  public azureCheck = false;
  public gcpCheck = false;
  public nodeSource = [];

  constructor(
    private store: Store<NgrxStateAtom>,
    private fb: FormBuilder //private managerRequests: ManagerRequests
  ) {}

  ngOnInit() {
    this.store.dispatch(
      new ManagersSearch({
        page: 1,
        per_page: 10,
      })
    );
  }

  addRegionValue(regionsGroup: FormGroup, index: number) {
    const valuesArray = regionsGroup.controls["values"] as FormArray;
    valuesArray.insert(index, this.fb.control(""));
  }

  removeRegionValue(regionsGroup: FormGroup, index: number) {
    const valuesArray = regionsGroup.controls["values"] as FormArray;
    valuesArray.removeAt(index);
  }

  addTag(manager: FormGroup) {
    const tagsArray = manager.controls["tagsArray"] as FormArray;
    const tagGroup = this.fb.group({
      key: "",
      values: this.fb.array([""]),
      include: true,
    });
    tagsArray.push(tagGroup);

    tagGroup.controls["key"].valueChanges.subscribe((key) => {
      const managerId = manager.value.id;
      const field = `tags:${key}`;
      this.store.dispatch(new ManagerSearchFields({ managerId, field }));
    });
  }

  removeTag(manager: FormGroup, index: number) {
    const tagsArray = manager.controls["tagsArray"] as FormArray;
    tagsArray.removeAt(index);
  }

  addTagValue(tag: FormGroup) {
    const valuesArray = tag.controls["values"] as FormArray;
    valuesArray.push(this.fb.control(""));
  }

  removeTagValue(tag: FormGroup, index: number) {
    const valuesArray = tag.controls["values"] as FormArray;
    valuesArray.removeAt(index);
  }

  fieldValuesFor(managerId: string, field: string): Observable<string[]> {
    return this.store
      .select(selectors.fieldsByManager)
      .pipe(map(getOr([], `${managerId}.fields.${field}`))) as Observable<
      string[]
    >;
  }

  previewNodesFor(managerId: string): Observable<string[]> {
    return this.store
      .select(selectors.nodesByManager)
      .pipe(map(getOr([], `${managerId}.nodes`))) as Observable<string[]>;
  }

  isLoadingPreviewNodesFor(managerId: string): Observable<boolean> {
    return this.store
      .select(selectors.nodesByManager)
      .pipe(map(getOr(false, `${managerId}.loading`))) as Observable<boolean>;
  }

  previewNodesCountFor(managerId: string): Observable<number> {
    return this.previewNodesFor(managerId).pipe(map((nodes) => nodes.length));
  }

  availableNodesCountFor(managerId: string): Observable<number> {
    return this.store
      .select(selectors.nodesByManager)
      .pipe(map(getOr(0, `${managerId}.allTotal`))) as Observable<number>;
  }

  logoFor(managerType: string) {
    const dir = "assets/img";
    switch (managerType) {
      case "automate": {
        return `${dir}/logos/AutomateLogo-default.svg`;
      }
      case "aws-ec2":
      case "aws-api": {
        return `${dir}/logo-aws.svg`;
      }
      case "azure-vm":
      case "azure-api": {
        return `${dir}/logo-azure.svg`;
      }
      case "gcp-api": {
        return `${dir}/logo-gcp.svg`;
      }
    }
  }

  isAwsApi(managerType: string): boolean {
    return managerType === "aws-api";
  }

  supportsFilterByName(managerType: string): boolean {
    return ["automate"].includes(managerType);
  }

  supportsFilterByRegion(managerType: string): boolean {
    return ["aws-ec2", "azure-vm"].includes(managerType);
  }

  supportsFilterByTag(managerType: string): boolean {
    return ["automate", "aws-ec2", "azure-vm"].includes(managerType);
  }

  search(searchName: string[]) {
    console.log("Search value is", searchName);
    var payload = {};
    if (searchName === null && this.nodeSource.length === 0) {
      this.store.dispatch(new ManagersSearch({}));
    } else {
      if (searchName === null) {
        payload = {
          filter_map: [
            {
              key: "manager_type",
              values: this.nodeSource,
            },
          ]
        };
      } else if (searchName && this.nodeSource.length > 0) {
        payload = {
          filter_map: [
            {
              key: "manager_type",
              values: this.nodeSource,
            },
            {
              key: "name",
              values: searchName,
            },
          ],
        };
      } else if (searchName && this.nodeSource.length === 0) {
        payload = {
          filter_map: [
            {
              key: "name",
              values: searchName,
            },
          ],
        };
      }

      console.log("payload is", payload);
      this.store.dispatch(new ManagersSearch(payload));
    }
  }

  onclickCheckbox(e: any, val: string) {
    switch (val) {
      case "automate": {
        this.automateCheck = e.target.checked;
        if (e.target.checked) {
          this.nodeSource.push("automate");
        } else {
          this.nodeSource.splice(this.nodeSource.indexOf("automate"), 1);
        }
        break;
      }
      case "aws": {
        this.awsCheck = e.target.checked;
        if (e.target.checked) {
          this.nodeSource.push("aws-ec2");
          this.nodeSource.push("aws-api");
        } else {
          this.nodeSource.splice(this.nodeSource.indexOf("aws-ec2"), 1);
          this.nodeSource.splice(this.nodeSource.indexOf("aws-api"), 1);
        }
        break;
      }
      case "azure": {
        this.azureCheck = e.target.checked;
        if (e.target.checked) {
          this.nodeSource.push("azure-api");
          this.nodeSource.push("azure-vm");
        } else {
          this.nodeSource.splice(this.nodeSource.indexOf("azure-api"), 1);
          this.nodeSource.splice(this.nodeSource.indexOf("azure-vm"), 1);
        }
        break;
      }
      case "gcp": {
        this.gcpCheck = e.target.checked;
        if (e.target.checked) {
          this.nodeSource.push("gcp-api");
        } else {
          this.nodeSource.splice(this.nodeSource.indexOf("gcp-api"), 1);
        }
        break;
      }
    }
    console.log(val, e.target.checked);
    console.log("final array is ", this.nodeSource);
    this.search(null);
  }

  onSearchInput(event) {
    const nameArray = [];
    const value = event.target.value;
    console.log(value);
    nameArray.push(value);
    //this.search(nameArray);
  }
}
