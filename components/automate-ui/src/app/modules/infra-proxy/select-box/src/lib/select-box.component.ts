import {
  Component,
  OnInit,
  Input,
  forwardRef,
  EventEmitter,
  Output,
  OnChanges,
  ViewChild,
  SimpleChanges,
  AfterViewInit,
  NgZone,
  OnDestroy
} from '@angular/core';
import { NG_VALUE_ACCESSOR } from '@angular/forms';
import { ListItem } from './list-item.domain';
import { CdkDragDrop } from '@angular/cdk/drag-drop';
import { CdkVirtualScrollViewport } from '@angular/cdk/scrolling';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';
import { Store } from '@ngrx/store';
import { combineLatest, Subject } from 'rxjs';
import {
  filter,
  takeUntil,
  debounceTime,
  distinctUntilChanged,
  map,
  pairwise,
  throttleTime
} from 'rxjs/operators';
import { isNil } from 'lodash/fp';
import { NgrxStateAtom } from 'app/ngrx.reducers';
import { EntityStatus } from 'app/entities/entities';
import {
  getAllStatus,
  roleList
} from 'app/entities/infra-roles/infra-role.selectors';
import { GetRoles } from 'app/entities/infra-roles/infra-role.action';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';
import { Regex } from 'app/helpers/auth/regex';

export const SELECT_BOX_ACCESSOR: any = {
  provide: NG_VALUE_ACCESSOR,
  useExisting: forwardRef(() => SelectBoxComponent),
  multi: true
};

@Component({
  selector: 'app-select-box',
  templateUrl: './select-box.component.html',
  styleUrls: ['./select-box.component.scss'],
  providers: [SELECT_BOX_ACCESSOR]
})
export class SelectBoxComponent implements OnInit, OnChanges, OnDestroy, AfterViewInit {

  /* paramter used to pass in the list items*/
  @Input() list;
  /* option to turn on search feature on the lists */
  @Input() search;
  @Input() disabled = false;
  @Input() currentRunList: ListItem[] = [];
  @Input() currentPage = 0;
  @Input() orgId: string;
  @Input() serverId: string;

  @Output() selectedValues: EventEmitter<ListItem[]> =   new EventEmitter();
  @ViewChild('scroller') scroller: CdkVirtualScrollViewport;

  public createrolesList: InfraRole[] = [];
  public defaultType = 'available roles and recipes';
  public error = false;
  public isLeftItemDragging = false;
  public isRightItemDragging = false;
  public leftFilterText = '';
  public loading = false;
  public loadRecipesOnly = false;
  public leftSelected = false;
  public rightSelected = false;
  /* working list of items on the left side */
  public originalItems: ListItem[] = [];
  public per_page = 9;
  public searchChangeObserver: Subject<string> = new Subject<string>();
  public searchValue = '';
  public typeAvailable: string[] = ['available roles and recipes', 'available roles', 'available recipes'];
  private isDestroyed = new Subject<boolean>();

  constructor(
    private store: Store<NgrxStateAtom>,
    private sanitizer: DomSanitizer,
    private ngZone: NgZone
  ) { }

  ngOnInit() {
    this.combinedRunlist(this.defaultType);
   }

  ngOnChanges(changes: SimpleChanges): void {
    this.originalItems = [];
    if (changes?.list?.currentValue) {
      this.combinedRunlist(this.defaultType);
    } else if (changes?.list?.currentValue.length > 0 ) {
      this.originalItems = changes.list.currentValue;
    }
  }

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  ngAfterViewInit(): void {
    this.scroller.elementScrolled().pipe(
      map(() => this.scroller.measureScrollOffset('bottom')),
      pairwise(),
      filter(([y1, y2]) => (y2 < y1 && y2 < 140)),
      throttleTime(200)
    ).subscribe(() => {
      if (!this.loading && !this.loadRecipesOnly) {
        this.ngZone.run(() => {
          this.currentPage += 1;
          this.loadRoles();
        });
      }
    }
    );
  }

  /* This method moves items from original list to selected on button click*/
  addItems() {
    this.moveItems(this.originalItems, this.currentRunList, 0);
  }

  /*This method handles the drag event onto original list on the left */
  dragOntoLeftItems(event: CdkDragDrop<string[]>) {
    this.isLeftItemDragging = false;
    if (event.previousContainer === event.container) {
      if ( this.getLeftSelectedList().length === 1 && this.originalItems.length > 1) {
        this.changeItemPosition(this.originalItems, event.previousIndex, event.currentIndex);
      } else if (this.getLeftSelectedList().length >= 2) {
        const leftSelectedList = this.getLeftSelectedList();
        this.moveItemsInArray(
          this.originalItems, event.previousIndex, event.currentIndex, leftSelectedList);
      }
    } else {
      this.moveItems(this.currentRunList, this.originalItems, event.currentIndex);
    }
  }

  /*This method handles the drag event onto selected list on the right */
  dragOntoRightItems(event: CdkDragDrop<string[]>) {
    this.isRightItemDragging = false;
    if (event.previousContainer === event.container) {
      if (this.getRightSelectedList().length === 1 && this.currentRunList.length > 1) {
        this.changeItemPosition(this.currentRunList, event.previousIndex, event.currentIndex);
      } else if (this.getRightSelectedList().length >= 2) {
        const rightSelectedList = this.getRightSelectedList();
        this.moveItemsInArray(
          this.currentRunList, event.previousIndex, event.currentIndex, rightSelectedList);
      }
    } else {
      this.moveItems(this.originalItems, this.currentRunList, event.currentIndex);
    }
  }

  /* This method returns the selected items on the original list on left side*/
  getLeftSelectedList(): ListItem[] {
    const leftSelectedList: ListItem[] = [];
    this.originalItems.forEach(
      element => {
        if (element.selected) { leftSelectedList.push(element); }
      }
    );
    return leftSelectedList;
  }

  /* This method returns the selected items on the selected list on right side*/
  getRightSelectedList(): ListItem[] {
    const rightSelectedList: ListItem[] = [];
    this.currentRunList.forEach(
      element => {
        if (element.selected) { rightSelectedList.push(element); }
      }
    );
    this.rightSelected = true;
    return rightSelectedList;
  }

  getSelectedData() {
    this.getLeftSelectedList().length > 0 ? this.leftSelected = true : this.leftSelected = false;
    this.getRightSelectedList().length > 0 ? this.rightSelected = true : this.rightSelected = false;
  }

  handleInput(currentText): void {
    this.leftFilterText = currentText;
    if (this.searchChangeObserver.observers.length === 0) {
      this.searchChangeObserver.pipe(debounceTime(1000), distinctUntilChanged())
        .subscribe(term => {
          this.searchRunlist(term);
        });
    }
    this.searchChangeObserver.next(currentText);
  }

  highlightText(list: string, searchText: string): SafeHtml {
    if (!list) { return []; }
    if (!searchText) { return list; }

    const value = list.replace(
      searchText, `<span class='highlight-text'>${searchText}</span>` );

    return this.sanitizer.bypassSecurityTrustHtml(value);
  }

  moveItemUp(): void {
    const rightSelectedList = this.getRightSelectedList();
    for (let i = 0; i < rightSelectedList.length; i++) {
      const idx = this.currentRunList.indexOf(rightSelectedList[i]);
      this.currentRunList[idx].selected = false;
      if (idx > 0) {
        const itemToMove = this.currentRunList.splice(idx, 1);
        this.currentRunList.splice(idx - 1, 0, itemToMove[0]);
      }
    }
    this.getSelectedData();
    this.selectedValues.emit(this.currentRunList);
  }

  moveItemDown(): void {
    const rightSelectedList = this.getRightSelectedList();
    const reverseList = rightSelectedList.concat();
    reverseList.reverse();
    for (let i = 0; i < reverseList.length; i++) {
      const idx = this.currentRunList.indexOf(reverseList[i]);
      this.currentRunList[idx].selected = false;
      if (idx < this.currentRunList.length) {
        const itemToMove = this.currentRunList.splice(idx, 1);
        this.currentRunList.splice(idx + 1, 0, itemToMove[0]);
      }
    }
    this.getSelectedData();
    this.selectedValues.emit(this.currentRunList);
  }

  onDragleftItemStarted(): void {
    this.isLeftItemDragging = true;
  }

  onDragrightItemStarted(): void {
    this.isRightItemDragging = true;
  }

  /* This method moves items from selected list to original on button click*/
  removeItems(): void {
    this.moveItems(this.currentRunList, this.originalItems, 0);
  }

  selectChangeHandler(id: string): void {
    this.defaultType = id;
    this.combinedRunlist(this.defaultType);
  }

  selectedRunlist(item: ListItem) {
    item.selected = (!this.disabled && !item.selected);
    this.getSelectedData();
  }

  /* helper method that changes the position of items in the list*/
  private changeItemPosition(list: ListItem[], currPos: number, newPos: number) {
    const item: ListItem = list.splice(currPos, 1)[0];
    item.selected = false;
    list.splice(newPos, 0, item);
    this.getSelectedData();
    this.selectedValues.emit(this.currentRunList);
  }

  private removeSelectedRunlist() {
    if (this.currentRunList.length > 0) {
      this.currentRunList.forEach(elm => {
        this.originalItems.forEach((avail, index) => {
          if (avail.value === elm.value) {
            this.originalItems.splice(index, 1);
          }
        });
      });
    }
  }

  private removeDuplicateRole(): ListItem[] {
    const setObj = new Set(); // create key value pair from array of array
    const result = this.originalItems.reduce((acc, item) => {
      if (!setObj.has(item.value)) {
        setObj.add(item.value);
        acc.push(item);
      }
      return acc;
    }, []); // converting back to array from mapobject
    return result;
  }

  private filterRunlist(searchvalue) {
    if (!this.error) {
      this.originalItems = this.originalItems.filter(
        item => item.value.search(new RegExp(searchvalue, 'i')) > -1);
    } else {
      this.originalItems = [];
    }
  }

  private getRecipes(searchvalue) {
    this.list?.forEach(element => {
      this.originalItems.push(new ListItem(element.name, element.type));
    });
    this.removeSelectedRunlist();
    this.filterRunlist(searchvalue);
  }

  private loadRoles(): void {
    this.createrolesList = [];
    this.loading = true;
    const payload = {
      roleName: this.searchValue,
      server_id: this.serverId,
      org_id: this.orgId,
      page: this.currentPage,
      per_page: this.per_page
    };
    this.store.dispatch(new GetRoles(payload));
    combineLatest([
      this.store.select(getAllStatus),
      this.store.select(roleList)
    ]).pipe(takeUntil(this.isDestroyed))
    .subscribe(([_getRolesSt, RolesState]) => {
      if (!isNil(RolesState) && _getRolesSt === EntityStatus.loadingSuccess) {
        this.createrolesList = RolesState.items;
        if (RolesState.items.length) {
          this.createrolesList.forEach((role) => {
            this.originalItems.push(new ListItem(role.name, 'role'));
          });
          const result = this.removeDuplicateRole();
          this.originalItems = result;
          this.removeSelectedRunlist();
          this.filterRunlist(this.searchValue);
          this.loading = false;
        } else {
          this.loading = false;
        }
      } else if (_getRolesSt === EntityStatus.loadingFailure) {
        this.loading = false;
      }
    });
  }

  private combinedRunlist(id: string) {
    this.originalItems = [];
    switch (id) {
      case 'available roles and recipes':
        this.currentPage = 1;
        this.error = false;
        if (this.searchValue !== '' && !Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN.test(
          this.searchValue)) {
          this.error = true;
          this.originalItems = [];
        } else {
          this.error = false;
          this.getRecipes(this.searchValue);
          this.loadRoles();
          this.loadRecipesOnly = false;
        }
        break;

      case 'available roles':
        this.currentPage = 1;
        this.error = false;
        if (this.searchValue !== '' && !Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN.test(
          this.searchValue)) {
          this.error = true;
        } else {
          this.error = false;
          this.loadRoles();
          this.loadRecipesOnly = false;
        }
        break;

      case 'available recipes':
        this.loadRecipesOnly = true;
        if (this.searchValue !== '' && !Regex.patterns.NO_WILDCARD_ALLOW_HYPHEN.test(
          this.searchValue)) {
          this.error = true;
        } else {
          this.error = false;
          this.getRecipes(this.searchValue);
        }
        break;

    }
    this.removeSelectedRunlist();
    this.getSelectedData();
  }

  /*helper method that moves items between lists */
  private moveItems(fromList: ListItem[], toList: ListItem[], insertIndex: number) {
    for (let removeIndex = fromList.length - 1; removeIndex >= 0; removeIndex--) {
      const item: ListItem = fromList[removeIndex];
      if (item.selected) {
        fromList.splice(removeIndex, 1);
        item.selected = false;
        toList.splice(insertIndex, 0, item);
      }
    }
    this.getSelectedData();
    this.selectedValues.emit(this.currentRunList);
  }

  private moveItemsInArray(
    list: ListItem[],
    currPos: number,
    newPos: number,
    selected: ListItem[]
    ) {
    const fromIndex = this.clamp(currPos, list.length - 1);
    const toIndex = this.clamp(newPos, list.length - 1);
    if (fromIndex === toIndex) {
      return;
    }
    const selectedlist = list[fromIndex];
    const index = toIndex < fromIndex ? -1 : 1;
    if (selected.length > 0) {
      const selectedItem = [];
      for (const item of selected) {
        const toPush = item;
        if (toPush) {
          toPush.selected = false;
          selectedItem.push(toPush);
        }
      }
      for (let i = 0; i < selected.length; i++) {
        list.splice(list.indexOf(selectedItem[i]), 1);
      }
      let ch = toIndex;
      if (fromIndex < toIndex) {
        ch = ch - selectedItem.length + 1;
        if (ch < 0) {
          ch = toIndex;
        }
      }
      if (ch > list.length) {
        list.push(...selectedItem);
      } else {
        list.splice(ch, 0, ...selectedItem);
      }
      return;
    }
    for (let i = fromIndex; i !== toIndex; i += index) {
      list[i] = list[i + index];
    }
    list[toIndex] = selectedlist;
    this.getSelectedData();
    this.selectedValues.emit(this.currentRunList);
  }

  /** Clamps a number between zero and a maximum. */
  private clamp(value: number, max: number): number {
    return Math.max(0, Math.min(max, value));
  }

  private searchRunlist(searchvalue: string) {
    this.searchValue = searchvalue;
    this.currentPage = 1;
    this.originalItems = [];
    this.combinedRunlist(this.defaultType);
  }
}
