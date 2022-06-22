import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SelectboxComponent } from './selectbox.component';
import { StoreModule, Store } from '@ngrx/store';
import { HttpClient, HttpHandler } from '@angular/common/http';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { ngrxReducers, NgrxStateAtom, runtimeChecks } from 'app/ngrx.reducers';
import { EventEmitter } from '@angular/core';
// import { By } from '@angular/platform-browser';
import { MockComponent } from 'ng2-mock-component';
import { InfiniteScrollModule } from 'ngx-infinite-scroll';


describe('SelectboxComponent', () => {
  let store: Store<NgrxStateAtom>;
  let component: SelectboxComponent;
  let fixture: ComponentFixture<SelectboxComponent>;

  beforeEach( () => {
    TestBed.configureTestingModule({
      declarations: [
        SelectboxComponent,
        MockComponent({ selector: 'chef-button', inputs: ['disabled']}),
        MockComponent({ selector: 'ul'}),
        MockComponent({ selector: 'li'})
      ],
      providers: [
        HttpClient,
        HttpHandler
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        InfiniteScrollModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: []
    }).compileComponents();
    store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture = TestBed.createComponent(SelectboxComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('select box', () => {

    beforeEach( () => {
      component.searchData = new EventEmitter();
      component.selectData = new EventEmitter();
      component.onScrollListData = new EventEmitter();
      const datalist = [];
      for (let i = 0; i < 10; i++) {
        datalist.push({
          id: `${i}`,
          name: `list ${i}`,
          type: i % 2 === 0 ? 'ssh' : 'winrm'
        });
      }
      component.data = datalist;
      component.selectedList = [];
      component.ngOnInit();
      component.ngOnChanges();
    });

    it('should emit Data', () => {
      spyOn(component.searchData, 'emit');
      component.emitData('data');
      expect(component.searchData.emit).toHaveBeenCalledWith('data');
    });

    it('should emit onScrollListData', () => {
      spyOn(component.onScrollListData, 'emit');
      component.onScrollDown();
      expect(component.onScrollListData.emit).toHaveBeenCalledWith();
    });

    it('should call ngOnInit', () => {
      component.ngOnInit();
      expect(component.copyDataListFlags).toBe(false);
      expect(component.moveLeftOrRight).toBe('');
      expect(component.selectedListTypeToMove).toBe('');
    });

    it('should select single item from left side', () => {
      component.typeValue = 'ssh';
      component.typeFieldName = 'type';
      component.copyDataListFlags = false;
      component.ngOnChanges();
      const data = {id: '0', name: 'list 0', type: 'ssh'};
      component.selectItem(data, 'left', 'ssh');
      expect(component.selectedListTypeToMove).toEqual('ssh');
      expect(component.selectedListDataToMove).toEqual([data]);
      expect(component.moveLeftOrRight).toEqual('left');
    });

    it('should select multiple item from left side', () => {
      component.typeValue = 'ssh';
      component.typeFieldName = 'type';
      component.copyDataListFlags = false;
      component.ngOnChanges();
      const firstData = {id: '0', name: 'list 0', type: 'ssh'};
      const secoundData = {id: '2', name: 'list 2', type: 'ssh'};
      component.selectItem(firstData, 'left', 'ssh');
      component.selectItem(secoundData, 'left', 'ssh');
      expect(component.selectedListTypeToMove).toEqual('ssh');
      expect(component.selectedListDataToMove).toEqual([firstData, secoundData]);
      expect(component.moveLeftOrRight).toEqual('left');
    });

    it('should select multiple item from left side and move to right side', () => {
      component.typeValue = 'ssh';
      component.typeFieldName = 'type';
      component.copyDataListFlags = false;
      component.ngOnChanges();
      const firstData = {id: '0', name: 'list 0', type: 'ssh'};
      const secoundData = {id: '2', name: 'list 2', type: 'ssh'};
      component.selectItem(firstData, 'left', 'ssh');
      component.selectItem(secoundData, 'left', 'ssh');
      component.moveRight();
      expect(component.moveLeftOrRight).toEqual('');
      expect(component.selectedListDataToMove).toEqual([]);
      expect(component.selectedListData).toEqual([secoundData, firstData]);
      expect(component.moveLeftOrRight).toEqual('');
    });

    it('should select single item from right side', () => {
      component.typeValue = 'ssh';
      component.typeFieldName = 'type';
      component.copyDataListFlags = false;
      component.ngOnChanges();
      const firstData = {id: '0', name: 'list 0', type: 'ssh'};
      const secoundData = {id: '2', name: 'list 2', type: 'ssh'};
      component.selectItem(firstData, 'left', 'ssh');
      component.selectItem(secoundData, 'left', 'ssh');
      component.moveRight();
      component.selectItem(secoundData, 'right', 'ssh');
      expect(component.selectedListTypeToMove).toEqual('ssh');
      expect(component.selectedListDataToMove).toEqual([secoundData]);
      expect(component.moveLeftOrRight).toEqual('right');
    });

    it('should select multiple item from right side', () => {
      component.typeValue = 'ssh';
      component.typeFieldName = 'type';
      component.copyDataListFlags = false;
      component.ngOnChanges();
      const firstData = {id: '0', name: 'list 0', type: 'ssh'};
      const secoundData = {id: '2', name: 'list 2', type: 'ssh'};
      component.selectItem(firstData, 'left', 'ssh');
      component.selectItem(secoundData, 'left', 'ssh');
      component.moveRight();
      component.selectItem(secoundData, 'right', 'ssh');
      component.selectItem(firstData, 'right', 'ssh');
      expect(component.selectedListTypeToMove).toEqual('ssh');
      expect(component.selectedListDataToMove).toEqual([secoundData, firstData]);
      expect(component.moveLeftOrRight).toEqual('right');
    });

    it('should select multiple item from right side and move to left side', () => {
      component.typeValue = 'ssh';
      component.typeFieldName = 'type';
      component.copyDataListFlags = false;
      component.ngOnChanges();
      const firstData = {id: '0', name: 'list 0', type: 'ssh'};
      const secoundData = {id: '2', name: 'list 2', type: 'ssh'};
      component.selectItem(firstData, 'left', 'ssh');
      component.selectItem(secoundData, 'left', 'ssh');
      component.moveRight();
      component.selectItem(secoundData, 'right', 'ssh');
      component.moveLeft();
      expect(component.selectedListDataToMove).toEqual([]);
      expect(component.selectedListData).toEqual([firstData]);
      expect(component.moveLeftOrRight).toEqual('');
    });

    it('should select/unselect multiple item from right side and move to left side', () => {
      component.typeValue = 'ssh';
      component.typeFieldName = 'type';
      component.copyDataListFlags = false;
      component.ngOnChanges();
      const firstData = {id: '0', name: 'list 0', type: 'ssh'};
      const secoundData = {id: '2', name: 'list 2', type: 'ssh'};
      component.selectItem(firstData, 'left', 'ssh');
      component.selectItem(secoundData, 'left', 'ssh');
      component.moveRight();
      component.selectItem(secoundData, 'right', 'ssh');
      component.selectItem(secoundData, 'right', 'ssh');
      component.selectItem(firstData, 'left', 'ssh');
      component.moveLeft();
      expect(component.selectedListDataToMove).toEqual([]);
      expect(component.selectedListData).toEqual([secoundData]);
      expect(component.moveLeftOrRight).toEqual('');
    });

    it('should highlight element left', () => {
      component.typeValue = 'ssh';
      component.typeFieldName = 'type';
      component.copyDataListFlags = false;
      component.ngOnChanges();
      const data = {id: '0', name: 'list 0', type: 'ssh'};
      component.selectItem(data, 'left', 'ssh');
      expect(component.selectedListTypeToMove).toEqual('ssh');
      expect(component.selectedListDataToMove).toEqual([data]);
      expect(component.moveLeftOrRight).toEqual('left');
      expect(component.highlightElement(data)).toBeTrue();
    });
  });
});
