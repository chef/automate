import { ComponentFixture, TestBed, waitForAsync } from '@angular/core/testing';
import { MockComponent } from 'ng2-mock-component';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { By } from '@angular/platform-browser';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { ScrollingModule } from '@angular/cdk/scrolling';
import { SelectBoxComponent } from './select-box.component';
import { ListItem } from './list-item.domain';
import { GetRolesSuccess } from 'app/entities/infra-roles/infra-role.action';
import { InfraRole } from 'app/entities/infra-roles/infra-role.model';

describe('SelectBoxComponent', () => {
  let component: SelectBoxComponent;
  let element;
  let fixture: ComponentFixture<SelectBoxComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'mat-select', inputs: ['value'] }),
        MockComponent({ selector: 'mat-icon' }),
        MockComponent({ selector: 'mat-option', inputs: ['value'] }),

        SelectBoxComponent
      ],
      imports: [
        DragDropModule,
        ScrollingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SelectBoxComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  describe('role run list', () => {
    let store: Store<NgrxStateAtom>;
    const availableRunlist: ListItem[] = [];
    availableRunlist.push(new ListItem('aix', 'recipe'));
    availableRunlist.push(new ListItem('audit', 'recipe'));

    const emptyRunlist: InfraRole[] = [];

    beforeEach(() => {
      store = TestBed.inject(Store);
    });

    it('render the available recipes and roles list', () => {
      component.originalItems = availableRunlist;
      expect(component.originalItems.length).not.toBeNull();
      expect(element.query(By.css('.no-data'))).toBeNull();
    });

    it('show no Data available', () => {
      store.dispatch(new GetRolesSuccess({roles: emptyRunlist,  total: emptyRunlist.length}));
      expect(component.createrolesList.length).toBe(0);
    });
  });
});
