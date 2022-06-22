import { ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { DataFeedConfigDetailsComponent } from './data-feed-config-details.component';
describe('DataFeedConfigDetailsComponent', () => {
  let component: DataFeedConfigDetailsComponent;
  let fixture: ComponentFixture<DataFeedConfigDetailsComponent>;


  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [
        DataFeedConfigDetailsComponent
    ],
    imports: [
      StoreModule.forRoot(ngrxReducers, { runtimeChecks })
    ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(DataFeedConfigDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
  describe('DataFeedConfigDetailsComponent', () => {
    it('check covertFeedInterval func is used to convert human readable format', () => {
      const res = component.covertFeedInterval('0h1m2s');
      expect(res).toEqual('1 Minute 2 Seconds ');
    });
    it('getData func is used to get to 5 data or all data', () => {
      const StatusCodes = [200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210];
      let getFirstFiveDataStatusCodesShow = true;
      let FirstFiveStatusCodes = component.getData(StatusCodes, getFirstFiveDataStatusCodesShow);
      expect(FirstFiveStatusCodes).toEqual([200, 201, 202, 203, 204]);
      getFirstFiveDataStatusCodesShow = false;
      FirstFiveStatusCodes = component.getData(StatusCodes, getFirstFiveDataStatusCodesShow);
      expect(FirstFiveStatusCodes).toEqual(StatusCodes);
    });
  });
});
