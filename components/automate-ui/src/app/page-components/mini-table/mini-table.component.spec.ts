import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';

import { MiniTableComponent } from './mini-table.component';
import { DefinedOrDefaultPipe } from '../../pipes/defined-or-default.pipe';

describe('MiniTableComponent', () => {
  let component: MiniTableComponent;
  let fixture: ComponentFixture<MiniTableComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        DefinedOrDefaultPipe,
        MiniTableComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(MiniTableComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  describe('when the array is 5x2', () => {
    beforeEach(() => {
      component.tableData = [
        ['Teams', '8 teams'],
        ['Groups', '2 groups'],
        ['Last Login', 'mm/dd/yyyy'],
        ['Created Date', 'mm/dd/yyyy'],
        ['Created By', 'someadmin']
      ];
      fixture.detectChanges();
    });

    it('should be created', () => {
      expect(fixture.debugElement.queryAll(By.css('tr')).length).toEqual(5);
      expect(fixture.debugElement.queryAll(By.css('td')).length).toEqual(2 * 5);
    });

  });

  describe('when there are undefined entries', () => {
    beforeEach(() => {
      component.tableData = [
        ['Teams', undefined]
      ];
      fixture.detectChanges();
    });

    it('should render the undefined entry as --', () => {
      const element = fixture.nativeElement;
      expect(element.querySelector('td:nth-child(1)').innerText.trim()).toEqual('Teams');
      expect(element.querySelector('td:nth-child(2)').innerText.trim()).toEqual('--');
    });
  });
});
