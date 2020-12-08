import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ConfirmApplyStartModalComponent } from './confirm-apply-start-modal.component';

describe('ConfirmApplyStartModalComponent', () => {
  let component: ConfirmApplyStartModalComponent;
  let fixture: ComponentFixture<ConfirmApplyStartModalComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
      declarations: [
        ConfirmApplyStartModalComponent
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ConfirmApplyStartModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('renders', () => {
    const modal = fixture.nativeElement;
    expect(modal).not.toBeNull();
  });

  it('displays a title', () => {
    const title = fixture.nativeElement.querySelector('#title');
    expect(title).not.toBeNull();
    expect(title.textContent.trim()).toEqual('Update Projects?');
  });

  it('displays a confirm-button', () => {
    const button = fixture.nativeElement.querySelector('#confirm-button');
    expect(button).not.toBeNull();
    expect(button.textContent.trim()).toEqual('Update Projects');
  });

  describe('when confirm-button is clicked', () => {
    it('emits a confirm event', async() => {
      spyOn(component.confirm, 'emit');

      const button = fixture.nativeElement.querySelector('#confirm-button');
      await button.click();

      expect(component.confirm.emit).toHaveBeenCalled();
    });
  });

  it('displays a cancel-button', () => {
    const button = fixture.nativeElement.querySelector('#cancel-button');
    expect(button).not.toBeNull();
    expect(button.textContent.trim()).toEqual('Cancel');
  });

  describe('when cancel-button is clicked', () => {
    it('emits a cancel event', async() => {
      spyOn(component.cancel, 'emit');

      const button = fixture.nativeElement.querySelector('#cancel-button');
      await button.click();

      expect(component.cancel.emit).toHaveBeenCalled();
    });
  });
});
