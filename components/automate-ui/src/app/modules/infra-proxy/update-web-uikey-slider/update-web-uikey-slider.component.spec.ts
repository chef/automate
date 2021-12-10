import { ComponentFixture, TestBed } from '@angular/core/testing';
import { UpdateWebUIKeySliderComponent } from './update-web-uikey-slider.component';

describe('UpdateWebUIKeySliderComponent', () => {
  let component: UpdateWebUIKeySliderComponent;
  let fixture: ComponentFixture<UpdateWebUIKeySliderComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ UpdateWebUIKeySliderComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(UpdateWebUIKeySliderComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
