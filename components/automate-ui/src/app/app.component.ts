import { Component,Injectable } from '@angular/core';
// import { webSocket } from 'rxjs/webSocket';
import { Inject } from '@angular/core';

@Inject({})
@Injectable()

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html'
})
export class AppComponent {


  constructor(){
  
  }
}
