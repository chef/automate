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

  //  public subject =webSocket('ws://localhost:4209')
   
  // test(){
  //   debugger
  //   console.log('test run ')
  //   this.subject.subscribe({
  //     next: msg => console.log('message received: ' + msg), // Called whenever there is a message from the server.
  //     error: err => console.log(err), // Called if at any point WebSocket API signals some kind of error.
  //     complete: () => console.log('complete') // Called when connection is closed (for whatever reason).
  //   });
  // }

  constructor(){
    debugger
  }
}
