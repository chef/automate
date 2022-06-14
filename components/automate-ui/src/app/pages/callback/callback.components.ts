import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
//import { Router } from '@angular/router';

@Component({
  selector: 'app-signin'
})
export class CallbackComponent implements OnInit {
  
    constructor(
    //private router: Router,
    private http: HttpClient
  ) { }

  ngOnInit() {
    let path = `/session${window.location.pathname}${window.location.search}`;
    console.log("get path: " + path);
    //this.router.navigateByUrl(path);
    this.http.get<any>(`${window.location.origin}${path}`).subscribe((res)=>{
    console.log(res);
   },(err) =>{
    console.error(err);
   })
  }

}
