import {Component, OnInit} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {environment} from '../environments/environment';

export interface RespManifest {
  schema_version : string;
  build : string;
  packages : string[];
}

export interface RespServer {
  id : string;
  title : string;
  type : string;
  description : string;
  fqdn : string;
}

export class Server {
  id : string;
  title : string;
  type : string;
  description : string;
  fqdn : string;

  constructor(respServer : RespServer) {
    this.id = respServer.id;
    this.title = respServer.title;
    this.description = respServer.description;
    this.type = respServer.type;
    this.fqdn = respServer.fqdn;
  }
}

@Component({selector: 'app-root', templateUrl: './app.component.html', styleUrls: ['./app.component.css']})
export class AppComponent implements OnInit {
  servers : Server[];
  channel : string;

  public deploy_in_progress : boolean;
  deploy_logs_url : string;

  build_version : string;
  packages : string[];

  constructor(private http : HttpClient) {
    this.channel = environment.channel;
    this.servers = [];
    this.deploy_in_progress = false;
  }

  ngOnInit() : void {
    this.http.get < RespServer[] > ("/assets/data.json").subscribe(data => {
      this.servers = data.map((respServer : RespServer) => new Server(respServer))
    });

    this
      .http
      .get("/assets/deploy_logs", {responseType: 'text'})
      .subscribe(data => {
        this.deploy_in_progress = true;
        this.deploy_logs_url = data;
      });

    this
      .http
      .get <RespManifest> (`https://packages.chef.io/manifests/${this.channel}/automate/latest.json`)
      .subscribe(data => {
        this.build_version = data.build;
        this.packages = data.packages;
      });
  }
}
