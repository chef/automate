## Context
Perfomance Tetsing and benchmarking of the modified and new APIs.


| Name | Api End Point | Filters | Main branch Response time | feature branch Response time | Version | date range |	
| --- | --- | --- | --- | --- | --- | --- |
| Node search 	|/api/v0/compliance/reporting/nodes/search	|"{start_time:""2022-07-24T00:00:00Z"",end_time:""2022-08-03T23:59:59Z""}"	|1049ms	|964ms	|2	|10			
| Profile 	|/api/v0/compliance/reporting/profiles	|"{start_time:""2022-07-24T00:00:00Z"",end_time:""2022-08-03T23:59:59Z""}"	|286ms	|278ms	|2	|10			
| Controls	|/api/v0/compliance/reporting/controls	|"{start_time:""2022-07-24T00:00:00Z"",end_time:""2022-08-03T23:59:59Z""}"	|858ms	|988ms	|2	|10			
| Failures Node	|/api/v0/compliance/reporting/stats/failures	|"{start_time:""2022-07-24T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|712ms	|686ms	|2	|10			
| Trend Node	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-07-24T00:00:00Z"",end_time:""2022-08-03T23:59:59Z""}"	|1077ms	|1232ms	|2	|10			
| Summary Node	|/api/v0/compliance/reporting/stats/trend	|"{start_time:""2022-07-24T00:00:00Z"",end_time:""2022-08-03T23:59:59Z""}"	|240ms	|251ms	|2	|10			
| Failures Controls	|/api/v0/compliance/reporting/stats/failures	|"{start_time:""2022-07-24T00:00:00Z"",end_time:""2022-08-03T23:59:59Z""}"	|250ms	|245ms	|2	|10			
| Trend Controls	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-07-24T00:00:00Z"",end_time:""2022-08-03T23:59:59Z""}"	|881ms	|893ms	|2	|10			
| Summary Controls	|/api/v0/compliance/reporting/stats/trend	|"{start_time:""2022-07-24T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|234ms	|244ms	|2	|10			
| Report Summary	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-07-24T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|280ms	|255ms	|2	|10			
| Node search 	|/api/v0/compliance/reporting/nodes/search	|"{start_time:""2022-07-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|267ms	|295ms	|2	|30			
| Profile 	|/api/v0/compliance/reporting/profiles	|"{start_time:""2022-07-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|259ms	|281ms	|2	|30			
| Controls	|/api/v0/compliance/reporting/controls	|"{start_time:""2022-07-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|889ms	|901ms	|2	|30			
| Failures Node	|/api/v0/compliance/reporting/stats/failures	|"{start_time:""2022-07-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|245ms	|240ms	|2	|30			
| Trend Node	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-07-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|279ms	|284ms	|2	|30			
| Summary Node	|/api/v0/compliance/reporting/stats/trend	|"{start_time:""2022-07-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|244ms	|256ms	|2	|30			
| Failures Controls	|/api/v0/compliance/reporting/stats/failures	|"{start_time:""2022-07-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|261ms	|267ms	|2	|30			
| Trend Controls	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-07-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|276ms	|321ms	|2	|30			
| Summary Controls	|/api/v0/compliance/reporting/stats/trend	|"{start_time:""2022-07-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|251ms	|242ms	|2	|30			
| Report Summary	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-07-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|245ms	|254ms	|2	|30			
| Node search 	|/api/v0/compliance/reporting/nodes/search	|"{start_time:""2022-06-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|264ms	|285ms	|2	|60			
| Profile 	|/api/v0/compliance/reporting/profiles	|"{start_time:""2022-06-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|259ms	|278ms	|2	|60			
| Controls	|/api/v0/compliance/reporting/controls	|"{start_time:""2022-06-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|927ms	|902ms	|2	|60			
Failures Node	|/api/v0/compliance/reporting/stats/failures	|"{start_time:""2022-06-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|242ms	|240ms	|2	|60			
Trend Node	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-06-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|263ms	|245ms	|2	|60			
Summary Node	|/api/v0/compliance/reporting/stats/trend	|"{start_time:""2022-06-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|243ms	|268ms	|2	|60			
Failures Controls	|/api/v0/compliance/reporting/stats/failures	|"{start_time:""2022-06-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|243ms	|267ms	|2	|60			
Trend Controls	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-06-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|265ms	|274ms	|2	|60			
Summary Controls	|/api/v0/compliance/reporting/stats/trend	|"{start_time:""2022-06-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|245ms	|275ms	|2	|60			
Report Summary	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-06-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|243ms	|261ms	|2	|60			
Node search 	|/api/v0/compliance/reporting/nodes/search	|"{start_time:""2022-05-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|268ms	|301ms	|2	|90			
Profile 	|/api/v0/compliance/reporting/profiles	|"{start_time:""2022-05-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|258ms	|276ms	|2	|90			
Controls	|/api/v0/compliance/reporting/controls	|"{start_time:""2022-05-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|852ms	|889ms	|2	|90			
Failures Node	|/api/v0/compliance/reporting/stats/failures	|"{start_time:""2022-05-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|260ms	|274ms	|2	|90			
Trend Node	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-05-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|753ms	|745ms	|2	|90			
Summary Node	|/api/v0/compliance/reporting/stats/trend	|"{start_time:""2022-05-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|242ms	|255ms	|2	|90			
Failures Controls	|/api/v0/compliance/reporting/stats/failures	|"{start_time:""2022-05-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|240ms	|267ms	|2	|90			
Trend Controls	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-05-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|287ms	|293ms	|2	|90			
Summary Controls	|/api/v0/compliance/reporting/stats/trend	|"{start_time:""2022-05-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|240ms	|280ms	|2	|90			
Report Summary	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-05-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z""}"	|242ms	|265ms	|2	|90			
Node search 	|/api/v0/compliance/reporting/nodes/search	|"{start_time:""2022-07-24T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z"",control:”ssh-07”,node-id: ""5ca72b1f-03f8-32b0-b5f6-be2b24796494""}"	|761ms	|792ms	|2	|10			
Report Summary	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-07-24T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z"",organization:""org10"",platform_with_version: ""centos 7.5.1804""}"	|249ms	|264ms	|2	|10			
Node search 	|/api/v0/compliance/reporting/nodes/search	|"{start_time:""2022-03-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z"",control:”ssh-07”,node-id: ""5ca72b1f-03f8-32b0-b5f6-be2b24796494""}"	|265ms	|275ms	|2	|30			
Report Summary	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-07-03T00:00:00Z"",end_time: ""2022-08-03T23:59:59Z"",organization:""org10"",platform_with_version: ""centos 7.5.1804""}"	|926ms	|987ms	|2	|30			
Node search 	|/api/v0/compliance/reporting/nodes/search	|"{start_time:""2022-07-21T00:00:00Z"",end_time: ""2022-07-31T23:59:59Z""}"	|1047ms	|1112ms	|2	|10			
Profile 	|/api/v0/compliance/reporting/profiles	|"{start_time:""2022-07-21T00:00:00Z"",end_time: ""2022-07-31T23:59:59Z""}"	|253ms	|264ms	|2	|10			
Controls	|/api/v0/compliance/reporting/controls	|"{start_time:""2022-07-21T00:00:00Z"",end_time: ""2022-07-31T23:59:59Z""}"	|252ms	|325ms	|2	|10			
Failures Node	|/api/v0/compliance/reporting/stats/failures	|"{start_time:""2022-07-21T00:00:00Z"",end_time: ""2022-07-31T23:59:59Z""}"	|243ms	|255ms	|2	|10			
Trend Node	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-07-21T00:00:00Z"",end_time: ""2022-07-31T23:59:59Z""}"	|841ms	|721ms	|2	|10			
Summary Node	|/api/v0/compliance/reporting/stats/trend	|"{start_time:""2022-07-21T00:00:00Z"",end_time: ""2022-07-31T23:59:59Z""}"	|243ms	|256ms	|2	|10			
Failures Controls	|/api/v0/compliance/reporting/stats/failures	|"{start_time:""2022-07-21T00:00:00Z"",end_time: ""2022-07-31T23:59:59Z""}"	|245ms	|250ms	|2	|10			
Trend Controls	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-07-21T00:00:00Z"",end_time: ""2022-07-31T23:59:59Z""}"	|250ms	|264ms	|2	|10			
Summary Controls	|/api/v0/compliance/reporting/stats/trend	|"{start_time:""2022-07-21T00:00:00Z"",end_time: ""2022-07-31T23:59:59Z""}"	|241ms	|256ms	|2	|10			
Report Summary	|/api/v0/compliance/reporting/stats/summary	|"{start_time:""2022-07-21T00:00:00Z"",end_time: ""2022-07-31T23:59:59Z""}"	|245ms	|258ms	|2	|10			