|   Operation	|   Nodes	|   Concurrency	|   Is large	|   Exec Time	|  Errors  	|
|---	        |---	    |---	        |---	        |---	        |---	    |
|   Insert	    |  1000   	| 10   	        | false   	    |   237 sec	    |   0	    | 
|   Insert	    |  2000 	| 10  	        | false  	    |   463 sec	    |   0	    |
|   Insert	    |  5000 	| 10  	        | false 	    |   1159 sec	|   119 failed nodes|
|   Insert	    |  7000 	| 10  	        | false 	    |   1717 sec	|   Crashed because size was more than 2gb	    |
|   Insert	    |  10000	| 10 	        | false  	    |   2277 sec	|   Crashed because size was more than 2gb	|
