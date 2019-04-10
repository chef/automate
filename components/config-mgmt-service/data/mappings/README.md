# Elasticsearch FAQ

This document will have a list of things you can do with elasticsearch and the new data modeling.

We assume you have an elasticsearch instance up and running, you can verify this by running:
```
$ curl $ELASTICSEARCH_URL
{
  "name" : "GAnyF",
  "cluster_name" : "elasticsearch",
  "cluster_uuid" : "q2aayhdGSL6IuRYyguPNJw",
  "version" : {
    "number" : "5.5.0",
    "build_hash" : "260387d",
    "build_date" : "2017-06-30T23:16:05.735Z",
    "build_snapshot" : false,
    "lucene_version" : "6.6.0"
  },
  "tagline" : "You Know, for Search"
}
```

_**NOTE:** If you are inside the habitat studio you should have an environment variable called `ELASTICSEARCH_URL`
but if you are not running the commands there, you might have to set that up._
```
export ELASTICSEARCH_URL=http://elasticsearch:9200
```

## How to load mapping into an index

To create an index you have to create a `PUT` request.

To create the `node-state` index: 
```
$ curl -XPUT -H "Content-Type: application/json" --upload-file data/mappings/node-state.json $ELASTICSEARCH_URL/node-state
{"acknowledged":true,"shards_acknowledged":true}
```

To create the `converge-history` index:
```
$ curl -XPUT -H "Content-Type: application/json" --upload-file data/mappings/converge-history.json $ELASTICSEARCH_URL/converge-history
{"acknowledged":true,"shards_acknowledged":true}
```

## How to create a document

To create a document you have to create a `POST` request.

Using the two converge examples (success and failure) run the following requests:
```
$ curl -XPOST -H "Content-Type: application/json" --upload-file data/mappings/examples/success_converge.json  $ELASTICSEARCH_URL/node-state/node-state
$ curl -XPOST -H "Content-Type: application/json" --upload-file data/mappings/examples/failed_converge.json  $ELASTICSEARCH_URL/node-state/node-state
$ curl -XPOST -H "Content-Type: application/json" --upload-file data/mappings/examples/success_converge.json  $ELASTICSEARCH_URL/converge-history/converge
$ curl -XPOST -H "Content-Type: application/json" --upload-file data/mappings/examples/failed_converge.json  $ELASTICSEARCH_URL/converge-history/converge
```

## How to retrieve a document

To retrieve a document you have to do a normal `GET` request to the endpoint `/_index/_type/_id`

An example to retrieve a `node-state` document:
```
$ curl $ELASTICSEARCH_URL/node-state/node-state/AV1B0fOva3HqZwYcLP_N
```
