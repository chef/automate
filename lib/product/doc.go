// Package product provides the description and parser for the top-level
// product.meta file and the package.meta files that can be provided for
// individual components.
//
// product.meta is a JSON file that lives in the root directory that
// describes the collections that make up automate. These collections
// can be base functionality, like postgres and elasticsearch, or
// things the user might want to deploy like workflow or chef-server.
// An example product.meta JSON blob:
//		{
//			"packages": [
//				"origin/pkg-core",
//				"origin/svc-core",
//				"origin/prod"
//			],
//			"collections": [
//				{
//					"name": "core",
//					"type": "base",
//					"services": [
//						"origin/svc-core"
//					],
//					"packages": [
//						"origin/pkg-core"
//					]
//				},
//				{
//					"name": "prod-1.0",
//					"aliases": ["prod"],
//					"type": "product",
//					"dependencies": ["core"],
//					"services": [
//						"origin/prod"
//					]
//				}
//			]
//		}
//
// Additional information about the pkg-core package could be provided in
// a package.meta file if needed. For example, we may want to describe its
// binlinks:
//		{
//			"name": "origin/pkg-core",
//			"binlinks": ["core-cli"]
//		}
// To see what metadata can be provided, see the PackageMetadata struct.
package product
