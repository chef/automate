# Verify Server

## How to add a new fiber api

- Create a new file under `server/api/v1`
- Define a receiver function on `Handler` struct to create your fiber api
  Example: `func (h *Handler) NewApi(c *fiber.Ctx) {}`
- Create a new test file with test cases for the NewApi
- Create an entry in `server/routes.go` with the newly created function for your api

## Adding a new service for your api

- Create a file in `services` folder
- Add the service as dependency in `server/api/v1/handler.go`
- Add a receiver function on `Handler` struct to add the service

## Useful usage information

- Create all models in `models` folder so that those can be used across the pkg
- Use the same logger instance from the `Handler` struct across all apis
- Create mock for all services so that those can be used while testing
- For mocking http servers, use mock client instead of starting a mock server
- All file names and folder names should be small letters with no `_` or `-` etc. Test files should end with `_test.go`

## Start a dev server

- cd `components/automate-cli
- `make verify-serve`