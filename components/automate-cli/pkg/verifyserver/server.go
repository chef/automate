package verifyserver

import (
	"fmt"
	"github.com/gofiber/fiber"
	"time"
)

func Start() {
	app := fiber.New()

	app.Get("/", func(c *fiber.Ctx) error {
		return c.SendString("Hello, World!")
	})

	log.Fatal(app.Listen(":3000"))
}
