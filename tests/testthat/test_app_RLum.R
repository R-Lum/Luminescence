context("app_RLum")

expect_message(app_RLum(app = "abc"), regexp = "Invalid app name")