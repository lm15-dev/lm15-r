test_that("registries resolve exact names, aliases and ambiguity", {
  a <- model_info("alpha", "openai", "openai_responses", aliases = list("alias"))
  registry <- new_model_registry(list(a))
  expect_identical(registry$get("openai", "alias"), a)
  expect_identical(registry$resolve("alias"), a)
  expect_error(registry$add(a, replace = FALSE), "already registered")
  registry$add(model_info("alpha", "anthropic", "anthropic_messages"))
  expect_null(registry$resolve("alpha"))
  expect_length(registry$list("openai"), 1L)
  expect_identical(registry$providers(), c("anthropic", "openai"))
  registry$add(model_info("alpha", "openai", "openai_responses"))
  expect_null(registry$get("openai", "alias"))
})

test_that("catalog loaders are sorted, atomic per catalog, and first-wins", {
  a <- model_info("alpha", "openai", "openai_responses")
  b <- model_info("alpha", "openai", "openai_responses", aliases = list("later"))
  expect_warning(registry <- discover_model_registry(catalogs = list(
    z = function() list(as_dict(b)),
    broken = function() list(as_dict(a), json_object(id = "bad")),
    a = function() list(as_dict(a)))), "could not be read")
  expect_identical(registry$list(), list(a))
  expect_null(registry$get("openai", "later"))
})

test_that("installed catalogs are data only, and registry changes reach routers", {
  library <- tempfile(); dir.create(file.path(library, "catalog", "lm15"), recursive = TRUE)
  on.exit(unlink(library, recursive = TRUE))
  a <- model_info("bespoke", "openai", "openai_responses")
  writeLines(as_json(json_array(as_dict(a))), file.path(library, "catalog", "lm15", "model-catalog.json"))
  registry <- discover_model_registry(libraries = library)
  router <- new_router(catalog = registry, env = character())
  expect_identical(resolve(router, "bespoke")$source, "catalog")
  registry$add(model_info("second", "gemini", "gemini_generate_content"))
  expect_identical(resolve(router, "second")$provider, "gemini")
})

test_that("unknown pricing dimensions do not invent token counts", {
  prices <- inference_pricing(input_per_million = 2, output_per_million = 5)
  expect_identical(estimate_cost(prices, usage(input_tokens = 1000000)), 2)
  expect_identical(estimate_cost(prices, usage()), 0)
  expect_identical(estimate_cost(prices, usage(input_tokens = 1000000, output_tokens = 2000000)), 12)
})
